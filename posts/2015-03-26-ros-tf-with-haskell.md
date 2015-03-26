---
title: ROS TF with Haskell and RosHask
tags: Functional programming, Haskell, ROS
---
ROS is a set of software libraries and tools that help build robot
applications. This semester, my CSE 568 project is all about ROS. As a
functional programming enthusiast, I would like to step away from the
Python and C++ world ROS lies in.

[roshask](https://hackage.haskell.org/package/roshask) is a Haskell
binding to ROS. It provides the facility to create ROS nodes,
communicate with ROS and generate Haskell data types from ROS message
definitions. The generation of Haskell datatype is very appealing as
it provides type-level guarantee of the format of messages -- I just
can't express more about my frustrations with programming ROS in
Python and having to diagnose everything in runtime. So far so good,
only when it comes to working with TF.

<!--more-->
## Stream-based Approach
RosHask encourages a stream-based approach to messages. Instead of
adding callbacks to messages, blocking code with while loop, RosHask
views messages in topics as a stream. You can consume, filter, merge,
split, fold , map ... over the stream. Below is my code for the
controller:

~~~ {#mycode .haskell}
main :: IO ()
main = runNode "evader" $ do
          scans <- subscribe "robot_0/base_scan"
          odoms <- subscribe "robot_0/odom"
          advertise "robot_0/cmd_vel" $
             RT.mapM nav scans
          advertise "/tf" $
             RT.mapM (tfTransform "world" "evader") odoms
~~~

As you can see, I simply map the navigation commands to the messages
produced by laser scanner. It looks much cleaner.

## TF Installation Glitch
When you install TF, roshask will attempt to find
libconsole-bridge-dev. This package does not belong to ROS's message
bindings and roshask will complain about it. RosHask, by default,
ignores a series of packages that belongs to ROS. It has a hard-coded
"ignoredPackages" field in its source code:

~~~ {#mycode .haskell}
ignoredPackages = ["genmsg_cpp", "rospack", "rosconsole", "rosbagmigration",
                   "roscpp", "rospy", "roslisp", "roslib", "boost"]
~~~

The solution is to add libconsole-bridge-dev to this list and build
RosHask myself. Thanks to __rgleichman__ for helping me this. The
related thread is
[here](https://github.com/acowley/roshask/issues/31). But I do hope
that RosHask could have a manifest that allows users to specify
additional ignoredPackages.

## TF Messages
The TF messages generated from RosHask are really nice ones. Below is
a snippet of how I convert Odom message to TF message:

~~~{#mycode .haskell}
tfTransform :: String -> String -> O.Odometry -> IO TF.TFMessage
tfTransform frameId childId (O.Odometry {O.pose = p}) = do
  time <- RT.getROSTime
  let Pt.Point { Pt.x = x, Pt.y = y, Pt.z = z} = PC.position $ PC.pose p
      orientation = PC.orientation $ PC.pose p
      header = Std.Header { Std.seq      = 0
                          , Std.stamp    = time
                          , Std.frame_id = frameId
                          }
      transform    = TF.Transform (Vector3 x y z) orientation
      transStamped = TS.TransformStamped { TS.header         = header
                                         , TS.child_frame_id = childId
                                         , TS.transform      = transform }
  return $ TF.TFMessage [transStamped]
~~~

Compared with corresponding Python code, it actually makes much more
sense:

~~~ {#mycode .python}
br.sendTransform((odom.pose.pose.position.x,
                  odom.pose.pose.position.y,
                  0.0),
                 [pose.orientation.x, pose.orientation.y,
                  pose.orientation.z, pose.orientation.w],   # Orientation
                 rospy.Time.now(),
                 robot,
                 "world")
~~~

Note the line marked with "#Orientation". For whatever reason,
Python TF's sendTransform function does not accept quaternion as the
input, and we'll need to supply an iterable array for this
parameter. Internally though, Python TF makes some hard coding to
extract elements from the array and puts them back to a quaternion:

~~~ {#mycode .python}
     # In sendTransform in tf/broadcaster.py
        t.transform.rotation.x = rotation[0]
        t.transform.rotation.y = rotation[1]
        t.transform.rotation.z = rotation[2]
        t.transform.rotation.w = rotation[3]
~~~

One pitty for RosHask is that I wish those messages will support
[Lens](https://hackage.haskell.org/package/lens), so that accessing
nested structures could be made easier.

Until now, the broadcasting part is done. Simple, right?

## FFI to Rescue
Not until I started to work with the listener part did I realize the
potential challenges -- RosHask only provides message delivery and
receiving mechanism, but does not build additional facilities. Taking
TF as an example, TF bundles itself with transformation lookup layer,
message buffer, so that it automatically selects messages, traverses
the transformation tree and calculates transformations. I was left
with 2 options: write a TF close myself in Haskell or use FFI. I
picked the 2nd one.

ROS's packages are written in C++, and GHC currently does not have a
very good way to integrate with C++. There are many ways mentioned
[here](https://wiki.haskell.org/CPlusPlus_from_Haskell) but I'm
just wrapping C++ calls in functions with __extern "C"__ keyword.

Building C files along with Cabal is relatively easy. Just add the
source definitions in .cabal file:

~~~ {#mycode .cabal}
Extra-source-files: cbits, include
...
Executable evader
  Extra-lib-dirs:  /opt/ros/indigo/lib
  C-sources:       cbits/transform.cxx
  Include-dirs:    include
  Includes:        transform.h, types.h
  Extra-Libraries:
                   stdc++
                   roscpp
                   rostime
                   boost_system
                   rosconsole
                   tf2
                   tf2_ros
...
~~~

To build C++ code, add stdc++ to extra-libraries section. The FFI part
is relatively intuitive. I generated the access to C struct with
hsc2hs by writing a Foreign.Storable instance for the datatype.

~~~{#mycode .haskell}
data Trans = Trans { x :: Float
                   , y :: Float
                   } deriving (Show)

foreign import ccall unsafe "transform.h getTransform"
  getTransform :: CString -> CString -> Ptr Trans -> IO CInt

instance Storable Trans where
  sizeOf _    = ((8))
{-# LINE 22 "Transform.hsc" #-}
  alignment _ = (4)
{-# LINE 23 "Transform.hsc" #-}
  peek ptr = do
    x <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 25 "Transform.hsc" #-}
    y <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 26 "Transform.hsc" #-}
    return $ Trans x y
~~~

The listener is created in transform.cxx. I'll have to maintain the
state and reply a stream of transformations. The type for RosHask's
Topic gives a very good hint:

~~~{#mycode .haskell}
newtype Topic m a = Topic { runTopic :: m (a, Topic m a) }
~~~

Upon each run, it "pushes" the topic and emits a result. Intuitively, my
tfLookupTransform is implemented as follows:

~~~{#mycode .haskell}
tfLookupTransform :: String -> String -> Topic IO (Maybe (Float, Float))
tfLookupTransform src dest = do
  Topic $ do
    res <- currentTransform src dest
    return (res, tfLookupTransform src dest)
~~~

Now it' almost done.

## Concurrency Issue
I notices a few glitches in the actually running. The robots adjusted
their directions too fast and sometimes they even overshoot so much
ahead of the assigned angle. After looking into the implementation of
the subscribe function in roshask, I believe it's a race condition and
I need to connect my Topic to a channel via the share function:

~~~ {#mycode .haskell}
trans <- liftIO $ RTU.share (tfLookupTransform "pursuer" "evader")
advertise "robot_1/cmd_vel" $ RT.mapM pursue (RT.catMaybes trans)
~~~

Now it runs smoothly:

<span class="img-medium-post">
![Ros](/media/ros.png)
</span>
