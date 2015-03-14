---
title: Better MLton Hacking Experience with Emacs
tags: MLton, Emacs, Functional programming
---

I'm doing a project to modify MLton's garbage collector. But working
with MLton's source code is a bit painful. The only way to do it was
to use the good old "grep" command. Since I'm not quite familiar with
Standard ML, I want more hints / error checks while I'm editing the
code.

<!--more-->

After a painful 2 weeks of navigating the code like walking in the
dark maze, I noticed that MLton actually ships with some very handy
Emacs minor modes for reading code, noticeably, **bg-build-mode** and
**def-use-mode**. They are included in the **ide/emacs** folder in
MLton's source directory.

The installation documentation for those 2 modes are at:

- [Emacs bg-build-mode](http://mlton.org/EmacsBgBuildMode)
- [Emacs def-use-mode](http://mlton.org/EmacsDefUseMode)

A tiny glitch with bg-build-mode is that it uses old
*compilation-compat-parse-errors* routine, which is not included in
newer version of Emacs (I'm using Emacs 24). It can be easily diabled
with the following modification:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{#mycode .lisp}
(defun bg-build-parse-messages ()
  (let ((original-display-message
         (when (fboundp 'display-message)
           (symbol-function 'display-message))))
    (when (fboundp 'display-message)
      (fset 'display-message
            (function
             (lambda (label &rest args)
               (unless (eq label 'progress)
                 (apply original-display-message label args))))))
    (unwind-protect
       ; (compat-compilation-parse-errors) ; simply comment this line out
      (when (fboundp 'display-message)
        (fset 'display-message original-display-message)))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With these two modes enabled, now I can browse the code more
efficiently:

![MLton DefUse](/media/mlton-def-use.png)
