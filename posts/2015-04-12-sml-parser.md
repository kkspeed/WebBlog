---
title: Simple LL Parser with SML
tags: Functional programming
---

This post is about writing a parser in Standard ML. I try to fit the
code in the idiom of Haskell. It may not pragmatic SML code.

<!--more-->

We will use the parser to parse
[Lambda Calculus](http://en.wikipedia.org/wiki/Lambda_calculus). The
language definition is given in the following abstract data type and
To make it prettier, we'll write a toString function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
(* Lambda calculus definitions *)
datatype Expression = Variable of { name : string }
                    | Abstraction of { argument : string,
                                       body : Expression }
                    | Application of { function : Expression,
                                       argument : Expression }

fun toString (Variable { name })  = "Var(" ^ name ^ ")"
  | toString (Abstraction { argument, body }) =
    String.concat [ "Abs(", argument, ".", toString body, ")" ]
  | toString (Application { function, argument }) =
    String.concat ["App(", toString function, " ", toString argument, ")" ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Our parser is defined as a datatype that takes in a list of chars
(stream) and emits a generic type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
(* Parser datatype *)
datatype 'b Parser = Parser of { runParser: char list -> (char list * 'b) option }

fun parse (Parser {runParser}, stream) = runParser stream
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The parser fits naturally in a monad. The bind operator is given by a
function that processes the output of a parser and generates a new
parser.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
(* Monad interface *)
infixr 5 >>=
fun p >>= f =
    let
        fun doParse (stream) =
            case parse (p, stream) of
                SOME (tls, r) => parse (f r, tls)
              | NONE => NONE
    in
        Parser { runParser = doParse }
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The sequential operator is defined as ignoring the output of previous
parser (but preserves the parsing state). The return simply wraps a
constant value in a parser. The fail operator raises error. Since in
our parser, error is defined simply as returning NONE, we'll do it
rightaway. More sophisticated parsers will use Either data type to
handle parsing errors (like
[Parsec](https://hackage.haskell.org/package/parsec)).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
infixr 5 >>
fun p1 >> p2 = p1 >>= (fn _ => p2)

fun return (v) = Parser {
        runParser = fn (stream) => SOME (stream, v)
    }

val fail = Parser { runParser = fn _ => NONE }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We declare a parser as a functor (the "functor" is defined in the realm
of Haskell, not SML), so that we can send pure functions into the
parser and updates its value.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
(* Functor *)
infix 3 <$>
fun f <$> p = p >>= (fn x => return (f x))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We give it alternative interface. When one parser fails, it tries the
next one.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
infix 3 <|>
fun p1 <|> p2  = Parser {
      runParser = fn stream =>
                     case parse (p1, stream) of
                         SOME r => SOME r
                       | NONE => parse (p2, stream)
  }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We define some simple parser combinators. "many" will try the parser
as long as it succeeds and the results will return a list of
results. "satisfy" tests if the first character satisfies a
predicate. If yes, it succeeds. "between" applies a parser many times
until it fails, then it tries the second parser and returns its
result. Then it apply the first parser to elminate trailing
characters. "many1" is like many, but it requires the parser to
succeed at least once.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
(* Generic parser combinators *)
fun many (p) =
    let
        fun doParse (st) = case parse (p, st) of
                               NONE => (st, [])
                            |  SOME (tls, a) =>
                               let
                                   val (rst, ss) = doParse tls
                               in
                                   (rst, a :: ss)
                               end
    in
        Parser { runParser = fn stream => SOME (doParse stream) }
    end

fun satisfy (pred) = Parser {
        runParser = fn (stream) =>
                       case stream of
                          a::tls => if (pred a)
                                    then SOME (tls, a)
                                    else NONE
                        | _  => NONE
    }

fun between (pb, p) = Parser {
        runParser = fn (stream) =>
                       case parse (pb >> p, stream) of
                           SOME (tls, r) => parse (pb >> return r, tls)
                         | NONE =>  NONE
    }

fun many1 (p) = p >>= (fn x =>
                many p >>= (fn res =>
                return (x :: res)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The next combinator is called "delay". It freezes a parser and makes
it a non-strict one. This is useful in defining mutually recursive
parsers (try to remove the "delay" in later definitions of lambda
calulus parser to see how it works :)).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
fun delay (f) = Parser {
      runParser = fn (stream) => parse (f (), stream)
  }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The next parsers are for char stream manipulations.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
(* Char stream parser *)
fun chr (c: char): char Parser =
  let
     fun p (stream) =
         case stream of
             (a :: tls) => if a = c
                           then SOME (tls, a)
                           else NONE
           | _ => NONE
  in
      Parser { runParser = p }
  end

val oneOf = foldr (op <|>) fail o map chr

val space = chr #" "
val spaces = many space
val token = between (spaces, many1 (satisfy Char.isAlpha))

fun charlist (a :: t) =
    chr a >>= (fn c => charlist t >>= (fn ts => return (c::ts)))
  | charlist [] = return []

fun string str = charlist (String.explode str)

fun bracket (bo, bc, p) = bo >> p >>= (fn r =>
                          bc >> return r)

fun braces p = bracket (spaces >> chr #"(", spaces >> chr #")", p)

fun tok s = spaces >>
            string s >>= (fn st =>
            spaces >>
            return (String.implode st))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now it comes to Lambda Calculus parser. The grammar for lambda
calculus looks like follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .bash}
expr  ::= abs | app | paren | var
abs   ::= "fn" args . expr
app   ::= (paren | var) expr
paren ::= (expr)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To make it mutually recursive, we'll use the fun ... and ... construct
in SML and use "delay" to suspend evaluations (otherwise we'll have
infinite recursion).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
(* Lambda calculus parser *)
fun abs () = tok "fn" >>
             token >>= (fn a =>
             tok "." >>
             expr () >>= (fn e =>
             return (Abstraction {
                          argument = String.implode a
                        , body = e })))

and arg () = var ()

and var () = token >>= (fn v =>
             return (Variable { name = String.implode v }))

and expr () = delay abs <|> delay app <|> delay paren <|> var ()

and app () = (delay paren <|> var ()) >>= (fn e1 =>
             delay expr >>= (fn e2 =>
             return (Application { function = e1
                                 , argument = e2 })))

and paren () = tok "(" >> delay expr >>= (fn e =>
               tok ")" >> return e)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's write a test function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {#mycode .sml}
(* Testing *)
fun lambdaCalc s = case parse (expr (), String.explode s) of
                       SOME (_, r) => toString r
                     | NONE => "FAIL"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here are the results:

~~~~~~~~~~~~~~~ {#mycode .sml}
lambdaCalc "(fn x . x x) (fn x . x x x)";
val it =
  "App(Abs(x.App(Var(x) Var(x))) Abs(x.App(Var(x) App(Var(x) Var(x)))))"
  : string
~~~~~~~~~~~~~~~

Looks good :).