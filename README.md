racket-util
=========================

This is a library that I created In order to
enhance the racket langauge.
It contain a set of macros, some macros are use
to help keep the indentation on a sane level and
some to bring the good part of c to racket.

Here is a list of the macros for more detail
look at the library

enum enum-case enum-val - do c enum statment in racket.  
if* - is good for increasing the readability of the "if" statment.  
/= div rem inc inc! dec dec! push! pop! - are common lisp operators.  
$i - is useful when racket infix notation have too many parentesis
so we parse an expression similar to the c expression.  
$f - when you want to write a sequence of expressions.  
$m - is a way to emulate c style in a functional way.  
dbg - for debuging.  
when-let if-let - reduce indentation in a usefull patten.  
swap! - swap two variables.  
point - add break point.  
define+ - make a function breakable by "return".  
up down while do-while until do-until each repeat - looping macros.

License
-------
racket-util is released under the MIT license.
