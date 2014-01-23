The idea here is to use Python's pluggable encoding support to create
a pre-processor that will take a file of code in SEXP and generate
something Python can actually use.

Effectively, you'll be writing in almost-lisp, but keeping the full
functionality of Python. You'll be able to export the module you've
written, and you'll be able to import any other Python module.


I got the idea from someone who wrote a curly-braces encoding for
Python.


Run test.py to see some "interesting" output.

Want to be able to switch between Python and Spexy-lisp at-will? Just
use `spexy.repl(sys.stdin)` from within your interactive Python
session, and there you go! (quit) when you want to back out of the
Spexy REPL, and you'll be back in Python, with all of your Spexy
defines still available. Switch back and forth as often as you like.


In Python, some things are expressions and some things are statements.
In Spexy, everything is an expression. There are a few things which I
could not figure out ways to "express," such as try/except/finally,
raise, yield, return, break, and print. However, a number of things
which would normally be written as Python statements can instead be
written as expressions, such as if/elif/else, multi-line code blocks
(via progn), class definitions, for/while, and variable assignment.


- siege
