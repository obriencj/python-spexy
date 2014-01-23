
# Overview of python-spexy

A [Python] module for converting statements written in a LISP-like
dialect directly into Python code.

This is not a [Scheme] or [LISP] interpreter. At best it's an
s-expr-like pre-processor to Python. At worst it's a
[ridiculous hack].

[python]: https://python.org/ "Python"

[scheme]: http://en.wikipedia.org/wiki/Scheme_(programming_language)
"Wikipedia -- Scheme (programming language)"

[lisp]: http://en.wikipedia.org/wiki/Lisp_programming_language
"Wikipedia -- LISP (programming language)"

[s-expr]: http://en.wikipedia.org/wiki/S-expression
"Wikipedia -- S-expression"

[ridiculous hack]: https://gist.github.com/obriencj/8567348
"Gist -- Spexy repl example"


## Background

I [re-discovered] this project in January of 2014 while cleaning out
~/devel on one of my machines. It was originally written in 2007,
during what I would refer to as my [Ambien]-era.

[re-discovered]: http://obriencj.tumblr.com/post/74175397785/spexy-oh-god-what-did-i-do
"Spexy? Oh god what did I do..."

[ambien]: http://en.wikipedia.org/wiki/Ambien#Adverse_effects
"Anterograde amnesia, Altered thought patterns..."


## Concept

As I have been reading through all this lost code and trying to get my
mind wrapped around the concepts that underly it, I've discovered that
I had set some rules for myself in this game of pre-processing.

No non-user imports
: The generated Python code is only allowed to import what the user
explicitly asks it to import. It may not inject other imports (which
means no functools or itertools).
   
No additional dependencies
: As an extension to the above, the generated Python code cannot
require Spexy as a module to function at runtime.
	
No non-user defines
: Don't inject methods or variables into the output other than those
that the user asks for.

The above makes this whole project significantly more interesting, and
at the same time significantly less useful (as if that were possible)
for other users. It's more like a puzzle and less like an actual
project. Yay!

Here is the original README from 2007:

> The idea here is to use Python's pluggable encoding support to
create a pre-processor that will take a file of code in SEXP and
generate something Python can actually use.

> Effectively, you'll be writing in almost-lisp, but keeping the full
functionality of Python. You'll be able to export the module you've
written, and you'll be able to import any other Python module.

> I got the idea from someone who wrote a [curly-braces encoding] for
Python.

> Run `test.py` to see some "interesting" output.

> Want to be able to switch between Python and Spexy-lisp at-will?
Just use `spexy.repl(sys.stdin)` from within your interactive Python
session, and there you go! (quit) when you want to back out of the
Spexy REPL, and you'll be back in Python, with all of your Spexy
defines still available. Switch back and forth as often as you like.

> In Python, some things are expressions and some things are
statements.  In Spexy, everything is an expression. There are a few
things which I could not figure out ways to "express," such as
try/except/finally, raise, yield, return, break, and print. However, a
number of things which would normally be written as Python statements
can instead be written as expressions, such as if/elif/else,
multi-line code blocks (via progn), class definitions, for/while, and
variable assignment.

[curly-braces encoding]: http://timhatch.com/projects/pybraces/
"pybraces"


## Example Transformations

The following are examples (from the original 2007 spexy.text) that
show Spexy inputs and their resulting Python outputs.

Note that in many of these cases the actual implementation differs
from the example. The `define` and `defclass` forms especially emit
wildly different code. I'll update them later, for now I'm just trying
to merge the scattered examples and documentation into one place.

Also note that as Python has evolved, better variants are now
available. In particular the spexy `if` could be a Python
[conditional expression].

[conditional expression]: http://docs.python.org/2.5/whatsnew/pep-308.html
"What's new in Python 2.5 -- PEP 308: Conditional Expressions"


### `let`
```lisp
(let ((x 1)
      (y 2)
      (z (something 3)))

  (activity x y z))
```
```python
(lambda x, y, z: \
  (activity(x[0], y[0], z[0]),)[-1])([1],[2],[something(3)])
```

### `let` with `setf`
```lisp
(let ((x 1)
      (y 2)
      (z None))

  (setf z (something (+ x y)))
  (activity x y z))
```
```python
(lambda x, y, z: \
  (z.__setitem__(0, something(x[0] + y[0])),
   activity(x[0], y[0], z[0]))[-1])([1], [2], [None])
```

### `if` as an expression
```lisp
(if foo 1 2)
```
```python
((lambda: 1),(lambda: 2))[not (foo)]()
```

### `define`
 *see note above regarding innaccuracies in examples*
```lisp
(define make_adder
  (lambda (by)
    (lambda (x) (+ x by))))
(define add_8
  (make_adder 8))
```
```python
make_adder = lambda by: lambda x: x + by
add_8 = make_adder(8)
```

### `defclass`
 *see note above regarding innaccuracies in examples*
```lisp
(defclass Foo (object)
       (define x 1)
       (defun __init__ (self x)
	 (setf self 'x x))))
```
```python
class Foo(object):
    x = 1
    def __init__(self, x):
        self.setattr("x", x)
```


## Requirements

* Python 2.5 or later (no support for Python 3 -- but who cares,
  nobody should actually use this anyway)


## Lost in time

The original spexy was posted into a CVS repository on a host which
has long since been replaced, replaced again, and replaced again
again. I wasn't able to dig up that original cvsroot. All I have is
the checkout of what I presume to be the most up-to-date code.

If you need a material lesson on one of the many values of a
distributed revision control system, let this serve as such. I most
likely have lost any and all history (commit messages, diffs,
timestamps, etc) for this project. Had I been using a DRCS, then I'd
still have everything.

Luckily, the utility of this particular project is so negligible that
I can safely say nothing of great value was lost.


## Contact

author: Christopher O'Brien <obriencj@gmail.com>


## License

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 3 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, see
<http://www.gnu.org/licenses/>.
