

"""
Some test calls to spexy.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from cStringIO import StringIO
from functools import partial
from itertools import count, imap
from sys import stdout
from unittest import TestCase

import spexy


def assoc(col, key, val):
    col[key] = val


def build_spexy(src_str):
    # generate a tree from src_str
    ptre = spexy.build_tree(StringIO(src_str))

    # convert first expr of tree into Python syntax
    return spexy.evaluate(spexy.ns_new(), ptre[0])


def eval_spexy(src_str, with_globals=None, with_locals=None):
    if with_globals is None:
        with_globals = dict(globals())

    if with_locals is None:
        with_locals = dict()

    # convert spexy src string into a Python src string
    peva = build_spexy(src_str)
    
    # compile it
    code = compile(peva, '<spexy>', 'eval')

    # evaluate the resulting Python code
    return eval(code, with_globals, with_locals)


def test_source(src_str):
    return eval_spexy(src_str)
    

def do_add(x, y):
    # a simple function to be used in a few spexy calls
    return x + y


class EvaluateTests(TestCase):

    def test_let_unwrapped(self):
        src = r""" (let ((a 1) (b 2)) (do_add a b)) """
        result = eval_spexy(src)

        self.assertEqual(result, 3)


    def test_letrec_unwrapped(self):
        src = r"""
        (letrec ((a 1)
                 (b 5)
                 (c (+ a b)))
            (do_add a c))
        """
        result = eval_spexy(src)
        assert(result == 7)


    def test_when_unwrapped(self):
        src = r"""
        (when False (do_add 1 2))
        """
        result = eval_spexy(src)
        assert(result == None)

        src = r"""
        (when True (do_add 1 2))
        """
        result = eval_spexy(src)
        assert(result == 3)


    def test_progn(self):

        # here we want to check that progn evaluates its expressions
        # in order, and that it evaluates to the value of the last
        # expression

        vals = [None] * 4
        X = partial(assoc, vals, 1)
        Y = partial(assoc, vals, 2)
        Z = partial(assoc, vals, 3)
        counter = count(100)

        src = r"""
        (progn
            (X (next counter))
            (Y (next counter))
            (Z (next counter))
            (next counter))
        """
        result = eval_spexy(src, with_locals=locals())

        assert(vals == [None, 100, 101, 102])
        assert(result == 103)
        

    def test_define(self):
        src = r"""
        (let ((a 1)
              (b 2))
            (define c (+ a b))
            c)
        """

        glbls = dict(globals())
        result = eval_spexy(src, with_globals=glbls)

        assert(result == 3)
        assert(glbls['c'] == 3)


    def test_member(self):
        src = r"""
        (let ((x "Hello World"))
            x.split)
        """
        result = eval_spexy(src)
        assert(type(result) == type("".split))

        src = r"""
        (let ((x "Hello World"))
            (x.split))
        """
        result = eval_spexy(src)
        assert(result == "Hello World".split())


    def test_define_shadowing(self):
        src = r"""
        (let ((x 100))
            (let ((a 1)
                  (b 2))
        
                (define x (+ a b))
                x))
        """
        result = eval_spexy(src)
        assert(result == 100)


    def test_defclass(self):
        src = r"""
        (defclass Foo (object)
            ((x 50)
             (get_x (lambda (self)
                        self.x))
             (set_x (lambda (self x)
                        (setf self.x x)))))
        """

        glbls = dict(globals())
        result = eval_spexy(src, with_globals=glbls)
        
        Foo = glbls['Foo']

        assert(result == None)
        assert(type(Foo) == type)

        f = Foo()
        assert(f.get_x() == 50)
        assert(f.set_x(100) == None)
        assert(f.get_x() == 100)
        assert(f.x == 100)

        
    def test_let_lambda_setf(self):
        src = r"""
        (let ((z 0))
            (lambda (x)
                (setf z (+ z x))
                z))
        """
        result = eval_spexy(src)

        assert(type(result) == type(lambda:None))
        assert(result(1) == 1)
        assert(result(1) == 2)
        assert(result(500) == 502)
        assert(result(-20) == 482)


    def test_make_list_dict_getf_setf(self):
        src = r"""
        (let ((m (make-dict (1 "foo")
                            (2 "bar")
                            (3 None)
                            (4 (make-list 9 8 7 6)))))

            (setf (getf m "message")
                  (make-list
                      (% "m[1] = %r" (getf m 1))
                      (% "m[2] = %r" (getf m 2))
                      (% "m[3] = %r" (getf m 3))
                      (% "m[4] = %r" (getf m 4))
                      (% "m[4][0] = %r" (getf (getf m 4) 0))))

            (setf (getf m 9) "tacos")
            (setf (getf m 3) (getf (getf m 4) 0))
            m)
        """

        result = eval_spexy(src)

        # from make-dict
        assert(result[1] == 'foo')
        assert(result[2] == 'bar')
        assert(result[3] == 9)
        assert(result[4] == [9, 8, 7, 6])
        assert(result[9] == 'tacos')

        assert(result['message'] == [
            "m[1] = 'foo'",
            "m[2] = 'bar'",
            "m[3] = None",
            "m[4] = [9, 8, 7, 6]",
            "m[4][0] = 9", ])
        

    def test_lambda_operators_unwrapped(self):
        src = r"""
        (let ((my_add (lambda (x y) (+ x y)))
              (my_mult (lambda (x y) (* x y)))
              (a 5)
              (b 6))
        
            (make-list
                (% "a = %s" a)
                (% "b = %s" b)
                (my_add a b)
                (my_mult a b)
                my_add
                my_mult))
        """
        result = eval_spexy(src)

        assert(result[0] == "a = 5")
        assert(result[1] == "b = 6")
        assert(result[2] == 11)
        assert(result[3] == 30)
        assert(result[4](7, 11) == 18)
        assert(result[5](7, 11) == 77)


    def test_while(self):
        counter = count(0, 5)

        src = r"""
        (let ((x 0)
              (bump (partial next (count 0 5))))

            (while (< x 5)
                (setf x (+ x 1))
                (make-list x (bump))))
        """

        result = eval_spexy(src)

        assert(result[0] == 5)
        assert(result[1] == 20)


    def test_for_each(self):
        src = r"""
        (letrec ((out (StringIO))
                 (my_print (lambda (s)
                             (out.write (str s))
                             (out.write "\n"))))
            (for-each
                (lambda (i) (my_print i))
                (xrange 0 5))
            (out.getvalue))
        """

        result = eval_spexy(src)
        exp = "\n".join(imap(str, xrange(0, 5))) + "\n"

        self.assertEqual(result, exp)


    def test_generate_while(self):
        # generate-while evaluates to a generator which will evaluate
        # the body for each value while the condition still evaluates
        # to True
        genny = test_source(r"""
        (let ((c 0))
            (generate-while (< c 5)
                (let ((x c))
                    (setf c (+ c 1))
                    x)))
        """)

        g = tuple(genny)
        self.assertEqual(g, tuple(xrange(0, 5)))


#
# The end.
