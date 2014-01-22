"""
Some test calls to spexy.

TODO: make this use unittest

author: Christopher O'Brien  <siege@preoccupied.net>

"""


from sys import stdout
import spexy



doofoo = lambda x,y: x*y



def build_source(src):
    from cStringIO import StringIO

    print src
    
    ptre = spexy.build_tree(StringIO(src))
    ptre = ptre[0]
    print repr(ptre)
    
    peva = spexy.evaluate(spexy.ns_new(), ptre)
    print peva
    return peva



def eval_source(src):
    peva = build_source(src)
    ret = eval(peva, globals())
    print repr(ret)
    return ret



def test_source(src):
    try:
        return eval_source(src)
        
    except Exception, e:
        print e
        raise
    


# testing let and unwrapped calls
test_source(r"""
(let ((a 1)
      (b 2))
  (doofoo a b))
""")



# testing letrec
test_source(r"""
(letrec ((a 1)
         (b 5)
         (c (+ a b)))
  (doofoo a c))
""")



test_source(r"""
(when False (doofoo 1 2))
""")



test_source(r"""
(when True (doofoo 1 2))
""")



# testing lambda, operators, and unwrapped calls
test_source(r"""
(let ((my_add (lambda (x y) (+ x y)))
      (my_mult (lambda (x y) (* x y)))
      (a 5)
      (b 6)
      (my_print (lambda (s) (stdout.write s) (stdout.write "\n"))))

  (my_print (% "a = %s" a))
  (my_print (% "b = %s" b))
  (my_print (str (my_add a b)))
  (my_print (str (my_mult a b))))
""")



# testing setf
test_source(r"""
(let ((my_add (lambda (x y) (+ x y)))
      (my_mult (lambda (x y) (* x y)))
      (a 5)
      (b 11)
      (z 0)
      (my_print (lambda (s) (stdout.write s) (stdout.write "\n"))))

  (my_print (% "a = %s" a))
  (my_print (% "b = %s" b))
  (setf z (my_add a b))
  (my_print (str z))
  (setf z (my_mult a b))
  (my_print (str z)))
""")



# testing make-dict, make-list, getf and setf(getf)
test_source(r"""
(let ((m (make-dict (1 "foo") (2 "bar") (3 None) (4 (make-list 9 8 7 6))))
      (my_print (lambda (s) (stdout.write s) (stdout.write "\n"))))

  (my_print (% "m[1] = %r" (getf m 1)))
  (my_print (% "m[2] = %r" (getf m 2)))
  (my_print (% "m[3] = %r" (getf m 3)))
  (my_print (% "m[4] = %r" (getf m 4)))
  (my_print (% "m[4][0] = %r" (getf (getf m 4) 0)))
  (setf (getf m 3) (getf (getf m 4) 0))
  (my_print (% "new m[3] = %r" (getf m 3))))
""")



# testing let, lambda, and setf as a closure
loony = test_source(r"""
(let ((z 0))
  (lambda (x) (setf z (+ z x)) z))
""")

print "loony = %r" % loony
print "loony(1) =", loony(1)
print "loony(1) =", loony(1)
print "loony(500) =", loony(500)
print "loong(-20) =", loony(-20)



# testing member
test_source(r"""
(let ((x "Hello World"))
  x.split)
""")



# testing call member
test_source(r"""
(let ((x "Hello World"))
  (x.split))
""")



# testing while
test_source(r"""
(let ((x 0)
      (my_print (lambda (s)
                  (stdout.write (str s))
                  (stdout.write "\n") s)))

  (while (< x 5)
    (my_print x)
    (setf x (+ x 1))))
""")



# testing for-each
test_source(r"""
(let ((my_print (lambda (s) (stdout.write (str s)) (stdout.write "\n"))))
  (for-each (lambda (i) (my_print i)) (xrange 0 5)))
""")



# testing define
test_source(r"""
(let ((a 1)
      (b 2))

  (define c (+ a b))
  c)
""")



#testing define shadowing. Note that we cannot shadow the current
#frame's arguments, only earlier frames
test_source(r"""
(let ((x 100))
  (let ((a 1)
        (b 2))

    (define x (+ a b))
    x))
""")



# generate-while evaluates to a generator which will evaluate the body
# for each value while the condition still evaluates to True
genny = test_source(r"""
(let ((c 0))
  (generate-while (< c 5)
    (let ((x c))
      (setf c (+ c 1))
      x)))
""")
for i in genny:
    print "genny yielded", i



# the defclass form becomes (define name (class "name" ...))
test_source(r"""
(defclass Foo (object)
  ((x 50)
   (get_x (lambda (self) self.x))
   (set_x (lambda (self x) (setf self.x x)))))
""")

f = Foo()
print f
print f.get_x()
f.set_x(100)
print f.x



#
# The end.
