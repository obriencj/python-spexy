#



progn = lambda *args: args[-1]

foo = lambda: 1

bar = lambda: 2

v = progn(foo(), bar())
print v



progn = lambda cont, *args: cont(args[-1])

foo = lambda cont: cont(1)

bar = lambda cont: cont(2)

v = foo(lambda a: bar(lambda b: progn(lambda x: x, None, a, b)))
print v





#(foo 1 2 3)
#((lambda (a) (foo a 2 3)) 1)
#(((lambda (a) (lambda (b) (foo a b 3))) 1) 2)
#((((lambda (a) (lambda (b) (lambda (c) (foo a b c)))) 1) 2) 3)
#(((((lambda (fn) (lambda (a) (lambda (b) (lambda (c) (fn a b c))))) foo) 1) 2) 3)



#(foo bar (baz quz))
baz(lambda a: foo(lambda x: (None,x), bar, a), quz)


#((foo bar) (baz quz))
#((lambda (a) (a (baz quz))) (foo bar))
#((lambda (a b) (a b)) (foo bar) (baz quz))
#(((lambda (a) (lambda (b) (a b))) (foo bar)) (baz quz))
foo(lambda a: baz(lambda b: a(lambda x: (None,x), b), quz), bar)

foo(bar)->a
baz(quz)->b
a(b)->x
terminator(x)



progn = lambda cont, *args: (cont, (args[-1],))

foo = lambda cont: (cont, (1,))

bar = lambda cont: (cont, (2,))

c,v = foo(lambda a: bar(lambda b: progn(lambda x: (None, x), a, b)))
while c:
    c,v = c(*v)
print v


foor = lambda cont,cc: (cont, (cc(4),))

callcc = lambda c,l: (c, l(c, c))

#callcc(lambda r: progn(foor(r), foo(), bar()))
#lambda cont,r: (cont, (progn(foor(r), foo(), bar())))
c,v = callcc(lambda x: (None, x), lambda cont,r: (cont, foor(lambda a: foo(lambda b: bar(lambda c: progn(r, a, b, c))), r)))
while c:
    c,v = c(*v)
print v



add = lambda a,b: a+b

make_adder = lambda x: lambda i: add(x,i)

v = make_adder(8)(5)
print v



add = lambda c,a,b: c(a+b)

make_adder = lambda c,x: c(lambda c,i: add(c,x,i))

v = make_adder(lambda a: a(lambda x:x, 5), 8)
print v



add = lambda c,a,b: (c, [(a+b),])

make_adder = lambda c,x: (c, (lambda c,i: add(c,x,i),))

c,v = make_adder(lambda a: a(lambda x:(None, x), 5), 8)
while c:
    c,v = c(*v)
print v


iter(lambda a: a[0](*a[1])



# The end.
