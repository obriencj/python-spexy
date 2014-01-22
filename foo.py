
progn = lambda cont, *args: cont(args[-1])

foo = lambda cont: cont(1)

bar = lambda cont: cont(2)

