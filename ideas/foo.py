
progn = lambda cont, *args: cont(args[-1])

foo = lambda cont: cont(1)

bar = lambda cont: cont(2)


def __getattribute__(name):
	print name
	return 100

x = 5
y = 50

