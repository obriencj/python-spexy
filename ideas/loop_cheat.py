
x = 0

def b():
    global x
    print "body: ", x
    x += 1
    return x


def c():
    global x
    print "cond: ", x
    return x < 10



my_for = lambda cond, body: \
             reduce((lambda a,b:b), \
                    iter((lambda: cond() and body() or False), False))


my_for(c, b)
