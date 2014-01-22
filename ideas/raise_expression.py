

fun_print = lambda s: \
            type(lambda:None)(compile("print s", "", "single"), locals())()


fun_raise = lambda ex: \
            type(lambda:None)(compile("raise ex", "", "single"), locals())()


fun_try_except = lambda to_try,exc_class,exc_handle: \
           type(lambda:None)(compile("try:\n\tto_try()\nexcept exc_class, exc:\n\texc_handle(exc)", "", "single"), locals())()


attempt = lambda: fun_raise(EOFError())

fixup = lambda e: fun_print(repr(e))


ret = fun_try_except(attempt, Exception, fixup)
print ret
