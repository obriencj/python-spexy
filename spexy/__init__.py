# This library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, see
# <http://www.gnu.org/licenses/>.


"""
A hack desguised as an encoding module that lets you write
SEXP-styled Python, with the added bonus of let, multi-expression
lambda, and cond

TODO: implement the Python encoding API

TODO: cons, car, cdr, and ways to convert between a python list or
tuple and back

TODO: default arguments to lambda and defun

TODO: variadic arguments to lambda and defun

TODO: defmacro (hahahahahahahahahahaha)

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from cStringIO import StringIO
from functools import partial
from itertools import chain, count, imap, izip, repeat
import sys


class SpexyException(Exception):
    """ an Exception raised by spexy """
    pass


class SpexyFormException(SpexyException):
    """ non-well-formed spexy expression detected """
    pass


class SpexyEvaluateException(SpexyException):
    """ problem during evaluation """
    def __str__(e):
        return "%s. Expression tree was %r" % e.args


#
# tree parsing. I borrowed this from previous work I did with a simple
# del.icio.us post filter module called deli


def next_fd_gen(fd):
    return iter(partial(fd.read, 1), "")


# I should make this work with tripple-quoted strings, too.
def build_quoted(fd, quotec='\"'):
    token = StringIO()
    esc = False

    token.write(quotec)
    for c in next_fd_gen(fd):
        if not esc and c == quotec:
            break
        else:
            token.write(c)
            esc = (c == '\\') and (not esc)

    token.write(quotec)
    return token.getvalue()


def build_tree(fd):

    """ returns a list of expressions parsed from fd """

    return list(gen_exprs(fd))


def gen_exprs(fd):

    token = None

    for c in next_fd_gen(fd):
        if c in ';#\"\'() \n\r\t':
            if token:
                yield token.getvalue()
                token = None

        else:
            if not token:
                token = StringIO()
            token.write(c)
            continue

        if c in ';#':
            # comments run to end of line
            for c in next_fd_gen(fd):
                if c in "\n\r":
                    break

        elif c == '(':
            yield build_tree(fd)

        elif c == ')':
            return

        elif c in '\'\"':
            yield build_quoted(fd, c)

    if token:
        yield token.getvalue()


#
# evaluating the parsed tree into something useful


# I'm unhappy that I need to use this at all, and I will try to
# minimize its usage as much as possible
def gensym(counter=count()):
    return "_spexy_gensym_%08x" % next(counter)


def comma(items):
    return ", ".join(map(str, items))


def seq(*items):
    # lets us do seq(a, b, *more) and get a sensible value
    return items


#
# macros, or Spexy forms that evaluate into other Spexy forms


def macro_class(name, inheritl, members):
    mems = ((repr(k),v) for k,v in members)
    return ("type", name,
            seq("make-tuple", *inheritl),
            seq("make-dict", *mems))


def macro_defclass(var, inheritl, members):
    return ("define", var, ("class", repr(var), inheritl, members))


def macro_defun(var, args, *body):
    return ("define", var, seq("lambda", args, *body))


def macro_eq(a, b):
    return ("==", a, b)


def macro_for_each(do_fun, in_seq):
    return ("let", (("fn", do_fun),
                    ("seq", in_seq)),
            ("reduce", ("lambda", ("a", "b"), ("fn", "b")), "seq", None))


def macro_generate_while(cond, *body):
    tmp = gensym()
    return ("letrec", ((tmp, ("lambda", (),
                              ("if", cond, seq("progn", *body), tmp))),),
            ("iter", tmp, tmp))


def macro_import(mod, *more):
    return ("map",
            ("lambda", ("x"), ((".", ("globals",), "__setitem__"),
                               "x", ("__import__", "x"))),
            seq("make-tuple", repr(mod), *(map(repr, more))))


def macro_import_from(mod, mem, *more):
    return ("letrec", (("members",
                        seq("make-tuple", repr(mem), *(map(repr, more)))),
                       ("mod",
                        ("__import__", repr(mod),
                         ("globals",), ("locals",), "members"))),
            ("for-each",
             ("lambda", ("x",), ((".", ("globals",), "__setitem__"),
                                "x", ("mod.__getitem__", "x"))),
             "members"))


def macro_letrec(vars, *body):
    empties = [(k,None) for k,v in vars]
    setup = (seq("setf", k, v) for k,v in vars)
    return ("let", empties,
            (seq("progn", *setup) + body))


def macro_member_call(var, mem, *args):
    return seq((".", mem), *args)


def macro_when(cond, *body):
    return ("if", cond, seq("progn", *body))


def macro_while(cond, *body):
    #a, b = gensym(), gensym()
    return ("reduce",
            ("lambda", ("_a", "_b"), "_b"),
            seq("generate-while", cond, *body),
            False)


def macro_try(progn, *rest):
    hndls = list()

    for r in rest:
        k = r[0]
        c = r[1:]
        if isinstance(k, (list, tuple)):
            h = seq("lambda", ("exc_type","exc_val","exc_tb"), *c)
        elif k in (":else",":finally"):
            h = seq("lambda", tuple(), *c)
        else:
            raise SpexyFormException("malformed try")
        hndls.append((k, h))

    return seq("trycall", seq("lambda", tuple(), progn), *hndls)


macros = {
    "!": macro_member_call,
    "class": macro_class,
    "defclass": macro_defclass,
    "defun": macro_defun,
    "eq": macro_eq,
    "for-each": macro_for_each,
    "generate-while": macro_generate_while,
    "import": macro_import,
    "import-from": macro_import_from,
    "letrec": macro_letrec,
    "when": macro_when,
    "while": macro_while,
    "try": macro_try,
    }


def macro_eval(n, *args):
    return macros.get(n)(*args)


# convert specials into their underlying operator
def special_op(ns, op, *items):
    jop = " %s " % op
    return "(" + jop.join(evaluate(ns, i) for i in items) + ")"


def special_define(ns, tok, var, expr):
    return set_global(var, evaluate(ns, expr))


def special_generator(ns, tok, func, seq, filter=None):
    junk = gensym()

    fe = evaluate(ns, func)
    se = evaluate(ns, seq)

    if filter:
        return "(%s(%s) for %s in %s if %s(%s))" % \
               (fe, junk, junk, se, evaluate(ns, filter), junk)
    else:
        return "(%s(%s) for %s in %s)" % \
               (fe, junk, junk, se)


def special_get(ns, tok, sym, *ex):
    if ex:
        return "%s.__getitem__(%s)" % \
               (evaluate(ns, sym), evaluate(ns, ex[0]))
    else:
        return ns_getter(ns, sym)(lhs)


# we don't compose the normal python if block because we want to be
# able to embed this within a single expression. I'm not using the new
# Python conditionals because I want to use this on an earlier version
# of Python.
def special_if(ns, tok, cond, a, *b):
    if not b:
        b = "None"
    elif len(b) == 1:
        b = b[0]
    else:
        b = (tok,)+b

    return "(((lambda: %s),(lambda: %s))[not %s]())" % \
           (evaluate(ns, a), evaluate(ns, b), evaluate(ns, cond))


def special_lambda(ns, tok, params, *body):
    return "(lambda %s: %s)" % \
           (comma(params), special_progn(ns, None, *body))


def special_let(ns, tok, vars, *body):
    nns = ns_shadow(ns)
    for v in vars:
        ns_add(nns, v[0], get_closure, set_closure)

    na = [v[0] for v in vars]
    nb = [("[%s]" % evaluate(ns, v[1])) for v in vars]

    return "((lambda %s: %s)(%s))" % \
           (comma(na), special_progn(nns, None, *body), comma(nb))


def special_lc(ns, tok, func, seq, filter=None):
    junk = gensym()

    fe = evaluate(ns, func)
    se = evaluate(ns, seq)

    if filter:
        return "([%s(%s) for %s in %s if %s(%s)])" % \
               (fe, junk, junk, se, evaluate(ns, filter), junk)
    else:
        return "([%s(%s) for %s in %s])" % \
               (fe, junk, junk, se)


def special_make_dict(ns, tok, *pairs):
    if pairs:
        p = [(evaluate(ns, k)+":"+evaluate(ns, v)) for k,v in pairs]
        return "{" + comma(p) + ",}"
    else:
        return "dict()"


def special_make_list(ns, tok, *items):
    if items:
        p = [evaluate(ns, i) for i in items]
        return "[" + comma(p) + ",]"
    else:
        return "list()"


def special_make_tuple(ns, tok, *items):
    if items:
        p = [evaluate(ns, i) for i in items]
        return "(" + comma(p) + ",)"
    else:
        return "tuple()"


def special_member(ns, tok, var, mem):
    return "%s.%s" % (evaluate(ns, var), mem)


def special_not(ns, tok, item):
    return "(not %s)" % evaluate(ns, item)


#def macro_print(*items):
#    g1, g2 = gensym(), gensym()
#    return seq("let", ((g1, (".", ("__import__", '"sys"'), "stdout")),
#                       (g2, (".", g1, "write"))),
#               *map(lambda i:  items))


#def special_print(ns, tok, item):
#
#    #
#    #pr_ln = (lambda _a: type(lambda:None) \
#    #         (compile('print _a', '', 'single'),
#    #          locals())())
#
#    a = gensym()
#    ln = (tok == "println")
#
#    funt = "type(lambda:None)"
#    comp = "compile('print %s%s', '', 'single')" % \
#           (a, (",","")[ln])
#
#    return "(lambda %s: %s(%s, locals())())(%s)" % \
#           (a, funt, comp, evaluate(ns, item))


#def special_print_to(ns, tok, stream, item):
#    a, b = gensym(), gensym()
#    ln = (tok == "println-to")
#
#    funt = "type(lambda:None)"
#    comp = "compile('print >> %s, %s%s', '', 'single')" % \
#           (a, b, (",","")[ln])
#
#    return "(lambda %s,%s: %s(%s, locals())())(%s, %s)" % \
#           (a, b, funt, comp, evaluate(ns, stream), evaluate(ns, item))


def special_print(ns, tok, *items):
    its = (evaluate(ns, i) for i in items)
    itsa = list(chain(*izip(its, repeat('" "'))))

    if tok == "println":
        itsa[-1] = r"'\n'"
    else:
        itsa.pop()

    return "__import__('sys').stdout.writelines(map(str, (%s)))" % \
        comma(itsa)


def special_print_to(ns, tok, stream, *items):
    its = (evaluate(ns, i) for i in items)
    itsa = list(chain(*izip(its, repeat('" "'))))

    if tok == "println":
        itsa[-1] = r"'\n'"
    else:
        itsa.pop()

    return "%s.writelines(map(str, (%s)))" % \
        (evaluate(ns, stream), comma(itsa))


def special_progn(ns, tok, *body):
    if not body:
        return None

    elif len(body) == 1:
        return evaluate(ns, body[0])

    else:
        bd = [evaluate(ns, b) for b in body]
        return "(%s,)[-1]" % (comma(bd))


def special_set(ns, tok, lhs, rhs):
    if isinstance(lhs, list):
        if lhs[0] == "getf":
            return "%s.__setitem__(%s, %s)" % \
                   (evaluate(ns, lhs[1]), (evaluate(ns, lhs[2])),
                    (evaluate(ns, rhs)))

        else:
            raise SpexyFormException("malformed setf")

    else:
        if "." in lhs:
            dots = lhs.split(".")

            leftmost = dots[0]
            rightmost = dots[-1]
            middle = dots[1:-1]

            if ns_sym(ns, leftmost):
                getter = ns_getter(ns, leftmost)
                leftmost = getter(ns, leftmost)

            if middle:
                leftmost = leftmost + "." + ".".join(middle)

            return "setattr(%s,%r,%s)" % \
                   (leftmost, rightmost, evaluate(ns, rhs))

        else:
            setter = ns_setter(ns, lhs)
            return setter(lhs, evaluate(ns, rhs))


def gen_try(exc, els=None, fin=None):
    ret = list()

    ret.append("def try_block(try_clause")
    for i,e in enumerate(exc):
        ret.append(",except_%i" % i)
    if els:
        ret.append(",else_clause")
    if fin:
        ret.append(",finally_clause")
    ret.append("):\n")

    ret.append("\tfrom sys import exc_info\n")

    ret.append("\ttry:\n\t\t")
    if not els:
        ret.append("return ")
    ret.append("try_clause()\n")
    for i,e in enumerate(exc):
        ts = e[0]
        if ts:
            ret.append("\texcept (")
            ret.append(",".join(map(str,e[0])))
            ret.append(",):\n")
        else:
            ret.append("\texcept:\n")
        ret.append("\t\treturn except_%i(*exc_info())\n" % i)

    if els:
        ret.append("\telse:\n\t\treturn else_clause()\n")
    if fin:
        ret.append("\tfinally:\n\t\tfinally_clause()\n")

    return "".join(ret)


def special_trycall(ns, tok, prog, *rest):
    errs = list()
    els = None
    fin = None

    for k,c in rest:
        if isinstance(k, (list,tuple)):
            errs.append(([evaluate(ns, e) for e in k], evaluate(ns, c)))
        elif k == ":else":
            els = evaluate(ns, c)
        elif k == ":finally":
            fin = evaluate(ns, c)

    trycode = gen_try(errs, els, fin)
    hndls = [e[1] for e in errs]
    if els:
        hndls.append(els)
    if fin:
        hndls.append(fin)

    return "(lambda _lcls:(eval(compile(%r,'<spexy>','single')," \
        "globals(),_lcls), _lcls)[-1])({})['try_block'](%s,%s)" % \
        (trycode, evaluate(ns, prog), comma(hndls))


specials = {
    "+": special_op,
    "*": special_op,
    "**": special_op,
    "-": special_op,
    "/": special_op,
    "%": special_op,
    "&": special_op,
    "|": special_op,
    "^": special_op,
    "<<": special_op,
    ">>": special_op,
    "<": special_op,
    "<=": special_op,
    ">": special_op,
    ">=": special_op,
    "==": special_op,
    "and": special_op,
    "in": special_op,
    "is": special_op,
    "or": special_op,

    ".": special_member,
    #"cond": special_cond,
    "define": special_define,
    "generator": special_generator,
    "gen": special_generator,
    "getf": special_get,
    "if": special_if,
    "lambda": special_lambda,
    "let": special_let,
    "list-comprehension": special_lc,
    "l-c": special_lc,
    "make-dict": special_make_dict,
    "make-list": special_make_list,
    "make-tuple": special_make_tuple,
    "not": special_not,
    "print": special_print,
    "println": special_print,
    "print-to": special_print_to,
    "println-to": special_print_to,
    "progn": special_progn,
    "setf": special_set,
    "trycall": special_trycall,
    }


#
# These are special functions which may be passed around by their name
# to other Python functions. This is done by creating an anonymous
# lambda that behaves similarly to the code generated by the special
# form. This can only be done with special forms which would evaluate
# all of their arguments, (so you could not pass if or define around,
# but you could pass + around)


def special_fun_op(ns, tok):
    a,b,c = gensym(), gensym(), gensym()
    return "(lambda *%s: reduce((lambda %s,%s: %s %s %s), %s))" % \
           (a, b, c, b, tok, c, a)


def special_fun_not(ns, tok):
    a = gensym()
    return "(lambda %s: not %s)" % (a,a)


special_fun = {
    "+": special_fun_op,
    "*": special_fun_op,
    "**": special_fun_op,
    "-": special_fun_op,
    "/": special_fun_op,
    "%": special_fun_op,
    "&": special_fun_op,
    "|": special_fun_op,
    "^": special_fun_op,
    "<<": special_fun_op,
    ">>": special_fun_op,
    "<": special_fun_op,
    "<=": special_fun_op,
    ">": special_fun_op,
    ">=": special_fun_op,
    "==": special_fun_op,
    "and": special_fun_op,
    "in": special_fun_op,
    "is": special_fun_op,
    "or": special_fun_op,

    "not": special_fun_not,
    }


def normal_call(ns, fn, *tree):
    args = [evaluate(ns, stmt) for stmt in tree]
    return "%s(%s)" % (evaluate(ns, fn), comma(args))


#
# some state that's passed around in the evaluation


# for use with statement variables
def get_global(sym):
    return "globals().__getitem__(%r)" % (sym)


def set_global(sym, val):
    return "globals().__setitem__(%r, %s)" % (sym, val)


# for use with closure variables (which are secretly just arguments to
# an enclosing lambda)
def get_closure(sym):
    return "%s[0]" % sym


def set_closure(sym, val):
    return "%s.__setitem__(0, %s)" %  (sym, val)


def ns_new(macro_d=macros, special_d=specials, special_fund=special_fun):
    # ((sequence of namespaces), macros, specials, special_fun)
    return (None, macro_d.copy(), special_d.copy(), special_fund.copy())


def ns_shadow(ns):
    # push a new empty namespace onto ns
    return seq(({}, ns[0]), *ns[1:])


def ns_add(ns, sym, getter, setter):
    ns[0][0][sym] = (getter, setter)


def ns_sym(ns, sym):
    n = ns[0]
    while n:
        if n[0].has_key(sym):
            return n[0][sym]
        n = n[1]

    return None


def ns_getter(ns, sym):
    s = ns_sym(ns, sym)
    if s:
        return s[0]
    else:
        return get_global


def ns_setter(ns, sym):
    s = ns_sym(ns, sym)
    if s:
        return s[1]
    else:
        return set_global


def ns_macros(ns):
    return ns[1]


def ns_specials(ns):
    return ns[2]


def ns_special_fun(ns):
    return ns[3]


def eval_token(ns, token):
    token = str(token)

    if token[0] in "\'\"":
        return token

    elif "." in token:
        sym,rhs = token.split(".",1)
        if ns_sym(ns, sym):
            return "%s.%s" % (ns_getter(ns, sym)(sym), rhs)
        else:
            return token

    elif ns_sym(ns, token):
        return ns_getter(ns, token)(token)

    elif ns_special_fun(ns).has_key(token):
        return ns_special_fun(ns)[token](ns, token)

    else:
        return token


def eval_tree(ns, tree):
    front = tree[0]

    if isinstance(front, list):
        return normal_call(ns, *tree)

    fun = ns_specials(ns).get(front)
    if fun:
        return fun(ns, *tree)

    mac = ns_macros(ns).get(front)
    if mac:
        return evaluate(ns, mac(*tree[1:]))

    return normal_call(ns, *tree)


def evaluate(ns, expr):
    import sys

    if not ns:
        ns = ns_new()

    try:
        if isinstance(expr, (list, tuple)):
            return eval_tree(ns, expr)
        else:
            return eval_token(ns, expr)

    except SpexyEvaluateException, e:
        raise

    except Exception, e:
        raise SpexyEvaluateException, (str(e), expr), sys.exc_traceback


def evaluate_source(ns, str):
    from cStringIO import StringIO
    tree = build_tree(StringIO(str))
    return special_progn(ns, None, *tree)


def repl(glbls, input=sys.stdin, output=sys.stdout, prompt=">>>> "):
    ns = ns_new()

    print >> output, prompt,

    for tree in gen_exprs(input):
        if tree and tree[0] == "quit":
            break

        try:
            expr = evaluate(ns, tree)
            print "-->", expr

        except SpexyFormException, sfe:
            print >> output, "##", sfe

        else:
            try:
                val = eval(expr, glbls)
                if val:
                    print >> output, repr(val)

            except Exception, e:
                print >> output, "##", e

        if prompt:
            print >> output, prompt,

    # done


if __name__ == "__main__":
    repl(globals())


#
# The end.
