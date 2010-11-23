import sys

__module__ = {}

def load(module,cwd):
    path0 = sys.path[0]
    sys.path[0] = cwd
    try:
        if __module__.get(module):
            m = reload(__import__[module])
            __module__[module] = m
        else:
            m = __import__(module,globals(),locals(),[])
    finally:
        sys.path[0] = path0
    return m

