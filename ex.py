import sys

__module__ = {}

def load(module,cwd):
    import os.path
    path0 = sys.path[0]
    sys.path[0] = os.path.expanduser(cwd)
    try:
        if __module__.get(module):
            m = reload(__import__(module))
        else:
            m = __import__(module,globals(),locals(),[])
        __module__[module] = m
    finally:
        sys.path[0] = path0
##    print >> sys.stderr, "[ex] import:", m
    return m


def print_all_modules(is_subpath_enable=True):
    import pydoc
    import os

    # excludes = [d for d in sys.path if not os.access(d, os.R_OK)]
    excludes = [ d for d in sys.path if "/tmp" in d] # adhoc
    def _all_modules():
        r = []
        def callback(path, modname, desc):
            if is_subpath_enable or modname.find(".") < 0:
                r.append(modname)
        pydoc.ModuleScanner().run(callback)
        return sorted(r)
    try:
        sys.path = list(set(sys.path).difference(excludes))
        for i in  _all_modules():
            print i
    finally:
        sys.path = list(set(excludes).union(sys.path))

