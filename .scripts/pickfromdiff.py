#!/usr/bin/python

import sys, os, tempfile
from subprocess import Popen, PIPE

def main(fn):
    patches = Popen(["hg", "qunapplied"], stdout=PIPE).communicate()[0]
    patch = extract(open(".hg/patches/" + patches.split('\n')[0]).read(), fn)
    Popen(["patch", "-p1", "-o", "-"], stdin=PIPE, stderr=PIPE).communicate(patch)

def extract(contents, fn):
    res = []
    for line in contents.split('\n'):
        if line.startswith('diff --git'):
            if line.startswith('diff --git a/'+fn):
                res.append(line)
            elif res:
                break
        elif res:
            res.append(line)
    if res: return '\n'.join(res)+'\n'
    return False


if __name__ == "__main__":
    main(sys.argv[1])
