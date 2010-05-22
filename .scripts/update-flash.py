#!/usr/bin/python

import random
import os
import sys
import subprocess


SRC='/opt/media/transcode'
DST='/mnt/floppy'
EXT='.mp3'
MIN_FREE=200
SEP='^'

if '-p' in sys.argv:
    SRC='/opt/media/oggtranscode'
    DST='/mnt/floppy/Music'
    EXT='.ogg'
    MIN_FREE=1000
    SEP='^'

src_dirs = set()
for root, dirs, files in os.walk(SRC):
    if filter(lambda s: s.endswith(EXT), files):
        src_dirs.add(os.path.relpath(root, start=SRC))


dst_dirs = set()
for root, dirs, files in os.walk(DST):
    src_rel = os.path.relpath(root, start=DST).replace(SEP, '/')
    if filter(lambda s: s.endswith(EXT), files):
        if 'incomplete' in files or src_rel not in src_dirs:
            print "Removing ", root
            map(lambda fn: os.unlink(os.path.join(root, fn)), files)
            os.removedirs(os.path.join(DST, root))
            dst_dirs.add(src_rel)

done_dirs = set()
try:
    with open(os.path.join(DST, 'previously.txt')) as f:
        map(done_dirs.add, map(str.strip, f.readlines()))
except IOError:
    pass

candidates = list(src_dirs.difference(dst_dirs, done_dirs))
random.shuffle(candidates)

with open(os.path.join(DST, 'previously.txt'), 'a') as f:
    for dir in candidates:
        stat = os.statvfs(DST)
        if stat.f_bsize * stat.f_bavail < (MIN_FREE << 20):
            break
        dstdir = os.path.join(DST, dir.replace('/', SEP))
        os.makedirs(dstdir)
        print dstdir
        open(os.path.join(dstdir, 'incomplete'), 'w').close()
        subprocess.check_call(['rsync', '-rPt', '--modify-window', '30',
            os.path.join(SRC, dir) + '/', dstdir])
        f.write('%s\n' % dir)
        f.flush()
        os.unlink(os.path.join(dstdir, 'incomplete'))
        dst_dirs.add(dir)

