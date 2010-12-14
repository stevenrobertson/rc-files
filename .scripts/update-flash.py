#!/usr/bin/python

import random, os, sys, subprocess, re, base64, doctest
from os.path import join, relpath
from itertools import *

# Custom (lexically-ordered) base-64 encoding of 24-bit ints (4 chars)
B64STR='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ^_abcdefghijklmnopqrstuvwxyz'
b64dec = lambda val: reduce(lambda v, c: (v << 6) + B64STR.index(c), val, 0)
def b64enc(val):
    """
    >>> r = sorted([random.randrange(0, 1<<24) for r in range(1000)])
    >>> rr = sorted(map(b64enc, r))
    >>> r == map(b64dec, rr)
    True
    """
    r = ''
    for i in range(4):
        r = B64STR[val & 63] + r
        val >>= 6
    return r

def fmt_dst_dir(idx, src_dir):
    """
    >>> fmt_dst_dir(0xffffff, 'longish title/longish album')
    'zzzz longish titl - longish albu'
    >>> fmt_dst_dir(0xab96fe, 'yay/endswithspc ')
    'etRy yay - endswithspc'
    """
    if '-p' in sys.argv:
        return src_dir
    return b64enc(idx) + ' ' + ' - '.join(
            [s[:12].strip() for s in src_dir.split('/')])

def remove_dir(path):
    print "Removing ", path
    [os.unlink(join(path, fn)) for fn in os.listdir(path)]
    os.removedirs(path)

def main():
    SRC='/opt/media/transcode'
    DST='/mnt/floppy'
    EXT='.mp3'
    MIN_FREE=200

    if '-p' in sys.argv:
        SRC='/opt/media/oggtranscode'
        DST='/mnt/floppy/Music'
        EXT='.ogg'
        MIN_FREE=1000

    # Create list of current source directories holding music
    src_dirs = set()
    for root, dirs, files in os.walk(SRC):
        if filter(lambda s: s.endswith(EXT), files):
            src_dirs.add(relpath(root, SRC))

    # Read history file, build (dst_dir => src_dir) map
    try:
        with open(join(DST, 'history.txt')) as histf:
            lines = filter(None, histf.read().split('\n'))
    except IOError:
        lines = ''
    drive_pairs = [l.split('\t') for l in lines if l.split('\t')[1] in src_dirs]
    drive_map = dict(drive_pairs)

    # Used later to number the directory names
    index = 0xffffff - len(lines)

    # Read flash drive to find out what's still there. Delete anything that was
    # also deleted from the source directories
    current_dirs = set()
    for root, dirs, files in os.walk(DST):
        if not dirs and not files and root != DST:
            remove_dir(root)
        if filter(lambda s: s.endswith(EXT), files):
            if relpath(root, DST) not in drive_map:
                remove_dir(root)
            else:
                current_dirs.add(relpath(root, DST))

    # Create the probability dictionary, every source dir gets the same chance
    prob_dict = dict([(d, 1.0) for d in src_dirs])

    # Multiply the probability of any old entry by the inverse of its age -
    # items deleted recently will have very low probability, items deleted
    # multiple times will also have low probability
    for i, (k, v) in enumerate(drive_pairs):
        prob_dict[v] *= 1 - float(i)/len(drive_pairs)

    # Remove any source dir which is already on the drive
    for d in set(map(drive_map.get, current_dirs)): del prob_dict[d]

    # Construct the cumulative probability dictionary, (cprob => src_dir)
    total_prob = sum(prob_dict.values())
    cprob = 0
    cprob_dict = {}
    for prob, src_dir in reversed(sorted([(v, k) for (k, v) in prob_dict.items()])):
        cprob_dict[cprob] = src_dir
        cprob += prob / total_prob

    l = 0
    for k, v in sorted(cprob_dict.items()):
        print '%01.6f %01.6f %s' % (k, k - l, v)
        l = k

    with open(os.path.join(DST, 'history.txt'), 'a') as histf:
        while True:
            # Ensure we have MIN_FREE MB of free space remaining, or quit
            stat = os.statvfs(DST)
            if stat.f_bsize * stat.f_bavail < (MIN_FREE << 20):
                break

            rval = random.random()
            # Select the highest cumulative probability less than the value
            # Hardly computationally efficient, but doesn't take much code
            cprob = sorted([c for c in cprob_dict.keys() if c < rval])[-1]

            # Pop this dir so it can't be copied again. This effectively
            # increases the cumulative probability of the item beneath it, but
            # since the dict was reverse-sorted before calculating cprobs, this
            # doesn't make a large difference
            src_dir = cprob_dict.pop(cprob)
            print src_dir

            # rsync the files across
            dst_dir = join(DST, fmt_dst_dir(index, src_dir))
            index -= 1
            os.makedirs(dst_dir)
            subprocess.check_call([
                'rsync', '--partial', '-r', '--modify-window', '30',
                os.path.join(SRC, src_dir) + '/', dst_dir])
            histf.write('%s\t%s\n' % (relpath(dst_dir, DST), src_dir))
            histf.flush()

if __name__ == "__main__":
    if '-t' in sys.argv:
        doctest.testmod()
        print 'Done testing'
    else:
        main()
