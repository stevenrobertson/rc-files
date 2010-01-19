#!/usr/bin/python

import multiprocessing
import subprocess
import re
import sys

def code(args):
    src, start, end = args
    print "Coding from %d to %d (%d)" % (start, start+end, end)
    start_s = '%d:00' % start
    end_s = '%d:10' % end
    out = '%02d-%02d.avi' % (start, start+end)
    null = open('/dev/null', 'w')
    subprocess.check_call(['nice', '-n', '+19', 'mencoder', '-oac', 'mp3lame',
        '-lameopts', 'br=192', '-vf', 'scale=1280:-2', '-ovc', 'lavc',
        '-lavcopts', 'vcodec=mpeg4:vbitrate=6500:threads=2',
        '-ffourcc', 'xvid', '-o', out, '-ss', start_s, '-endpos', end_s, src], stdout=null)

def get_duration(src):
    sub = subprocess.Popen(['mkvinfo', src], stdout=subprocess.PIPE)
    stdout, stderr = sub.communicate()
    for line in stdout.split('\n'):
        m = re.match('[|] [+] Duration: ([1234567890.]+)s', line)
        if m:
            return float(m.group(1))

src = sys.argv[1]
p = multiprocessing.Pool(multiprocessing.cpu_count() - 1)
d = get_duration(src)
parts = []
i = 2
s = 0
while s*60 < d:
    if (s+3*i/2)*60 > d:
        i*=2
    parts.append((src, s, i))
    s += i
    i *= 2
    i = min(i, 32)
p.map(code, parts)
