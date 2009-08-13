#!/usr/bin/env python

import sys
import os
import threading
import Queue
import subprocess
import traceback

import quodlibet.formats
import quodlibet.config
quodlibet.config.init()

# MUSIC and TRANSCODE _must_ include trailing slash!

MUSIC='/home/steven/Music/'
TRANSCODE='/home/steven/transcode/'
THREADS=2

def transcode(queue, thread_id, start_len):
    n = open('/dev/null', 'rw')
    while True:
    	try:
            src_dir, dst_dir = queue.get_nowait()

            for fn in os.listdir(dst_dir):
                os.unlink(os.path.join(dst_dir, fn))

            for fn in sorted(filter(lambda s: s.endswith('.flac'),
                    os.listdir(src_dir))):
                src_fn = os.path.join(src_dir, fn)
                dst_fn = os.path.join(dst_dir, '%s.mp3' % fn[:-5])
                print "Thread %d: encoding %s" % (thread_id, dst_fn)
                dec = subprocess.Popen(['flac', '-s', '-d', src_fn, '-c'],
                        stdout=subprocess.PIPE, stdin=n)
                subprocess.check_call(['lame', '--preset', 'standard', '-h',
                    '--quiet', '-', dst_fn], stdout=n, stdin=dec.stdout)
                dec.wait()

                src_f = quodlibet.formats.xiph.FLACFile(src_fn)
                dst_f = quodlibet.formats.mp3.MP3File(dst_fn)
                for k in filter(lambda k: not k.startswith('~'), src_f.keys()):
                    if k == 'tracknumber':
                        dst_f[k] = src_f[k].split('/')[0]
                    else:
                        dst_f[k] = src_f[k]
                dst_f.write()

            src_art = os.path.join(src_dir, '.folder.jpg')
            dst_art = os.path.join(dst_dir, '.folder.jpg')
            if os.path.isfile(src_art):
                with open(src_art, 'r') as s:
                    with open(dst_art, 'w') as d:
                        d.write(s.read())

            queue.task_done()
            print("Thread %d: finished (%.1f%%)." % (thread_id,
                    (100. * (start_len - queue.qsize())/start_len)))

        except Queue.Empty:
            break
        except KeyboardInterrupt:
            raise
        except:
            print "\n\nIn dir %s:\n\t" % src_dir
            traceback.print_exc()
            for fn in os.listdir(dst_dir):
                os.unlink(os.path.join(dst_dir, fn))
            queue.task_done()
        #finally:
        #    for fn in os.listdir(tmp_dir):
        #        os.unlink(os.path.join(tmp_dir, fn))
        #    os.rmdir(tmp_dir)

def get_stale(delete = False):
    """Returns the list of mp3s in TRANSCODE whose counterpart flacs in MUSIC
    are either missing or newer.  If delete is true, this function will also
    unlink those mp3s.  This is useful because if there's a bug in the script
    I haven't caught yet (and in the many times I have previously) it will
    clean up partial files too."""
    stale = []
    for root, dirs, files in os.walk(TRANSCODE):
        for file in files:
            if file[-4:] == '.mp3':
                dst = os.path.join(root, file)
                rel = dst.replace(TRANSCODE, '', 1)[:-4]
                src = os.path.join(MUSIC, rel + '.flac')

                if (not os.path.isfile(src) or
                        os.path.getmtime(src) > os.path.getmtime(dst)):
                    stale.append(dst)

    if delete:
        for file in stale:
            print "Unlinking %s" % file
            os.unlink(file)
    return stale


def get_new():
    """The reverse idea of get_stale.  Returns a list of tuples (src, dest).
    Doesn't unlink anything."""
    new = []
    for root, dirs, files in os.walk(MUSIC):
        for file in files:
            if file[-5:] == '.flac':
                src = os.path.join(root, file)
                rel = src.replace(MUSIC, '', 1)[:-5]
                dst = os.path.join(TRANSCODE, rel + '.mp3')

                if (not os.path.isfile(dst) or
                        os.path.getmtime(src) > os.path.getmtime(dst)):
                    new.append((src, dst))
    return new

def main():
    stale = get_stale(True)
    new = get_new()
    new.sort()

    newdirs = set()
    for f in new:
        newdirs.add( (os.path.dirname(f[0]), os.path.dirname(f[1])) )

    for s, d in newdirs:
        if not os.path.isdir(d):
            os.makedirs(d)

    q = Queue.Queue()
    for pair in sorted(newdirs): q.put(pair)

    for i in range(THREADS):
        t = threading.Thread(target = transcode, args=(q, i, q.qsize()))
        t.start()

    q.join()

if __name__ == '__main__':
    main()

