#!/usr/bin/env python

import sys
import os
import threading
import tempfile
import Queue
import subprocess
import traceback

from mutagen import flac, mp4

# MUSIC and TRANSCODE _must_ include trailing slash!

MUSIC='/opt/media/music/'
TRANSCODE='/opt/media/transcode/'
TEMP='/tmp/flac2aac-thread'
THREADS=6
TARGET_QUALITY='0.45'

str_map = {
        'title': '\xa9nam',
        'album': '\xa9alb',
        'artist': '\xa9ART',
        }
tup_map = {
        'tracknumber': 'trkn',
        'discnumber': 'disk'
        }

def tagmap(flac_file, mp4_file):
    f = flac_file
    m = mp4_file
    for tag in f.keys():
        if tag in str_map:
            m[str_map[tag]] = f[tag][0]
        elif tag == 'date':
            m['\xa9day'] = f[tag][0][:4]
        elif tag in tup_map:
            vals = list((int(i) for i in f[tag][0].split('/')))
            if len(vals) == 2:
                m[tup_map[tag]] = [vals]
            else:
                m[tup_map[tag]] = [(vals[0], 0)]
    m['fmd5'] = '%x' % f.info.md5_signature


def transcode(queue, thread_id, start_len):
    tmp_wav_fn = '%s%d.wav' % (TEMP, thread_id)
    n = open('/dev/null', 'rw')
    while True:
	try:
	    src_fn, dst_fn = queue.get_nowait()
            print src_fn
            tmp_mp4_fn = '%s~' % (dst_fn)
            subprocess.check_call(['flac', '-d', src_fn, '-o', tmp_wav_fn,
                                   '-f'], stdout=n, stderr=n, stdin=n)
            subprocess.check_call(['neroAacEnc', '-q', TARGET_QUALITY, '-if',
                                   tmp_wav_fn, '-of', tmp_mp4_fn], stdout=n,
                                   stderr=n, stdin=n)
            src = flac.FLAC(src_fn)
            dst = mp4.MP4(tmp_mp4_fn)
            tagmap(src, dst)
            dst.save()
            os.rename(tmp_mp4_fn, dst_fn)
            queue.task_done()
            print("Thread %d: finished (%.1f%%)." % (thread_id,
                    (100. * (start_len - queue.qsize())/start_len)))

        except Queue.Empty:
            break
        except KeyboardInterrupt:
            raise
        except:
            print "\n\nOn file %s:\n\t" % src_fn
            traceback.print_exc()
            queue.task_done()
        finally:
            if os.path.exists(tmp_wav_fn):
                os.unlink(tmp_wav_fn)
            if os.path.exists(tmp_mp4_fn):
                os.unlink(tmp_mp4_fn)

def get_stale(delete = False):
    """Returns the list of mp4s in TRANSCODE whose counterpart flacs in MUSIC
    are either missing or newer.  If delete is true, this function will also
    unlink those mp4s.  This is useful because if there's a bug in the script
    I haven't caught yet (and in the many times I have previously) it will
    clean up partial files too."""
    stale = []
    for root, dirs, files in os.walk(TRANSCODE):
        for file in files:
            if file[-5:] == '.mp4~':
                os.unlink(os.path.join(root, file))
            elif file[-4:] == '.mp4':
                dst = os.path.join(root, file)
                rel = dst.replace(TRANSCODE, '', 1)[:-4]
                src = os.path.join(MUSIC, rel + '.flac')

                if (not os.path.isfile(src) or
                        os.path.getmtime(src) > os.path.getmtime(dst)):
                    stale.append(dst)

    if delete:
        for file in stale:
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
                dst = os.path.join(TRANSCODE, rel + '.mp4')

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
        newdirs.add(os.path.dirname(f[1]))

    for d in newdirs:
        if not os.path.isdir(d):
            os.makedirs(d)

    q = Queue.Queue()

    for fn in new: q.put(fn)

    for i in range(THREADS):
        t = threading.Thread(target = transcode, args=(q, i, q.qsize()))
        t.start()

    q.join()

if __name__ == '__main__':
    main()

