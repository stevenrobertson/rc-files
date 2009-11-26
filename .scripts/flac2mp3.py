#!/usr/bin/env python

import sys
import os
import multiprocessing
import subprocess
import traceback

import quodlibet.formats
import quodlibet.config
quodlibet.config.init()

def transcode_mp3((src, dst)):
    try:
        n = open('/dev/null', 'rw')
        dec = subprocess.Popen(['flac', '-s', '-d', src, '-c'],
                stdout=subprocess.PIPE, stdin=n)
        subprocess.check_call(['lame', '--preset', 'standard', '-h',
            '--quiet', '--resample', '44.1', '-', dst + '.wip'], stdout=n,
            stdin=dec.stdout)
        dec.wait()

        src_f = quodlibet.formats.xiph.FLACFile(src)
        dst_f = quodlibet.formats.mp3.MP3File(dst + '.wip')
        for k in filter(lambda k: not k.startswith('~'), src_f.keys()):
            if k == 'tracknumber':
                dst_f[k] = src_f[k].split('/')[0]
            else:
                dst_f[k] = src_f[k]
        dst_f.write()
        os.rename(dst + '.wip', dst)
        return dst
    except:
        traceback.print_exc()
        return None

def transcode_ogg((src, dst)):
    try:
        subprocess.check_call(['oggenc', '-Q', '-q', '3.5', '-o', dst + '.wip', src])
        os.rename(dst + '.wip', dst)
        return dst
    except:
        traceback.print_exc()
        return None

def clean_stale(src_dir, dst_dir, ext):
    for root, dirs, files in os.walk(dst_dir):
        for file in files:
            dst = os.path.join(root, file)
            rel = os.path.splitext(os.path.relpath(dst, dst_dir))[0]
            src = os.path.join(src_dir, rel + '.flac')
            if dst.endswith('.wip'): os.unlink(dst)
            if dst.endswith(ext):
                if (not os.path.isfile(src) or
                        os.path.getmtime(src) > os.path.getmtime(dst)):
                    print "Unlinking %s" % file
                    os.unlink(dst)
                    files.remove(file)
        if files == ['.folder.jpg']: os.unlink(os.path.join(root, files[0]))
        if not files and not dirs:
            os.removedirs(root)

def get_new(src_dir, dst_dir, ext):
    new = []
    for root, dirs, files in os.walk(src_dir):
        dst_root = os.path.join(dst_dir, os.path.relpath(root, src_dir))
        for file in files:
            if file.endswith('.flac'):
                src = os.path.join(root, file)
                dst = os.path.join(dst_root, os.path.splitext(file)[0] + ext)
                if not os.path.isdir(dst_root):
                    print "Creating %s" % dst_root
                    os.makedirs(dst_root)

                if (not os.path.isfile(dst) or
                    os.path.getmtime(src) > os.path.getmtime(dst)):
                    new.append((src, dst))

        img_src = os.path.join(root, '.folder.jpg')
        img_dst = os.path.join(dst_root, '.folder.jpg')

        if (os.path.isfile(img_src) and os.path.isdir(dst_root) and
                not os.path.isfile(img_dst)):
            os.link(img_src, img_dst)
    return new

def main():
    src_dir = '/opt/media/music/'

    dsts =  [
                ('/opt/media/transcode/', '.mp3', transcode_mp3),
                ('/opt/media/oggtranscode/', '.ogg', transcode_ogg),
            ]

    pool = multiprocessing.Pool()

    new = []

    for (dst_dir, ext, func) in dsts:
        clean_stale(src_dir, dst_dir, ext)
        new += get_new(src_dir, dst_dir, ext)
    new.sort()

    for idx, fn in enumerate(pool.imap(func, new)):
        print "Finished encoding %s (%2.1f%%)" % (fn, 100.*idx/len(new))
        if not fn:
            print "Exception caught, terminating"
            pool.terminate()

if __name__ == '__main__':
    main()

