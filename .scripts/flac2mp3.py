#!/usr/bin/env python

import sys
import os
import multiprocessing
import subprocess
import traceback

from mutagen.flac import FLAC

import quodlibet.formats
import quodlibet.config
quodlibet.config.init()

def qlopen(dst):
    if dst.endswith('.mp3') or dst.endswith('.mp3.wip'):
        return quodlibet.formats.mp3.MP3File(dst)
    else:
        return quodlibet.formats.xiph.OggFile(dst)

def retag(src, dst):
    srcf = quodlibet.formats.xiph.FLACFile(src)
    dstf = qlopen(dst)
    for key in filter(lambda k: not k.startswith('~'), srcf.keys()):
        if key == 'tracknumber':
            dstf['tracknumber'] = srcf['tracknumber'].split('/')[0]
        else:
            dstf[key] = srcf[key]
    dstf['flac_md5'] = '%x' % FLAC(src).info.md5_signature
    dstf.write()

def check_md5(src, dst):
    srcf = FLAC(src)
    dstf = qlopen(dst)
    return dstf.get('flac_md5') == '%x' % srcf.info.md5_signature

def decider((src, dst)):
    """multiprocessing tries (and fails) to pickle the functions"""
    if dst.endswith('.mp3'):
        return transcode_mp3(src, dst)
    else:
        return transcode_ogg(src, dst)

def transcode_mp3(src, dst):
    try:
        dec = subprocess.Popen(['flac', '-s', '-d', src, '-c'],
            stdout=subprocess.PIPE)
        subprocess.check_call(['nice', '-n', '+19', 'lame', '--preset',
            'fast', 'medium', '-h', '--quiet', '--resample', '44.1', '-',
            dst + '.wip'], stdin=dec.stdout)
        dec.wait()
        retag(src, dst + '.wip')
        os.rename(dst + '.wip', dst)
        return dst
    except:
        traceback.print_exc()
        return None

def transcode_ogg(src, dst):
    try:
        dec = subprocess.Popen(['flac', '-s', '-d', src, '-c'],
            stdout=subprocess.PIPE)
        subprocess.check_call(['nice', '-n', '+19', 'oggenc', '-Q',
            '-q', '3.5', '-o', dst+'.wip', '-'], stdin=dec.stdout)
        dec.wait()
        retag(src, dst + '.wip')
        os.rename(dst + '.wip', dst)
        return dst
    except:
        traceback.print_exc()
        return None

def clean_stale(src_dir, dst_dir, ext):
    for root, dirs, files in os.walk(dst_dir):
        for file in sorted(files):
            dst = os.path.join(root, file)
            rel = os.path.splitext(os.path.relpath(dst, dst_dir))[0]
            src = os.path.join(src_dir, rel + '.flac')
            if dst.endswith('.wip'): os.unlink(dst)
            if dst.endswith(ext):
                unlink = False
                if os.path.isfile(src):
                    if os.path.getmtime(src) > os.path.getmtime(dst):
                        if check_md5(src, dst):
                            print "Retagging %s/%s" % (rel, file)
                            retag(src, dst)
                        else: unlink = True
                else: unlink = True
                if unlink:
                    print "Unlinking %s/%s" % (rel, file)
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
                ('/opt/media/transcode/', '.mp3'),
                ('/opt/media/oggtranscode/', '.ogg'),
            ]

    pool = multiprocessing.Pool()

    new = []

    for (dst_d, ext) in dsts:
        clean_stale(src_dir, dst_d, ext)
        new += get_new(src_dir, dst_d, ext)
    new.sort()
    if len(sys.argv) > 1:
        print "Matching " + sys.argv[1]
        new = filter(lambda srcdst: sys.argv[1] in srcdst[0], new)
    print "Starting encoding"

    for idx, fn in enumerate(pool.imap(decider, new)):
        print "Finished encoding %s (%d/%d)" % (fn, idx, len(new))
        if not fn:
            print "Exception caught, terminating"
            pool.terminate()
            break

if __name__ == '__main__':
    main()

