#!/usr/bin/env python
import os, re
from os.path import *
from subprocess import Popen, PIPE
from BaseHTTPServer import HTTPServer
from SimpleHTTPServer import SimpleHTTPRequestHandler

def get_global_doc_dir():
    subp = Popen('ghc-pkg describe base'.split(), stdout=PIPE)
    subp.wait()
    data = subp.stdout.read()
    return re.search(r'\nhaddock-html: (.*)\n', data).group(1)

class DocHandler(SimpleHTTPRequestHandler):
    doc_dirs = [ get_global_doc_dir()
               , realpath(expanduser('~/.cabal/share/doc')) ]

    def translate_path(self, path):
        path = normpath(path)
        for dir in self.doc_dirs:
            if path.startswith(dir):
                return path
        return SimpleHTTPRequestHandler.translate_path(self, path)

def main():
    os.chdir(DocHandler.doc_dirs[1])
    httpd = HTTPServer(('127.0.0.1', 5480), DocHandler)
    httpd.serve_forever()

if __name__ == '__main__':
    main()
