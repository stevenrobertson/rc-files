#!/usr/bin/python

import os

tree = {}

for path in os.listdir('/proc'):
    if not path.isdigit():
        continue
    try:
        with open('/proc/%s/status' % path) as fptr:
            lns = fptr.read().split('\n')
            pid = filter(lambda s: s.startswith('Pid:\t'), lns)[0].split()[1]
            ppid = filter(lambda s: s.startswith('PPid:\t'), lns)[0].split()[1]
            name = filter(lambda s: s.startswith('Name:\t'), lns)[0]
            name = name.split('\t', 1)[1]
            parent = tree.setdefault(ppid, {'children': []})
            proc = tree.setdefault(pid, {'children': []})
            proc.update({'pid': pid, 'ppid': ppid, 'name': name})
            parent['children'].append(pid)
            tree[pid] = proc
    except IOError, IndexError:
        pass
for zsh in filter(lambda p: p.get('name') == 'zsh', tree.values()):
    if zsh['children']:
        continue
    parent = tree.get(zsh['ppid'])
    while parent:
        if parent['pid'] == '1':
            break
        if parent['name'] == 'xterm' or parent['name'] == 'gnome-terminal':
            try:
                os.kill(int(zsh['pid']), 1)
            except:
                pass
            break
        parent = tree.get(parent['ppid'])

