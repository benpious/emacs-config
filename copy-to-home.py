from shutil import copyfile
import os
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('-to', action='store_true')
parser.add_argument('-from', action='store_true')
args = parser.parse_args()


ours = 'init.el'
theirs = os.path.expanduser('~/.emacs.d/tmp/init.el')
dst = ''
src = ''
if args.to == True:
    dst = theirs
    src = ours
else if args.from == True:
    dst = ours
    src = theirs
else:
    print('Must specify -to or -from')
    exit()

dirname = os.path.dirname(dst)
if not os.path.exists(dirname):
    os.makedirs(dirname)

copyfile(src, dst)
