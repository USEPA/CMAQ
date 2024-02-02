import sys
import argparse
try:
    # Python 3.8 or above
    from importlib.metadata import version
except ImportError:
    # Python 3.6
    from importlib_metadata import version


parser = argparse.ArgumentParser()
parser.add_argument('requirements', nargs='*', help='requirements.txt file path(s)', default=['requirements.txt'])
args = parser.parse_args()

vinfo = sys.version_info

def show(libname, vtxt, modifier):
    print(f'{libname:20s} {vtxt:12s} {modifier:12s}')

show('Package', 'Version', 'Required')
show('-'*20, '-'*12, '-'*12)
show('Python', f'{vinfo.major}.{vinfo.minor}.{vinfo.micro}', '>= 3.6')

for reqpath in args.requirements:
    with open(reqpath, 'r') as rf:
        reqtxt = rf.read().strip()
    for reqline in reqtxt.split('\n'):
        if ' ' not in reqline:
            libname = reqline
            modifier = ''
        else:
            si = reqline.find(' ')
            libname = reqline[:si]
            modifier = reqline[si:].strip()

        try:
            vtxt = version(libname)
        except Exception:
            vtxt = 'not found'
        show(libname, vtxt, modifier)
