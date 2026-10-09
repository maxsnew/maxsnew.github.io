import re, sys

def unquote(v):
    v = v.strip()
    if len(v) >= 2 and v[0] == v[-1] and v[0] in '"\'':
        return v[1:-1]
    return v

def tstr(s):
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'

entries, cur, in_links, last_key = [], None, False, None
for raw in open(sys.argv[1]):
    line = raw.rstrip('\n')
    if not line.strip():
        continue
    m = re.match(r'^- (\w+): (.*)$', line)
    if m:
        cur = {'links': {}}
        entries.append(cur)
        cur[m.group(1)] = unquote(m.group(2))
        in_links, last_key = False, m.group(1)
        continue
    m = re.match(r'^  (\w+):\s*(.*)$', line)
    if m:
        key, val = m.group(1), m.group(2)
        if key == 'links' and val == '':
            in_links = True
        else:
            in_links, last_key = False, key
            cur[key] = unquote(val)
        continue
    m = re.match(r'^    (.+?):\s*(.*)$', line)
    if m and in_links:
        cur['links'][m.group(1).strip()] = unquote(m.group(2))
        continue
    # YAML folds a wrapped plain scalar onto the previous key with one space.
    if re.match(r'^\s+\S', line) and not in_links and last_key:
        cur[last_key] = cur[last_key] + ' ' + line.strip()
        continue
    raise SystemExit('UNPARSED LINE: %r' % line)

def group_of(e):
    if e.get('manuscript') == 'True': return 'manuscript'
    if e.get('abstract') == 'True':   return 'abstract'
    if e.get('preprint') == 'True':   return 'preprint'
    return 'paper'

GROUPS = ('manuscript', 'paper', 'preprint', 'abstract')
out = ['# Generated from the old publications.yaml by yaml2toml.py.',
       '# Entries are pre-split into one array per group, replacing the',
       '# sequential List.partition in the old Haskell Lib.hs. Tera v2 removed',
       '# the `filter` filter, so the template just reads the four arrays.',
       '# Order within each array matches the original file.', '']
present = set(group_of(e) for e in entries)
for g in GROUPS:
    if g not in present:
        out.append('%s = []' % g)
out.append('')
for e in entries:
    out.append('[[%s]]' % group_of(e))
    for k in ('title', 'id', 'authors'):
        out.append('%s = %s' % (k, tstr(e[k])))
    # Always emitted, empty when absent, so the template can test truthiness
    # without worrying about undefined keys.
    for k in ('venue', 'note'):
        out.append('%s = %s' % (k, tstr(e.get(k, ''))))
    out.append('')
    out.append('[%s.links]' % group_of(e))
    # Sorted to match the old Haskell, which rendered links via Map.toList.
    for lk in sorted(e['links']):
        lv = e['links'][lk]
        # These were relative ("docs/x.pdf"), which resolved correctly only
        # because the old page lived at /publications.html. The page is now at
        # /publications/, so make them site-absolute.
        if not re.match(r'^(https?:)?//', lv):
            lv = '/' + lv.lstrip('/')
        out.append('%s = %s' % (tstr(lk), tstr(lv)))
    out.append('')

open(sys.argv[2], 'w').write('\n'.join(out))
from collections import Counter
print('entries:', len(entries))
print('groups:', Counter(group_of(e) for e in entries))
print('with venue:', sum(1 for e in entries if 'venue' in e), 'with note:', sum(1 for e in entries if 'note' in e))
