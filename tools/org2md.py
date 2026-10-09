"""Convert the two org-mode blog posts to Markdown for Zola.

Zola has no org reader, so this is a one-time conversion. It also fixes two
things Pandoc got wrong on these files: absolute links became file:/// URLs,
and inline math was mangled because markdown ate the underscores. Math is
protected here by backslash-escaping _ and * inside $...$, which CommonMark
renders back to literal characters for MathJax to pick up.
"""
import re, sys, os

def convert_links(s):
    # [[url][text]] -> [text](url); org's degenerate forms fall back to text.
    def repl(m):
        url, text = m.group(1), m.group(2)
        if not url:
            return text
        if not text:
            return url
        return '[%s](%s)' % (text, url)
    return re.sub(r'\[\[([^\]]*)\]\[([^\]]*)\]\]', repl, s)

def convert_text(s):
    out = []
    for i, seg in enumerate(s.split('$')):
        if i % 2 == 1:                      # inside inline math
            out.append(seg.replace('_', '\\_').replace('*', '\\*'))
        else:
            seg = convert_links(seg)
            seg = re.sub(r'(?m)^(\*+) ', lambda m: '#' * len(m.group(1)) + ' ', seg)
            seg = re.sub(r'(?<![\w*])\*([^*\n]+)\*(?![\w*])', r'**\1**', seg)
            out.append(seg)
    return '$'.join(out)

def convert(path, out_path):
    src = open(path).read()
    m = re.match(r'^---\n(.*?)\n---\n', src, re.S)
    fm, body = m.group(1), src[m.end():]
    title = re.search(r'^title:\s*(.*)$', fm, re.M).group(1).strip()
    if title[0] == title[-1] and title[0] in '"\'':
        title = title[1:-1]
    slug = os.path.basename(path)[:-4]
    date = slug[:10]

    lines, chunks, mode, buf = body.split('\n'), [], 'text', []
    for line in lines:
        st = line.strip()
        if st.startswith('#+begin_'):
            chunks.append((mode, buf)); buf = []
            mode = 'html' if st == '#+begin_html' else 'quote'
            continue
        if st.startswith('#+end_'):
            chunks.append((mode, buf)); buf = []; mode = 'text'
            continue
        if re.match(r'^\[fn:\d+\]:', st):   # unused org footnote definition
            continue
        buf.append(line)
    chunks.append((mode, buf))

    parts = []
    for kind, buf in chunks:
        text = '\n'.join(buf).strip('\n')
        if not text:
            continue
        if kind == 'html':
            # Raw HTML block: CommonMark passes it through untouched, so the
            # LaTeX inside is safe from markdown.
            parts.append('<div>\n' + text + '\n</div>')
        elif kind == 'quote':
            parts.append('\n'.join('> ' + l if l.strip() else '>' for l in text.split('\n')))
        else:
            # Bare $$...$$ display math also needs protecting.
            text = re.sub(r'(?ms)^\$\$(.+?)\$\$\s*$',
                          lambda m: '<div>\n$$' + m.group(1) + '$$\n</div>', text)
            parts.append(convert_text(text))

    toml = ('+++\ntitle = %s\ndate = %s\naliases = ["/blog/%s.html"]\n+++\n\n'
            % ('"%s"' % title.replace('"', '\\"'), date, slug))
    open(out_path, 'w').write(toml + '\n\n'.join(parts).rstrip() + '\n')
    print('wrote %s (title=%r date=%s)' % (out_path, title, date))

for p in sys.argv[1:]:
    convert(p, p[:-4] + '.md')
