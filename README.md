# maxsnew

Built with [Zola](https://www.getzola.org/). Install it with `brew install zola`;
there is no other toolchain to set up.

To build
```sh
zola build -o _site   # or ./deploy.sh
```

To preview locally with live reload
```sh
zola serve
```

To deploy to gh pages
```sh
./deploy.sh
```

## Layout

- `content/` — the pages themselves (markdown, TOML front matter).
- `static/` — everything copied verbatim: css, js, images, `docs/` (CV, slides,
  papers) and the prebuilt EECS 483 course sites under `static/teaching/`.
  Hard-linked rather than copied at build time, so the ~260MB costs nothing.
- `templates/` — Tera templates. `base.html` is the shared layout;
  `class.html` is the standalone layout for the 598 course pages.
- `publications.toml` — publication data, read by `templates/publications.html`
  via `load_data`. Entries are pre-split into one array per group
  (`manuscript`, `paper`, `preprint`, `abstract`).
- The EECS 598 `docs/` directories live beside their `index.md` under
  `content/` as colocated assets, so they publish at the same URLs as before.

Note: this needs Zola >= 0.23, which moved to Tera v2 (macros removed,
`filter` filter removed). Templates here use Tera v2 components.

## Purescript (unused)

The `ps/` directory is not currently built into the site.
```sh
npm install -g bower pulp purescript
cd ps && bower install && pulp build --to ../static/js/hubway.js
```
