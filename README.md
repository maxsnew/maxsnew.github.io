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

To deploy

Pushing to `main` (or `src`) runs `.github/workflows/deploy.yml`, which builds
and publishes straight to GitHub Pages. This needs a one-time setting:
**Settings > Pages > Source = "GitHub Actions"**.

`deploy.sh` is the old path and still works while Pages is served from the
`master` branch. Delete it, and the `master` branch, once the workflow is live.
Prefer the workflow: deploy.sh copies files over `master` and runs `git add -A`
without ever removing anything, so it accumulates every file that has ever
passed through the working tree - including untracked ones it sweeps up by
accident. The workflow publishes a fresh artifact each run.

`static/CNAME` keeps the maxsnew.com custom domain attached to the output.

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
