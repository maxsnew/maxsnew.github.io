One-time conversion scripts used for the Hakyll -> Zola port. They are not part
of the build; they are kept so the two generated conversions are reproducible.

- `yaml2toml.py publications.yaml.orig ../publications.toml`
  Converts the old publications.yaml, pre-splitting entries into one array per
  group (the sequential List.partition that used to live in src/Lib.hs) and
  making relative `docs/...` links site-absolute.
- `org2md.py <post.org>`
  Converts an org-mode blog post to Markdown. Zola has no org reader.
