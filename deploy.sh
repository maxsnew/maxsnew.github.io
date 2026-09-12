#!/usr/bin/env bash

# Build into _site rather than Zola's default public/: the master branch's
# .gitignore already ignores _site, and the `git add -A` below would otherwise
# commit the whole build output into the deploy branch.
zola build -o _site \
    && git checkout master \
    && cp -a _site/. . \
    && git add -A \
    && git commit -m "Site updated: $(date)" \
    && git push origin master \
    && git checkout src
