#!/usr/bin/env bash

cabal build --allow-newer=base \
    && cabal run maxsnew-exe --allow-newer=base clean \
    && cabal run maxsnew-exe --allow-newer=base build \
    && git checkout master \
    && cp -a _site/. . \
    && git add -A \
    && git commit -m "Site updated: $(date)" \
    && git push origin master \
    && git checkout src
