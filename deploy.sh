#!/usr/bin/env bash

cabal build \
    && cabal run maxsnew-exe clean \
    && cabal run maxsnew-exe build \
    && git checkout master \
    && cp -a _site/. . \
    && git add -A \
    && git commit -m "Site updated: $(date)" \
    && git push origin master \
    && git checkout src
