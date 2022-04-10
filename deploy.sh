#!/usr/bin/env bash

stack build \
    && stack exec maxsnew-exe clean \
    && stack exec maxsnew-exe build \
    && git checkout master \
    && cp -a _site/. . \
    && git add -A \
    && git commit -m "Site updated: $(date)" \
    && git push origin master \
    && git checkout src
