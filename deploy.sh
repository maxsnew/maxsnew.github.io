#!/usr/bin/env bash

zola build \
    && git checkout master \
    && cp -a public/. . \
    && git add -A \
    && git commit -m "Site updated: $(date)" \
    && git push origin master \
    && git checkout src
