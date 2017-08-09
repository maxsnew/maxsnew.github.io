#!/bin/bash

stack exec maxsnew-exe build && git checkout master && git rm -rf --ignore-unmatch * && mv -f _site/* . && rm -rf _site && git add . && git commit -m "Site updated: $(date)" && git push origin master && git checkout src
