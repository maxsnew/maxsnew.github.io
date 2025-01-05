#!/usr/bin/env bash

DEST=`pwd`/content/teaching/eecs-483-wn25

# cd src/483-fa23 && make web && chmod +w HTML_WEB/* && cp HTML_WEB/* $DEST
# cd src/483-wn24/web && make clean && make html && cp -r build/html/* $DEST && rm -rf $DEST/_sources
cd src/483-wn25 && make web && chmod +w HTML_WEB/* && cp HTML_WEB/* $DEST
