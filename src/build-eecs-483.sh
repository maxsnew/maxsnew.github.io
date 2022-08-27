#!/usr/bin/env bash

DEST=`pwd`/content/teaching/eecs-483-fa22

cd src/483fa22 && make web && chmod +w HTML_WEB/* && cp HTML_WEB/* $DEST
