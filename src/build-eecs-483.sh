#!/usr/bin/env bash

DEST=`pwd`/content/teaching/eecs-483-fa23

cd src/483-fa23 && make web && chmod +w HTML_WEB/* && cp HTML_WEB/* $DEST
