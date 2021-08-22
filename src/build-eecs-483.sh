#!/usr/bin/env bash

DEST=`pwd`/content/teaching/eecs-483-fa21

cd src/eecs-483 && make web && chmod +w HTML_WEB/* && cp HTML_WEB/* $DEST
