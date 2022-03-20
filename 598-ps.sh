#!/usr/bin/env bash

src=$2
tgt=content/teaching/eecs-598-w22/docs
tex=problem-set-$1.tex
pdf=problem-set-$1.pdf

cp $src/$tex $tgt/$tex
cp $src/$pdf $tgt/$pdf

