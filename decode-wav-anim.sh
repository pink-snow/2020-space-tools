#!/usr/bin/env zsh

set -e

from1=0.8
to1=1.0

from2=3.01
to2=2.99

typeset -Z 5 f
f=0

IN=$1
OUT=$2

rm -rf $OUT
mkdir -p $OUT

for i in {1..100}
	./decode-wav-anim-frame.rs $IN $OUT/${(l:5::0:)$((f++))}.png \
		$(( from1 + (to1-from1) * (1. - (100-i) * (100-i) / 10000.) )) $from2

for i in {1..10}
	./decode-wav-anim-frame.rs $IN $OUT/${(l:5::0:)$((f++))}.png $to1 \
		$(( from2 + (to2-from2) * (1. - (10-i) * (10-i) / 100.) ))

convert $OUT/*.png $OUT/anim.gif
