#!/usr/bin/env bash

set -e

cat << EOF
<html>
	<head><title>Annotated</title>
	<style>img {max-width:46vw} pre {overflow-x: auto}</style>
	</head>
	<body>
EOF
for i in *.txt; do
	i=${i%.txt}
	echo '<h1>'$i'</h1><div><a href="'$i'.svg"><img src="'$i'.svg"></a><a href="'$i'.png"><img src="'$i'.png"></a></div>'
	echo '<pre>'
	cat $i.txt
	echo '</pre>'
done
echo '</body></html>'
