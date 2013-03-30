#!/bin/bash

for f in `find /home/jok/WolframWorkspaces/Base/MultiAgent/ -iname "*.m"`; do cat $f ; done | wc -l
