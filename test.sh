#!/bin/sh
set -v
cat input.txt | valgrind ./driver
dot -Tjpeg graph.gv > graph.jpeg
#geeqie graph.jpeg
for file in list*.gv; do
    [ -f "$file" ] || break
    echo $file
    dot -Tjpeg $file > $file.jpeg 
done
convert list*.jpeg -append adjlist.jpeg
rm *.gv
rm list*.jpeg
