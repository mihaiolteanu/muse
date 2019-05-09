curl -s http://127.0.0.1:4007/genres >> /dev/null | hxselect -c -s "
" a | dmenu -p "Genre:" -fn "Terminus-15" -l 10
