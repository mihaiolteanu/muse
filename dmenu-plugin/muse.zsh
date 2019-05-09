#!/usr/bin/env zsh

# Extract the contents of any html tag. Pure magic!
alias hxmagic="hxnormalize -x 2> /dev/null | hxselect -c"

mycurl() {
    local url=$1
    local user_agent="User-Agent: Mozilla/5.0 (Macintosh; \ 
        Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML,\
        like Gecko) Chrome/44.0.2403.89 Safari/537."
    curl -sLH $user_agent $1
}

list_artists () {
    mycurl "http://127.0.0.1:4007/artists" |
        hxmagic -s "\n" "a::attr(href)" |
        grep "/artist/" |
        sed 's/\/artist\///g' |
        sed 's/+/ /g'
}

list_tags () {
    mycurl "http://127.0.0.1:4007/tags" |
        hxmagic -s "\n" "a::attr(href)" |
        grep "/tag/" |
        sed 's/\/tag\///g' |
        sed 's/+/ /g'
}

select_artist () {
    artist= list_artists | rofi -dmenu -i -p "Select artist"
    echo -n $artist
}

select_tag () {
    # tag= list_tags | rofi -dmenu -i -p "Select tag"
    tag= list_tags | dmenu -i -l 15 -fn "Terminus-15" -p "Select tag"
}

play_artist () {
    mycurl $(printf "http://127.0.0.1:4007/play?source=/artist/%s" $1)
}

main () {
    while getopts "at" opt; do
        case $opt in
            a)
                play_artist $(select_artist)
                ;;
            t)
                select_tag
                ;;
        esac
    done
    shift $((OPTIND-1))
}

main $@


