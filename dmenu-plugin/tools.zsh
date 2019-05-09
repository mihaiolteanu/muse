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
