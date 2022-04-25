#!/bin/bash -i
###################################################################
##                                                               ##
##                Nishant's Zsh Configuration                    ##
##	              There are many like it                     ##
##	               But this one is mine                      ##
##                                                               ##
###################################################################

###################################################################
### Aesthetics
###################################################################

# https://stackoverflow.com/questions/689765/how-can-i-change-the-color-of-my-prompt-in-zsh-different-from-normal-text
PS1=$'[%(?.%F{green}√.%F{red}X)%f][%*][%F{red}%1d%f]: '

# Silence login message
touch ~/.hushlogin

# ls colors
CLICOLOR=1
LSCOLORS=gafacadabaegedabagacad

###################################################################
### Misc Behavior Mod
###################################################################

# Auto CD
setopt AUTO_CD

# Case insensitive globbing
setopt NO_CASE_GLOB

# append and reload the history after each command
PROMPT_COMMAND="history -a; history -n"


# Autocorrect
unsetopt correct
# setopt CORRECT
# setopt CORRECT_ALL

###################################################################
### History
###################################################################

# append and reload the history after each command
PROMPT_COMMAND="history -a; history -n"

# Store History
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history

# Add metadata
setopt EXTENDED_HISTORY

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# share history across multiple zsh sessions
setopt SHARE_HISTORY

# adds commands as they are typed, not at shell exit
setopt INC_APPEND_HISTORY


# append to the history file, don't overwrite it
setopt APPEND_HISTORY

# expire duplicates first
setopt HIST_EXPIRE_DUPS_FIRST 
# do not store duplications
setopt HIST_IGNORE_DUPS
#ignore duplicates when searching
setopt HIST_FIND_NO_DUPS
# removes blank lines from history
setopt HIST_REDUCE_BLANKS

# ignore certain commands from the history
HISTIGNORE="ls:ll:pwd:bg:fg:history"

# search history with current text or go up
bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward

HISTSIZE=20000
SAVEHIST=$HISTSIZE


###################################################################
### Utils
###################################################################

alias rm="trash"
alias blacklist="sudo emacs /etc/hosts"
alias la="ls -a"
alias sexit="killall Terminal"
alias cleanup="bash ~/scripts/cleanup.sh"
alias activate='source venv/bin/activate'
alias venv='python3 -m venv venv; activate'
alias sizeof="du -sh"
alias ca="conda activate"
alias cda="conda deactivate"
alias vi="emacs -Q -nw"
alias vim="vi"

function notify {
    title=$1
    body=$2
    osascript -e "display notification \"${body}\" with title \"${title}\""
}

function git_push {
    message=$1
    git add .
    git commit  -m "$message"
    git push
}

function sizeup {
    dir=$1
    if [ -z "$dir" ]
    then
	dir="$(pwd)"
    fi
    du -hx "${dir}" | sort -n | tail -200
}

function port_hog {
    PORT=$1
    sudo lsof -i -P | grep "$PORT"
}


function gen_gif {
    in_file=$1
    in_base=$(basename "$in_file" | cut -d. -f1)
    ffmpeg -i "$in_file" -pix_fmt rgb24 -r 10 "$in_base.gif"
}


###################################################################
### Emacs
###################################################################

EDITOR='emacsclient --create-frame --alternate-editor=""'
alias emc='emacsclient --no-wait --create-frame --alternate-editor=""'
alias emcnw='emacsclient -tty --create-frame --alternate-editor=""'
alias kille="emacsclient -e '(kill-emacs)'"
alias remc="kille;emc && exit"

function em {
    if [ "$#" -eq 0 ]
    then
	echo "Starting new Emacs process ..." >&2
	emacs & disown
    elif emacsclient -n "$@" 2> /dev/null
    then
	echo "Opened $* in Emacs server" >&2
    else
	echo "Opening $* in a new Emacs process ..." >&2
	emacs "$@" & disown
    fi
}
