###################################################################
##                                                               ##
##                Nishant's Bash Configuration                   ##
##	              There are many like it                     ##
##	               But this one is mine                      ##
##                                                               ##
###################################################################

###################################################################
### Work Aliases
###################################################################
alias emeril="conda activate emeril; cd ~/Desktop/emeril/"
alias depmap="conda activate depmap; cd ~/Desktop/depmap/"
# alias honk="conda activate honk; cd ~/Desktop/honk/"
alias loom="conda activate loom; cd ~/Desktop/loom/"

###################################################################
### Aesthetics
###################################################################

export TERM=xterm-256color;
export CLICOLOR=1
export LSCOLORS='GxFxCxDxBxegedabagaced'
export PS1="[\t][\[\e[31m\]\W\[\e[m\]]: "

###################################################################
### Misc Behavior Mod
###################################################################

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend
# append and reload the history after each command
PROMPT_COMMAND="history -a; history -n"

# ignore certain commands from the history
HISTIGNORE="ls:ll:pwd:bg:fg:history"

# unlimited history
export HISTSIZE=
export HISTFILESIZE=

# Hide bash warning on macOS
export BASH_SILENCE_DEPRECATION_WARNING=1

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
alias vie="emacs -q -nw"

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
    du -kx "${dir}" | sort -n | tail -200
}

function port_hog {
    PORT=$1
    sudo lsof -i -P | grep "$PORT"
}


function gen_gif {
    in_file=$1
    in_base=$(basename "$in_file" | cut -d. -f1)
    ffmpeg -i $in_file -pix_fmt rgb24 -r 10 "$in_base.gif"
}


###################################################################
### Emacs
###################################################################

export EDITOR="/usr/local/bin/emacsclient -c -a \"\""

function emi {
    daemon_name="${1:-server}"
    emacs --daemon="$daemon_name"
    if [ "$daemon_name" != "server" ]; then
	emacsclient -s "$daemon_name" --eval '(setq-default mode-line-misc-info (concat "Ψ: " server-name ""))'
    fi
}

function emc {
    daemon_name="${1:-server}"
    emacsclient -s "$daemon_name" -n -c -a \"\"
}

function emcnw {
    daemon_name="${1:-server}"
    emacsclient -s "$daemon_name" -n -c -a -nw \"\"
}

function kille {
    daemon_name="${1:-server}"
    emacsclient -s "$daemon_name" -e '(kill-emacs)'
}

function remc {
    daemon_name="${1:-server}"
    kille "$daemon_name"
    emi "$daemon_name" &&
	emc "$daemon_name"
    exit
}


###################################################################
### Completion
###################################################################

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
