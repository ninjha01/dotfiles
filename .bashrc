###################################################################
##                                                               ##
##                Nishant's Bash Configuration                   ##
##	              There are many like it                     ##
##	               But this one is mine                      ##
##                                                               ##
###################################################################

# Aesthetics #######################################################
export TERM=xterm-256color;
export CLICOLOR=1
export LSCOLORS='GxFxCxDxBxegedabagaced'
export PS1="[\t][\[\e[31m\]\W\[\e[m\]]: "

# Utils ############################################################
alias rm="trash";
alias blacklist="sudo emacs /etc/hosts";
alias la="ls -a";
alias sexit="killall Terminal"
alias cleanup="bash ~/scripts/cleanup.sh";
alias activate='source venv/bin/activate';
alias venv='python3 -m venv venv; activate'

function git_push {
    message=$1
    git add .
    git commit  -m "$message"
    git push
}

function gen_gif {
    in_file=$1
    in_base=$(basename "$in_file" | cut -d. -f1)
    ffmpeg -i $in_file -pix_fmt rgb24 -r 10 "$in_base.gif"
}   
# Emacs ###########################################################
export EDITOR="/usr/local/bin/emacsclient -c -a \"\"";

function emi {
    daemon_name="${1:-server}"
    emacs --daemon="$daemon_name"
    # Show server name in modeline
    emacsclient -s $daemon_name --eval '(setq-default mode-line-misc-info server-name)'
}

function emc {
    daemon_name="${1:-server}"
    emacsclient -s $daemon_name -n -c -a \"\";
}

function emcnw {
    daemon_name="${1:-server}"
    emacsclient -s $daemon_name -n -c -a -nw \"\";
}

function kille {
    daemon_name="${1:-server}"
    emacsclient -s $daemon_name -e '(kill-emacs)'
}

function remc {
    daemon_name="${1:-server}"
    kille $daemon_name &&
    emi $daemon_name &&
    emc $daemon_name
}

# Expanding History Capabilities ##################################
export HISTSIZE=10000;
shopt -s histappend;
PROMPT_COMMAND="history -a;$PROMPT_COMMAND";

# Expanding Bash completion #######################################
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
