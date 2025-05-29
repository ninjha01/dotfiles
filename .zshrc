#!/bin/zsh
### Aesthetics
PS1=$'[%(?.%F{green}√.%F{red}X)%f][%*][%F{red}%1d%f]: '
touch ~/.hushlogin
export CLICOLOR=1
export LSCOLORS=gafacadabaegedabagacad

### Misc Behavior Mod
export HOMEBREW_NO_ANALYTICS=1
autoload -U select-word-style
select-word-style bash
setopt AUTO_CD
setopt NO_CASE_GLOB
unsetopt correct
# sort by chronology, not alphabet
git config --global branch.sort -committerdate

### History
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
setopt EXTENDED_HISTORY
HISTCONTROL=ignoreboth
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
HISTIGNORE="ls:ll:pwd:bg:fg:history"
bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward
HISTSIZE=20000
export SAVEHIST=$HISTSIZE
echo "setup settings"

### Utils
alias cat="bat"
alias ls="eza"
alias sudo='sudo '
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
alias vie="emacs -Q -nw"
alias vim="vi"
alias hgrep="history 0 | grep"

function spruce_up {
    /bin/rm -rf ~/.Trash/*;
    rm ~/Desktop/Screen\ Shots/*;
    brew cleanup &&
    brew update &&
    brew upgrade &&
    brew cleanup &&
    sudo shutdown -r now
}

function lc {
    fc -ln -1 | awk '{$1=$1}1' | pbcopy
}

function jjq {
    jq -R -r "${1:-.} as \$line | try fromjson catch \$line"
}

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

function p() {
    # if we're in a python project use uv run
    if [[ -f "package-lock.json" ]]; then
	# if command is "i" or "install"
	# we want to run npm install, not npm run install
	if [[ "$1" == "i" || "$1" == "install" ]]; then
	    npm install
	else
	    npm run "$@"
	fi
    elif [[ -f "yarn.lock" ]]; then
	if [[ "$1" == "i" || "$1" == "install" ]]; then
	    yarn
	else
            yarn "$@"
	fi
    elif [[ -f "pnpm-lock.yaml" ]]; then
        pnpm "$@"
    elif [[ -f "bun.lock" ]]; then
        bun "$@"
    elif [[ -f "requirements.txt" ]]; then
	uv run "$@"
    else
        echo "No recognized lock file found."
    fi
}

function sizeup {
    dir=$1
    dir="${dir:-$(pwd)}"
    du -hx "${dir}" | sort -n | tail -200
}

function porthog {
    PORT=$1
    lsof -iTCP -sTCP:LISTEN -P | grep "$PORT"
}

function gen_gif {
    in_file=$1
    in_base=$(basename "$in_file" | cut -d. -f1)
    ffmpeg -i "$in_file" -pix_fmt rgb24 -r 10 "$in_base.gif"
}

function git_branch_cleanup_dangerous {
    echo "This will delete all branches that are no longer on the remote."
    read -q "REPLY?Are you sure you want to continue? (y/n) "
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
	git fetch --prune
	git branch -vv | grep ': gone]' | awk '{print $1}' | xargs git branch -D
	echo "Cleanup complete."
    else
	echo "Aborting."
    fi
}

### Emacs
export EDITOR='emacsclient --create-frame --alternate-editor=""'
alias emc='emacsclient --no-wait --create-frame --alternate-editor=""'
alias emcnw='emacsclient -tty --create-frame --alternate-editor=""'
alias kille="emacsclient -e '(kill-emacs)'"
alias remc="kille;emc && exit"

function e {
    if [ "$#" -eq 0 ]; then
        echo "Starting new Emacs process ..." >&2
        emacs & disown
    elif emacsclient -n "$@" 2> /dev/null; then
        echo "Opened $* in Emacs server" >&2
    else
        echo "Opening $* in a new Emacs process ..." >&2
        emacs "$@" & disown
    fi
}
echo "setup utils"


echo "Loaded .zshrc"

# pnpm
export PNPM_HOME="/Users/nishantjha/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
export PATH="/opt/homebrew/opt/postgresql@16/bin:$PATH"
