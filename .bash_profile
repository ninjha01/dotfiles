###################################################################
##                                                               ##
##                Nishant's Bash Configuration                   ##
##	              There are many like it                     ##
##	               But this one is mine                      ##
##                                                               ##
###################################################################

# Aesthetics ######################################################
export PS1="/\[\e[31m\]\W\[\e[m\]/: ";
export TERM=xterm-256color;
###################################################################

# Aliases #########################################################
alias rm="trash";
alias backup="bash  ~/scripts/backup.sh";
alias blacklist="sudo emacs /etc/hosts";
alias la="ls -a";
alias cleanup="bash ~/scripts/cleanup.sh";
alias emi="emacs --daemon";
alias emc="emacsclient -c -a \"\"";
alias kille="emacsclient -e '(kill-emacs)'"
###################################################################

# Expanding History Capabilities ##################################
export HISTSIZE=10000;
shopt -s histappend;
PROMPT_COMMAND="history -a;$PROMPT_COMMAND";
###################################################################
