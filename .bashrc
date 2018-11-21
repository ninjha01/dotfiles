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
export PS1="[\t][\e[31m\]\W\[\e[m\]]: "

# Auth ############################################################
export SSO_USER='ERS611'
function getpwd {
    export SSO_PWD=`security find-generic-password -wa $(whoami)`
}
#alias awstokens='$HOME/SecurityTokenScript-MacOS-v1.2.9'

# Utils ############################################################
alias rm="trash";
alias blacklist="sudo emacs /etc/hosts";
alias la="ls -a";
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
alias emi="emacs --daemon";
alias emc="emacsclient -c -a \"\"";
alias emcnw="emacsclient -nw -a \"\"";
alias kille="emacsclient -e '(kill-emacs)'"

# Proxy Stuff #####################################################
alias con="cofproxy on"
alias coff="cofproxy off"
export HTTP_PROXY="http://vdcproxy.kdc.capitalone.com:8099"
cofproxy on > /dev/null

# Expanding History Capabilities ##################################
export HISTSIZE=10000;
shopt -s histappend;
PROMPT_COMMAND="history -a;$PROMPT_COMMAND";

# Path Exports ####################################################
export NVM_DIR="$HOME/.nvm" 
. "/usr/local/opt/nvm/nvm.sh";

# Add Home Bin to Path
export PATH="$HOME/bin:$PATH"

# Spark ###########################################################
export SPARK_HOME=/usr/local/Cellar/apache-spark/2.3.1/libexec
export PYTHONPATH=/usr/local/Cellar/apache-spark/2.3.1/libexec/python/:$PYTHONP$

# Hadoop ##########################################################
alias hstart="/usr/local/Cellar/hadoop/3.1.1/sbin/start-dfs.sh;/usr/local/Cellar/hadoop/3.1.1/sbin/start-yarn.sh"
alias hstop="/usr/local/Cellar/hadoop/3.1.1/sbin/stop-yarn.sh;/usr/local/Cellar/hadoop/3.1.1/sbin/stop-dfs.sh"
export HADOOP_INSTALL=/usr/local/Cellar/hadoop/3.1.0
export HADOOP_CONF_DIR=$HADOOP_INSTALL/libexec/etc/hadoop

export PATH="/usr/local/sbin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
