#Aesthetics
export PS1="/\[\e[31m\]\W\[\e[m\]/: ";
export TERM=xterm-256color;

#Aliases
alias rm="trash";
alias backup="bash  ~/scripts/backup.sh";
alias blacklist="sudo emacs /etc/hosts";
alias la="ls -a";
alias cleanup="bash ~/scripts/cleanup.sh";

# Expanding History Capabilities
export HISTSIZE=10000;
shopt -s histappend;
PROMPT_COMMAND="history -a;$PROMPT_COMMAND";

## macOS Specific #############################################

### Application renames #####
alias pymol="/Applications/PyMOL.app/Contents/bin/pymol";
alias pdflatex="/Library/TeX/texbin/pdflatex";
alias adb="~/platform-tools/adb shell;"
alias emacs="/usr/local/Cellar/emacs/25.2/bin/emacs";
alias concatpdf="/System/Library/Automator/Combine PDF Pages.action/Contents/Resources/join.py";


### Utilities ###############
alias nj7kv="cd /Volumes/nj7kv/public_html";
alais saturn="cd ~/Google\ Drive/Projects/Saturn/; gradle -q run;"
#############################

### PATH Exports ##############################################

#Gradle 
export GRADLE_HOME=/Users/Nishant/gradle-3.2.1/;
export PATH=~/gradle-3.2.1/bin/:$PATH;

# MacPorts Installer addition on 2017-05-06_at_20:22:09: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH";
# Finished adapting your PATH environment variable for use with MacPorts.

#Adding pdflatex to path
export PATH="/Users/Nishant/Library/TeXShop/bin:$PATH";

# added by Anaconda3 installer
export PATH="/Users/Nishant/anaconda3/bin:$PATH";
export RBENV_ROOT=/usr/local/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Setting PATH for Python 3.5
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}";
export PATH;
################################################################


###############################################################
