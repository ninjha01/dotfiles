#Aesthetics
export PS1="/\[\e[31m\]\W\[\e[m\]/: ";
export TERM=xterm-256color;

#Aliases
alias up="cd ..";
alias 2up="cd ../../";
alias rm="trash";
alias loginlab="bash ~/scripts/loginlab.sh";
alias upload="bash  ~/scripts/upload.sh";
alias edit="bash  ~/scripts/edit.sh";
alias zipup="bash  ~/scripts/zipup.sh";
alias backup="bash  ~/scripts/backup.sh";
alias getlab="bash  ~/scripts/getlab.sh";
alias proj="bash ~/scripts/project.sh";
alias blacklist="sudo emacs /etc/hosts";
alias cleanup="bash ~/scripts/cleanup.sh";
alias saturn="bash ~/scripts/saturn.sh";
alias d2h="bash ~/scripts/d2h.sh";
alias pdflatex="/Library/TeX/texbin/pdflatex";
alias studio="bash ~/scripts/studio.sh";
alias drive="bash ~/scripts/drive.sh";
alias adb="bash ~/scripts/adb.sh";
alias la="ls -a";
alias nj7kv="cd /Volumes/nj7kv/public_html";
alias emacs="/usr/local/Cellar/emacs/25.2/bin/emacs";
alias concatpdf="/System/Library/Automator/Combine PDF Pages.action/Contents/Resources/join.py";
# Expanding History Capabilities
export HISTSIZE=10000;
shopt -s histappend;
PROMPT_COMMAND="history -a;$PROMPT_COMMAND";

# Setting PATH for Python 3.5
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}";
export PATH;

#Gradle stuff
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
