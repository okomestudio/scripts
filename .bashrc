# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
  xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes
if [ -n "$force_color_prompt" ]; then
  if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	  # We have color support; assume it's compliant with Ecma-48
	  # (ISO/IEC-6429). (Lack of such support is extremely rare, and
	  # such a case would tend to support setf rather than setaf.)
	  color_prompt=yes
  else
	  color_prompt=
  fi
fi
if [ "$color_prompt" = yes ]; then
  PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
  PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
  xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
  *)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Alias and completion for Git.
if [[ -e /usr/share/bash-completion/completions/git ]]; then
  . /usr/share/bash-completion/completions/git
  __git_complete g __git_main
  __git_complete gco _git_checkout
  alias g='git'
  alias gco='g checkout'
  alias gst='g status'
  # Want git push/pull aliased as well, but completion does not appear
  # to work with <repo> [<refspec>] CLI.
fi

# Update fancy prompt
PS1='[\u@\h \W]\$ '

# For remote shell over SSH.
if [[ -n "$SSH_CLIENT" ]]; then
  PS1='\[\e[0;31m\][\u@\h \W]$\[\e[m\] '
fi

# When in a git repo, show the branch name.
if [[ -e /etc/bash_completion.d/git-prompt ]]; then
  . /etc/bash_completion.d/git-prompt
  PS1='[\u@\h \W$(__git_ps1 "(%s)")]\$ '
fi

export PS1

##############################################################################
# Setting PATH: Any path that needs to be set in non-login shell needs
# to be in ~/.bashrc.  Others can go in ~/.bash_profile.

addpath() {
  if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    PATH="$1:$PATH"
  fi
}

addpythonpath() {
  if [ -d "$1" ] && [[ ":$PYTHONPATH:" != *":$1:"* ]]; then
    PYTHONPATH="$1:$PYTHONPATH"
  fi
}

# Add to PATH.
addpath /iraf/bin
addpath /usr/local/stsci_python/bin
addpath ~/bin
addpath ~/lib/aws-emr
addpath ~/.arc_install/arcanist/bin
export PATH

# Add to PYTHONPATH.
addpythonpath /usr/local/stsci_python/lib/python
addpythonpath /usr/local/pyds9/lib/python2.7/site-packages
addpythonpath /usr/local/astLib/lib/python2.7/site-packages
addpythonpath ~/lib/python2.7/site-packages
addpythonpath /astro/DEEP2Wind/lib/python2.7/site-packages
export PYTHONPATH

#export LS_COLORS='ex=00;35:ln=00;32'
export LS_COLORS="${LS_COLORS}:ex=32"
export LC_COLLATE=C

# Misc
export BROWSER=google-chrome
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"
export PAGER=less

# Add syntax highlighting via Pygments (do: pip install Pygments)
export LESS='-R'
export LESSOPEN='|~/.lessfilter %s'

# Lahman database
export LAHMANDB=/usr/local/lahman/2013

# for encoding used by python for stdin/stdout/stderr
export PYTHONIOENCODING=utf-8

# for Hadoop
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export HADOOP_INSTALL=/usr/local/hadoop
export HADOOP_HOME=$HADOOP_INSTALL
export PATH=$PATH:$HADOOP_INSTALL/bin
export PATH=$PATH:$HADOOP_INSTALL/sbin
export HADOOP_MAPRED_HOME=$HADOOP_INSTALL
export HADOOP_COMMON_HOME=$HADOOP_INSTALL
export HADOOP_HDFS_HOME=$HADOOP_INSTALL
export YARN_HOME=$HADOOP_INSTALL
export HADOOP_COMMON_LIB_NATIVE_DIR=$HADOOP_INSTALL/lib/native
export HADOOP_OPTS="-Djava.library.path=$HADOOP_INSTALL/lib"

# Spark
export SPARK_HOME=/usr/local/spark
