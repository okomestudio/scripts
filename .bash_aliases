#!/bin/bash

# ~/.bash_aliases

HOST=`hostname -s`

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto --classify'

  alias grep='grep --color=auto'
  #alias fgrep='fgrep --color=auto'
  #alias egrep='egrep --color=auto'
fi

alias ll='ls -lh'
alias la='ls -A'
alias l='ls -CF'

alias ds9='ds9 -zscale'

alias co='git checkout'
alias log='git log'
alias pull='git pull'
alias push='git push'
alias st='git status'
