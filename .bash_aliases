#!/bin/bash

# ~/.bash_aliases

# Enable color support of ls and also add handy aliases.
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto --classify'

  alias grep='grep --color=auto'
  # alias fgrep='fgrep --color=auto'
  # alias egrep='egrep --color=auto'
fi

alias ds9='ds9 -zscale'

alias less='less -S'

alias l='ls -CF'
alias la='ls -A'
alias ll='ls -lh'

alias open='xdg-open'

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
