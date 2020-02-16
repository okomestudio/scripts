#!/usr/bin/env bash

if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
	use_color=t
fi


# grep
if [[ ! -z "$use_color" ]]; then
  alias grep='grep --color=auto'
  export GREP_COLOR=''
fi


# ls: Enable color support of ls and also add handy aliases.
if [[ ! -z "$use_color" ]] && [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors \
    && eval "$(dircolors -b ~/.dircolors)" \
      || eval "$(dircolors -b)"

  alias ls='ls --color=auto --classify'
else
  alias ls='ls --classify'
fi

alias ll='ls -alh'


# open
alias open='xdg-open'


# cd: Override to source local .bashrc if exists.
function cd() {
  command cd "$@" || return
  local bashrc="$PWD/.bashrc"
  if [[ $PWD != $HOME && -f $bashrc ]]; then
    read -p 'Load directory-specific Bash config? [Y/n]: ' resp
    resp=${resp:-y}
    if [[ $resp == "Y" ]] || [[ $resp == "y" ]]; then
      source $bashrc
      msg="Sourced $bashrc"
      [[ ! -z "$use_color" ]] && msg="\033[0;32m${msg}\033[0m"
      >&2 echo -e "$msg"
    fi
  fi
}
