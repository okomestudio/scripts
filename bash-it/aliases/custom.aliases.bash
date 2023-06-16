# -*- mode: sh; sh-basic-offset: 2; -*-

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


# fd: Faster find
alias fd='fdfind'


# markdown viewer on shell
function mdviewer() {
  pandoc "$1" | lynx -stdin;
}


# awsume
if pyenv which awsume > /dev/null 2>&1 ; then
  alias awsume='. $(pyenv which awsume)'
fi


# cd: Override by sourcing .bash_local if exists in the directory.
function cd() {
  command cd "$@" || return
  local bash_local="$PWD/.bash_local"
  if [[ -f $bash_local ]]; then
    read -p 'Load directory-local Bash config? [Y/n]: ' resp
    resp=${resp:-y}
    if [[ $resp == "Y" ]] || [[ $resp == "y" ]]; then
      # shellckeck source=/dev/null
      source $bash_local
      local msg="Sourced $bash_local"
      [[ ! -z "$use_color" ]] && msg="\033[0;32m${msg}\033[0m"
      >&2 echo -e "$msg"
    fi
  fi
}
