if [ "`pwd`" = '/' ]; then
    cd
fi

# Reload PATH (probably blown away by /etc/zsh/zprofile)
source $HOME/.zshenv

prompt_gentoo_setup () {
    prompt_gentoo_prompt=${1:-'blue'}
    prompt_gentoo_user=${2:-'green'}
    prompt_gentoo_root=${3:-'red'}

    if [ "$USER" = 'root' ]
    then
        base_prompt="%B%F{$prompt_gentoo_root}%m%k"
    else
        base_prompt="%B%F{$prompt_gentoo_user}%n@%m%k"
    fi
    post_prompt="%b%f%k"

    setopt noxtrace localoptions
    path_prompt="%B%F{$prompt_gentoo_prompt}%T %3~"
    PROMPT="$base_prompt $path_prompt %# $post_prompt"
}

if [ -n "$SSH_CLIENT" ]; then
    prompt_gentoo_setup gray
    export _CMD_HOST="$(hostname -s):"
    if [ -z "$DISPLAY" ]; then
        # Set up for xpra automatically
        export DISPLAY=:1
    fi
else
    prompt_gentoo_setup blue
fi

case $TERM in
    xterm*)
        precmd() { print -Pn "\e]0;%m:%2~\a" }
        preexec() { print -Pn "\e]0;$_CMD_HOST$1\a" };;
    screen*)
        precmd() { print -Pn '\ek%m:%2~\e\\' }
        preexec() { print -Pn "\\ek$_CMD_HOST$1\\e\\\\" };;
esac


if [ "$(uname)" = "Linux" ]; then
    export BROWSER="firefox"
    if [ -x $(which dircolors) ]; then
        eval $( dircolors -b )
        alias ls='ls --color=auto'
    fi
elif [ "$(uname)" = "Darwin" ]; then
    _add_to_path "/opt/local/bin"
    if which gls 2>&1 > /dev/null ; then
        alias ls='gls --color=auto'
    fi
    source ${HOME}/.profile
    zstyle ':completion:*:users' users steven strobe zenia root
fi

setopt AUTO_CONTINUE
unsetopt nomatch

if [ `whoami` == "root" ]; then
    HISTFILE=/root/.zsh_history
else
    HISTFILE=~/.zsh_history
fi
HISTSIZE=100000
SAVEHIST=100000
setopt HIST_IGNORE_DUPS APPEND_HISTORY HIST_EXPIRE_DUPS_FIRST HIST_FIND_NO_DUPS
setopt SHARE_HISTORY

bindkey -e
bindkey '\e[1~' beginning-of-line
bindkey '\e[7~' beginning-of-line
bindkey '\eOH'  beginning-of-line
bindkey '\e[H'  beginning-of-line
bindkey '^A'    beginning-of-line
bindkey '\e[4~' end-of-line
bindkey '\e[8~' end-of-line
bindkey '\eOF'  end-of-line
bindkey '\e[F'  end-of-line
bindkey '^E'    end-of-line
bindkey '\e[3~' delete-char
bindkey '^?'    backward-delete-char

# set up agent.  horribly dangerous, but screw it.
if [ ! -f "$SSH_AUTH_SOCK" -a -z "$SSH_CLIENT" ]; then
    test -f ~/.ssh-agent-info && source ~/.ssh-agent-info > /dev/null
    if test -n "$(ssh-add -L 2>&1 | grep 'Could not open a connection')"; then
        ssh-agent >~/.ssh-agent-info
        source ~/.ssh-agent-info > /dev/null
    fi
fi

# From Gentoo-wiki. Ironically, this is only needed on Arch.
#local _myhosts
#_myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
#zstyle ':completion:*' hosts $_myhosts


# The following lines were added by compinstall

zstyle ':completion:*' cache-path ~/.zsh_cache
zstyle ':completion:*' completer _list _complete _ignored _approximate
zstyle ':completion:*' condition 0
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort name
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' menu select=3
zstyle ':completion:*' preserve-prefix '//[^/]##/'
#zstyle ':completion:*' rehash true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-cache on
zstyle ':completion:*' verbose true
zstyle :compinstall filename $HOME/.zshrc
fpath=(~/.zsh_functions $fpath)

autoload -Uz compinit
compinit -i
# End of lines added by compinstall

alias dc=cd
alias grep='grep --color'
alias donemail='echo done | mail -s done steven@strobe.cc 8137287254@vtext.com'
alias unrarx='unrar x -kb -o+'
if [ ! "$(uname)" = "Darwin" ]; then
    alias open='xdg-open'
fi

# Lines configured by zsh-newuser-install
setopt appendhistory extendedglob notify
unsetopt no_case_glob
unsetopt beep nomatch
bindkey -e
# End of lines configured by zsh-newuser-install

export PYTHONDONTWRITEBYTECODE=1
