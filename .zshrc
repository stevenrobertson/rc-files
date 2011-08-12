if [ "`pwd`" = '/' ]; then
    cd
fi

source /etc/profile

unset LC_ALL
export LANG="en_US.UTF-8"
export LC_COLLATE="C"

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
else
    prompt_gentoo_setup blue
fi

if [ $TERM = "xterm" -o $TERM = "xterm-color" ]; then
    precmd() { print -Pn "\e]0;%m:%~\a" }
    preexec () { print -Pn "\e]0;$1\a" }
fi

if [ "$(uname)" = "Linux" ]; then
    export BROWSER="firefox"
    if [ -x $(which dircolors) ]; then
        eval $( dircolors -b )
        alias ls='ls --color=auto'
    fi
elif [ "$(uname)" = "Darwin" ]; then
    export PATH="${PATH}:/opt/local/bin"
    if which gls 2>&1 > /dev/null ; then
        alias ls='gls --color=auto'
    fi
    source ${HOME}/.profile
    zstyle ':completion:*:users' users steven strobe zenia root
fi

add_to_path () {
  if [ -z "$(echo $PATH | grep $1)" ]; then
    export PATH="$1:$PATH"
  fi
}

add_to_path "$HOME/.scripts"
add_to_path "$HOME/.cabal/bin"
add_to_path "/usr/NX/bin"

export EDITOR="vim"
setopt AUTO_CONTINUE
unsetopt nomatch

HISTSIZE=2000
if [ `whoami` == "root" ]; then
    HISTFILE=/root/.zsh_history
else
    HISTFILE=~/.zsh_history
fi
SAVEHIST=2000
setopt HIST_IGNORE_DUPS

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
if [ -z "$SSH_AUTH_SOCK" ]; then
    test -f ~/.ssh/id_dsa && test -f ~/.ssh-agent-info && source ~/.ssh-agent-info > /dev/null
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
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=3
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-cache on
zstyle ':completion:*' rehash true
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/steven/.zshrc'

fpath=(~/.zsh_functions $fpath)
autoload -Uz compinit
compinit -i
# End of lines added by compinstall

alias mq='hg -R $(hg root)/.hg/patches'
#alias vm='kvm -m 1024 -usb -usbdevice tablet -soundhw ac97 -vga std'
alias js='jackd -R -d freebob -r 44100'
alias burn360iso='growisofs -use-the-force-luke=dao -use-the-force-luke=break:1913760  -dvd-compat -speed=4'
alias trivm="kvm -m 1700 -hda /dev/sda -smp 2 -vga std -usb -usbdevice tablet -net nic,vlan=0 -net tap,vlan=0,ifname=local0 -localtime"
alias grep='grep --color'
alias bopen='source `which _bopen.sh`'
alias donemail='echo done | mail -s done steven@strobe.cc 8137287254@vtext.com'
alias unrarx='unrar x -kb -o+'

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory extendedglob notify
unsetopt no_case_glob
unsetopt beep nomatch
bindkey -e
# End of lines configured by zsh-newuser-install

export GNOME_DISABLE_CRASH_DIALOG=1

