. /etc/profile

autoload -U promptinit
promptinit

prompt_gentoo_setup () {
  prompt_gentoo_prompt=${1:-'blue'}
  prompt_gentoo_user=${2:-'green'}
  prompt_gentoo_root=${3:-'red'}

  if [ "$USER" = 'root' ]
  then
    base_prompt="%B%F{$prompt_gentoo_root}%m%k "
  else
    base_prompt="%B%F{$prompt_gentoo_user}%n@%m%k "
  fi
  post_prompt="%b%f%k"

  #setopt noxtrace localoptions

  path_prompt="%B%F{$prompt_gentoo_prompt}%1~"
  PS1="$base_prompt$path_prompt %# $post_prompt"
  PS2="$base_prompt$path_prompt %_> $post_prompt"
  PS3="$base_prompt$path_prompt ?# $post_prompt"
}

case "$(hostname)" in
    'tantalus')
        PROMPTCOLOR="blue";
        ;;
    'hermes.local')
        PROMPTCOLOR="green";
        ;;
esac

prompt_gentoo_setup $PROMPTCOLOR

if [ "$(uname)" = "Linux" ]; then
    export BROWSER="firefox"
    export PATH="${PATH}:/home/steven/scripts/"
elif [ "$(uname)" = "Darwin" ]; then
    export PATH="${PATH}:/opt/local/bin"
    export CLICOLOR=1
fi

export EDITOR="vim"
setopt autocd
setopt AUTO_CONTINUE
unsetopt nomatch

if [ $TERM = "xterm" ]; then
    precmd() { print -Pn "\e]0;%m:%~\a" }
    preexec () { print -Pn "\e]0;$1\a" }
fi

# half-bashify zsh
HISTSIZE=200
if [ `whoami` == "root" ]; then
    HISTFILE=/root/.zsh_history
else
    HISTFILE=~/.zsh_history
fi
SAVEHIST=200
bindkey '\e[1~' beginning-of-line
bindkey '\eOH'  beginning-of-line
bindkey '\e[H'  beginning-of-line
bindkey '^A'    beginning-of-line
bindkey '\e[4~' end-of-line
bindkey '\eOF'  end-of-line
bindkey '\e[F'  end-of-line
bindkey '^E'    end-of-line
bindkey '\e[3~' delete-char

source ~/.aliases

# set up agent.  horribly dangerous, but screw it.
source ~/.ssh-agent-info > /dev/null
if test -n "$(ssh-add -L 2>&1 | grep 'Could not open a connection')"; then
    ssh-agent >~/.ssh-agent-info
    source ~/.ssh-agent-info > /dev/null
fi

# The following lines were added by compinstall
zstyle :compinstall filename "$HOME/.zshrc"
zstyle ':completion:*' list-colors ''

autoload -Uz compinit
compinit
# End of lines added by compinstall

