unset LC_ALL
export LANG="en_US.UTF-8"
export LC_COLLATE="C"

_add_to_path () {
  if [ -z "$(echo $PATH | grep $1)" ]; then
    export PATH="$1:$PATH"
  fi
}

_add_to_path "$HOME/.scripts"
_add_to_path "$HOME/.cabal/bin"
_add_to_path "/usr/NX/bin"
_add_to_path "$HOME/src/android-sdk-linux_86/tools"
_add_to_path "$HOME/src/android-sdk-linux_86/platform-tools"

export PYTHONDONTWRITEBYTECODE=1
export EDITOR="vim"
export BROWSER="google-chrome"
export GNOME_DISABLE_CRASH_DIALOG=1
export GTK_IM_MODULE="xim"
