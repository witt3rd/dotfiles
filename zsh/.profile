# >>> doom emacs
export EMACSDIR="$HOME/doom-emacs"
export DOOMDIR="$HOME/.doom.d"
export DOOMLOCALDIR="$HOME/.doom.d.local"
# <<< doom emacs

# >>> python
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
# <<< python

# >>> go
export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin
# <<< go

# >>> rust
. /home/donald/.cargo/env
# <<< rust

# >>> tex
export PATH=/usr/local/texlive/2021/bin/x86_64-linux:$PATH
# <<< tex
