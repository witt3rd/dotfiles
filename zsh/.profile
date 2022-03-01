# >>> doom emacs
export EMACSDIR="$HOME/doom-emacs"
export DOOMDIR="$HOME/.doom.d"
export DOOMLOCALDIR="$HOME/.doom.d.local"
# <<< doom emacs

# >>> PYTHON
export PATH="$(python3 -m site --user-base)/bin:${PATH}"
export PATH="$HOME/.poetry/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

# <<< PYTHON

# >>> go
export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin
# <<< go

# >>> rust
[[ ! -f $HOME/.cargo/env ]] || source $HOME/.cargo/env
# <<< rust

# >>> tex
export PATH=/usr/local/texlive/2021/bin/x86_64-linux:$PATH
# <<< tex
