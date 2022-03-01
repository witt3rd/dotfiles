# >>> doom emacs
export EMACSDIR="$HOME/doom-emacs"
export DOOMDIR="$HOME/.doom.d"
export DOOMLOCALDIR="$HOME/.doom.d.local"
# <<< doom emacs

# >>> PYTHON
export PATH="$(python3 -m site --user-base)/bin:${PATH}"
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

export PATH="$HOME/.poetry/bin:$PATH"
