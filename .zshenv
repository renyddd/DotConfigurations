export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="$PATH:$HOME/.local/bin"

# golang
export GOPATH="$HOME"/go
export GOROOT=/usr/local/go
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/bin"

# rust
# https://www.rust-lang.org/zh-CN/tools/install
export PATH="$PATH:$HOME/.cargo/bin"
. "$HOME/.cargo/env"


export PATH="$PATH:$HOME/.roswell/bin"
