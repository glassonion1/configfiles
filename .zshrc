# 環境変数
export LANG=ja_JP.UTF-8

# パス周りの設定
typeset -U path PATH
path=(
  /opt/homebrew/bin(N-/)
  /opt/homebrew/sbin(N-/)
  /usr/bin
  /usr/sbin
  /bin
  /sbin
  /usr/local/bin(N-/)
  /usr/local/sbin(N-/)
  /Library/Apple/usr/bin
)

export GOROOT=/usr/local/go
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH

# GPGの設定
export GPG_TTY=$(tty)

# opensslのパス
export PATH="/usr/local/opt/openssl/bin:$PATH"
# asdf関連のパス
export PATH="$HOME/.asdf/bin:$HOME/.asdf/shims:$PATH"

# pecoスクリプトの読み込み
for f (~/.zsh/*) source "${f}"

# 色を使用出来るようにする
autoload -Uz colors
colors

# cd したら自動的にpushdする
setopt auto_pushd
# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

# プロンプト
# 1行表示
# PROMPT="%~ %# "
# 2行表示
PROMPT="%{${fg[green]}%}[%n@%m]%{${reset_color}%} %~
%# "

########################################
# PROMPTテーマ
setopt prompt_subst #プロンプト表示する度に変数を展開

precmd () { 
  if [ -n "$(git status --short 2>/dev/null)" ];then
    export GIT_HAS_DIFF="✗"
    export GIT_NON_DIFF=""
  else 
    export GIT_HAS_DIFF=""
    export GIT_NON_DIFF="✔"
  fi
  # git管理されているか確認
  git status --porcelain >/dev/null 2>&1
  if [ $? -ne 0 ];then
    export GIT_HAS_DIFF=""
    export GIT_NON_DIFF=""
  fi
  export BRANCH_NAME=$(git branch --show-current 2>/dev/null)
}
# 末尾に空白をつけることで改行される
PROMPT=" 
%F{cyan}%~%f"
PROMPT=${PROMPT}'%F{green}  ${BRANCH_NAME} ${GIT_NON_DIFF}%F{red}${GIT_HAS_DIFF} 
%f$ '

# emacs 風キーバインドにする
bindkey -e

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# beep を無効にする
setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# Ctrl+Dでzshを終了しない
setopt ignore_eof

# '#' 以降をコメントとして扱う
setopt interactive_comments

# launch emacs on commandline
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

# データベース関連のパス設定(mysql & postgres)
export PATH="/usr/local/opt/mysql-client/bin:$PATH"
export PATH="/usr/local/opt/libpq/bin:$PATH"
# for M1 Mac
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/taisukefujita/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/taisukefujita/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/taisukefujita/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/taisukefujita/google-cloud-sdk/completion.zsh.inc'; fi
