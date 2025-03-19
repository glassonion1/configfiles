;; デフォルト文字コード
(prefer-coding-system 'utf-8)
;; キー
(global-set-key "\C-h" 'delete-backward-char)
;; MacでGUIの時、optionをmeta
(if window-system (progn
                    (when (equal system-type 'darwin)
                      (setq mac-option-modifier 'meta))
                    ))

(setq-default indent-tabs-mode nil)

;; 末尾スペースの可視化
(setq-default show-trailing-whitespace t)

(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; ls does not support --diredの対策
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; ファイルの設定
(setq delete-auto-save-files t)
(setq backup-inhibited t)
;; Clipboard
(setq x-select-enable-clipboard t)

;; straight.el(パッケージマネージャ)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; インストールするパッケージのリスト
(defvar my/favorite-packages
  '(
    exec-path-from-shell
    dracula-theme
    solarized-theme
    material-theme
    doom-themes
    ))
;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (straight-use-package package)))

;; シェルに設定されている環境変数を引き継ぐ
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;(setq exec-path-from-shell-check-startup-files nil)

;; 濁点分離問題
(use-package ucs-normalize
  :config
  (setq file-name-coding-system 'utf-8-hfs)
  (setq local-coding-system 'utf-8-hfs))

;; テーマと色
;(load-theme 'material t)
(load-theme 'doom-dracula t)
;(load-theme 'dracula t)

;; 全角スペース タブ trailing-spacesを目立たせる
(use-package whitespace
  :config
  (setq whitespace-style
        '(tabs tab-mark spaces space-mark trailing))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (set-face-foreground 'whitespace-space "LightSlateGray")
  (set-face-background 'whitespace-space "DarkSlateGray")
  (set-face-foreground 'whitespace-tab "LightSlateGray")
  (set-face-background 'whitespace-tab "DarkSlateGray"))
(global-whitespace-mode 1)

;; neotree
(use-package neotree
  :straight t
  :init
  (setq-default neo-keymap-style 'concise)
  :config
  (setq neo-show-hidden-files t)
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (bind-key [f8] 'neotree-toggle)
  )

;; lsp-mode
;; プロジェクトルートで M-x lsp-workspace-folder-add を実行すること
;; lsp-restart-workspaceでリスタートできる
(use-package lsp-mode
  :straight t
  :hook
  ((rust-mode . lsp)
   (go-mode . lsp)
   (python-ts-mode . lsp)
   (tsx-ts-mode . lsp)
   )
  :config
  (setq-default lsp-enable-snippet nil)
  (setq-default lsp-eldoc-render-all nil)
  :custom
  (lsp-rust-server 'rls)
  :commands lsp)

;; flycheck
(use-package flycheck
  :straight t)

;; 入力補完
(use-package corfu
  :straight (corfu :type git
                   :host github
                   :repo "minad/corfu"
                   :branch "async"
                   :files (:defaults "extensions/*"))
  :custom ((corfu-auto t)
           (corfu-auto-delay 0)
           (corfu-auto-prefix 1)
           (corfu-cycle t)
           (corfu-on-exact-match nil)
           (corfu-popupinfo-delay 0)
           (tab-always-indent 'complete))
  :init
  (global-corfu-mode +1)
  (corfu-popupinfo-mode +1)
  :config
  ;; lsp-modeでcorfuが起動するように設定する
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none)))

(use-package kind-icon
  :straight t
  :after corfu
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eldoc-box
  :straight t
  :after (:all eldoc)
  )

;; Treesitの設定
(setq treesit-language-source-alist
      '((json "https://github.com/tree-sitter/tree-sitter-json")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        ;(go "https://github.com/tree-sitter/tree-sitter-go")
        ;(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        ))

(dolist (element treesit-language-source-alist)
  (let* ((lang (car element)))
    (if (treesit-language-available-p lang)
        (message "tree-sistter: %s is already installed" lang)
      (message "tree-sitter: %s is not installed" lang)
      (treesit-install-language-grammar lang))))
(use-package treesit
  :config
  (setq treesit-font-lock-level 4))

;; フォーマッターの設定
(use-package reformatter
  :straight t
  :config
  (reformatter-define go-format
    :program "goimports")
  (reformatter-define web-format
    :program "prettier"
    :args `("--write" "--stdin-filepath" ,buffer-file-name))
  (reformatter-define python-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name))
  :hook
  (go-mode . go-format-on-save-mode)
  (tsx-ts-mode . web-format-on-save-mode)
  (json-ts-mode . web-format-on-save-mode)
  (graphql-mode . web-format-on-save-mode)
  (python-ts-mode . python-format-on-save-mode))

;; go-mode go-ts-modeはインデントがいまいちなので導入見送り
(use-package go-mode
  :straight t
  :mode
  ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq c-basic-offset 4)
              (setq tab-width 4))))

;; python-ts-mode
(use-package python-ts-mode
  :mode ("\\.py\\'" . python-ts-mode))

;; json-ts-mode
(use-package json-ts-mode
  :mode
  ("\\.json\\'" . json-ts-mode)
  :config
  (add-hook 'json-ts-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq tab-width 2)
              (setq js-indent-level 2))))

;; tsx-ts-mode
(use-package tsx-ts-mode
  :mode (("\\.ts\\'" . tsx-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.mts\\'" . tsx-ts-mode)
         ("\\.js\\'" . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.[mc]js$" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

;; work around ts-ls bug
(advice-add 'json-parse-buffer :around
              (lambda (orig &rest rest)
                (while (re-search-forward "\\u0000" nil t)
                  (replace-match ""))
                (apply orig rest)))

;; graphql-mode
(use-package graphql-mode
  :straight t
  :mode
  ("\\.graphql\\'" . graphql-mode)
  ("\\.graphqls\\'" . graphql-mode)
  )

;; rust-mode
(use-package rust-mode
  :straight t
  :custom
  (rust-format-on-save t))
(use-package cargo
  :straight t
  :hook
  (rust-mode . cargo-minor-mode))

;; terraform-mode
(use-package terraform-mode
  :straight t
  :mode
  ("\\.tf\\'" . terraform-mode)
  :hook
  (terraform-mode . terraform-format-on-save-mode))

;; yaml-mode
(use-package yaml-ts-mode
  :straight t
  :mode
  ("\\.yml\\'" . yaml-ts-mode)
  ("\\.yaml\\'" . yaml-ts-mode)
  :config
  (define-key yaml-ts-mode-map "\C-m" 'newline-and-indent))

;; protobuf-mode
(use-package protobuf-mode
  :straight t
  :init
  :mode
  ("\\.proto\\'" . protobuf-mode)
  :config
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (setq c-basic-offset 2)
              ))
  )

;; docker-mode
(use-package dockerfile-mode
  :straight t
  :mode
  ("Dockerfile\\'" . dockerfile-mode))
