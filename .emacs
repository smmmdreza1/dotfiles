(package-initialize)

;; fixing cursor problem windows.
(setq w32-use-visible-system-caret nil)

;;; seting emacs default directory
(setq default-directory "C:/Users/PC-Safari/")

;;; Emacs Load Path
(add-to-list 'load-path "~/.emacs.local")

(load "~/.emacs.rc/rc.el")

(load "~/.emacs.rc/misc-rc.el")
(load "~/.emacs.rc/org-mode-rc.el")
(load "~/.emacs.rc/autocommit-rc.el")

(add-to-list 'custom-theme-load-path
             "~/.emacs.d/themes")


;;; =============================================
;;FONTS:
(add-to-list 'default-frame-alist `(font . "Ubuntu Mono-14"))

;; font for all unicode characters
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;; override font for cyrillic characters
(set-fontset-font t 'cyrillic "Droid Sans Mono Regular")


;;; =============================================
;;GRAPHICAL UI:
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(column-number-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode 1)
(global-hl-line-mode 1)

(setq visible-bell t)

(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(rc/require-theme 'gruber-darker)
;; (rc/require-theme 'zenburn)
;; (load-theme 'adwaita t)

(eval-after-load 'zenburn
  (set-face-attribute 'line-number nil :inherit 'default))


;;; =============================================
;;; Packages conficurations:

;;; ido, smex
(rc/require 'smex 'ido-completing-read+)

(require 'ido-completing-read+)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)


;;; ------------------------------------------
;; rainbow-delimiters
;;  - rainbow-delimiters is a "rainbow parentheses"
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;; ------------------------------------------
;; rainbow-mdoe
(use-package rainbow-mode
  :ensure t)

(rainbow-mode 1)


;;; ------------------------------------------
;; which key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 4))


;;; ------------------------------------------
;;; ivy
(use-package ivy
  :defer 0
  :diminish
  :config
  (ivy-mode 1))


;;; ------------------------------------------
;; counsel
(use-package counsel
  :bind (("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)))


;;; ------------------------------------------
;; helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;;; ------------------------------------------
;;; Paredit
(use-package paredit
  :ensure t)

(defun rc/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook  'rc/turn-on-paredit)
(add-hook 'clojure-mode-hook     'rc/turn-on-paredit)
(add-hook 'lisp-mode-hook        'rc/turn-on-paredit)
(add-hook 'common-lisp-mode-hook 'rc/turn-on-paredit)
(add-hook 'scheme-mode-hook      'rc/turn-on-paredit)
(add-hook 'racket-mode-hook      'rc/turn-on-paredit)


;;; ------------------------------------------
;;; c-mode, basm-mode, fasm-mode, porth-mode, noq-mode, jai-mode, simpc-mode
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))


(require 'basm-mode)

(require 'fasm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . fasm-mode))

(require 'porth-mode)

(require 'noq-mode)

(require 'jai-mode)

(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))


;;; ------------------------------------------
;;; Whitespace mode
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace ))

;; Define the whitespace style.
(setq-default whitespace-style
              '(face spaces tabs newline space-mark tab-mark))

(add-hook 'tuareg-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'simpc-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'java-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'scala-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'haskell-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'erlang-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'asm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'fasm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'nim-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'porth-mode-hook 'rc/set-up-whitespace-handling)


;;; ------------------------------------------
;;; magit
;; magit requres this lib, but it is not installed automatically on
;; Windows.
;(rc/require 'cl-lib)
(rc/require 'magit)

(setq magit-auto-revert-mode nil)


;;; ------------------------------------------
;;; multiple cursors
(rc/require 'multiple-cursors)


;;; ------------------------------------------
;;; dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

(rc/require 'dired-single)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
         loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse) 
  (define-key dired-mode-map "^"
              (function
               (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(rc/require 'dired-hide-dotfiles)

;; To toggle hiding
(define-key dired-mode-map "H" #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook  #'dired-hide-dotfiles-mode)


;;; ------------------------------------------
;;; helm
(rc/require 'helm 'helm-git-grep 'helm-ls-git 'helm-org)
(setq helm-ff-transformer-show-only-basename nil)

;;; ------------------------------------------
;;; eglot
(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("<f6>"    . eglot-format-buffer)
              ("C-c r"   . eglot-rename)
              ("C-c C-d" . rust-dbg-wrap-or-unwrap)
              ("C-c d"   . eldoc)
              ("C-c a"   . eglot-code-actions)))


;;; ------------------------------------------
;;; rust-mode
(use-package rust-mode
  :ensure t
  :hook ((rust-mode . eglot-ensure)
         (rust-mode . company-mode)
         (rust-mode . flycheck-mode))
  ;; :bind (("<f6>" . my/rust-format-buffer))
  :config
  (require 'eglot)
  (add-to-list
   'eglot-server-programs
   '(rust-mode . ("rust-analyzer")))
  ;; (require 'rusf-rustfmt)
  ;; (defun my/rust-format-buffer ()
  ;;   (interactive)
  ;;   (rust-format-buffer)
  ;;   (save-buffer))
  ;; (require 'lsp-rust)
  ;; (setq lsp-rust-analyzer-completion-add-call-parenthesis nil
  ;;      lsp-rust-analyzer-proc-macro-enable t)
  )


;;; ------------------------------------------
;;; rust-ts-mode
;; (require 'eglot)
;; (use-package rust-ts-mode
;;   :ensure t
;;   :mode ("\\.rs\\'" . rust-ts-mode)
;;   :hook ((rust-ts-mode . eglot-ensure)
;;          (rust-ts-mode . company-mode))
;;   :config
;;   (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))


;;; ------------------------------------------
;;; lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :hook  ((rust-mode    . lsp-deferred)
;;           (rust-ts-mode . lsp-deferred)
;;           (go-mode      . lsp-deferred)
;;           (python-mode      . lsp-deferred))
;;   :bind (:map lsp-mode-map
;;               ("C-c d" . lsp-describe-thing-at-point)
;;               ("C-c a" . lsp-execute-code-action)
;;               ("C-c f" . flycheck-list-errors))
;;   :config
;;   ;; rust
;;   (require 'lsp-rust)
;;   (setq lsp-rust-analyzer-completion-add-call-parenthesis nil
;;         lsp-rust-analyzer-proc-macro-enable t
;;         lsp-rust-analyzer-cargo-watch-command "clippy")

;;   ;; general
;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
;;   (lsp-enable-which-key-integration t)
;;   (setq lsp-enable-links nil
;;         lsp-keep-workspace-alive nil
;;         lsp-signature-doc-lines 2)
;;   (cl-defmethod lsp-clients-extract-signature-on-hover
;;     (contents (_server-id (eql rust-analyzer)))
;;     "from https://github.com/emacs-lsp/lsp-mode/pull/1740 to extract signature in rust"
;;     (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
;;            (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
;;            (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
;;                             ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
;;                             (t nil)))
;;            (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
;;            (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
;;                             ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
;;                             (t (-first-item groups))))
;;            (sig (->> sig-group
;;                      (--drop-while (s-equals? "```rust" it))
;;                      (--take-while (not (s-equals? "```" it)))
;;                      (--map (s-replace-regexp "//.*" "" it))
;;                      (--map (s-trim it))
;;                      (s-join " "))))
;;       (lsp--render-element (concat "```rust\n" sig cmt "\n```")))))


;;; ------------------------------------------
;;; company
(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local company-backend '(company-elisp))))
         (emacs-lisp-mode . company-mode))
  :custom
  (comapny-keymap--unbind-quick-access company-active-map)
  (company-tng-mode t)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(global-company-mode)


;;; ------------------------------------------
;;; format-all
(use-package format-all
  :ensure t
  :commands (format-all-mode format-all-buffer format-all-region))


;;; ------------------------------------------
;;; eshell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun rc/configure-eshell ()
  ;; Save command history when commands are entered


  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after (eshell))

(use-package eshell  
  :commands eshell
  :hook (eshell-first-time-mode . rc/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-'("zsh")))

  (eshell-git-prompt-use-theme 'robbyrussell))


;;; ------------------------------------------
;;; yasnippet
(rc/require 'yasnippet)
(require 'yasnippet)

(setq yas/triggers-in- nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))

(yas-global-mode 1)


;;; ------------------------------------------
;;; word-wrap
(defun rc/enable-word-wrap ()
  (interactive)
  (toggle-word-wrap 1))

(add-hook 'markdown-mode-hook 'rc/enable-word-wrap)


;;; ------------------------------------------
;;; nxml
(add-to-list 'auto-mode-alist '("\\.html\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.ant\\'" . nxml-mode))


;;; ------------------------------------------
;;; tramp
;;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
(setq tramp-auto-save-directory "/tmp")


;;; ------------------------------------------
;;; treemacs
(use-package treemacs
  :ensure t
  :custom
  (treemacs-is-never-other-windwo t)
  :hook
  (treemacs-mode . treemacs-project-follow-mode))


;;; ------------------------------------------
;;; golden-ratio
;; (use-package golden-ratio
;;   :ensure t
;;   :hook (after-init . golden-ratio-mode)
;;   :custom
;;   (golden-ratio-exclude-modes '(occur-mode)))


;;; ------------------------------------------
;;; powershell
(rc/require 'powershell)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))


;;; ------------------------------------------
;;; Tide
(rc/require 'tide)

(defun rc/turn-on-tide ()
  (interactive)
  (tide-setup))

(add-hook 'typescript-mode-hook 'rc/turn-on-tide)


;;; ------------------------------------------
;;; Proof general
(rc/require 'proof-general)
(add-hook 'coq-mode-hook
          '(lambda ()
              (local-set-key (kbd "C-c C-q C-n")
                            (quote proof-assert-until-point-interactive))))


;;; ------------------------------------------
;;; LaTeX mode
(add-hook 'tex-mode-hook
          (lambda ()
            (interactive)
            (add-to-list 'tex-verbatim-environments "code")))

(setq font-latex-fontify-sectioning 'color)


;;; ------------------------------------------
;;; Move Text
(rc/require 'move-text)


;;; ------------------------------------------
;;; Ebisp
(add-to-list 'auto-mode-alist '("\\.ebi\\'" . lisp-mode))


;;; ------------------------------------------
;;; Packages that don't require configuration
(rc/require
 'scala-mode
 'd-mode
 'yaml-mode
 'glsl-mode
 'tuareg
 'lua-mode
 'less-css-mode
 'graphviz-dot-mode
 'clojure-mode
 'cmake-mode
 'rust-mode
 'csharp-mode
 'nim-mode
 'jinja2-mode
 'markdown-mode
 'purescript-mode
 'nix-mode
 'dockerfile-mode
; 'love-minor-mode
 'toml-mode
 'nginx-mode
 'kotlin-mode
 'go-mode
 'php-mode
 'racket-mode
 'qml-mode
 'ag
 'hindent
 'elpy
 'typescript-mode
 'rfc-mode
 'sml-mode
 )


(defun astyle-buffer (&optional justify)
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr"
     nil
     t)
    (goto-line saved-line-number)))


(add-hook 'simpc-mode-hook
          (lambda ()
            (interactive)
            (setq-local fill-paragraph-function 'astyle-buffer)))


;;; ------------------------------------------
(require 'compile)

;; pascalik.pas(24,44) Error: Can't evaluate constant expression

compilation-error-regexp-alist-alist

(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))


;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;; ------------------------------------------
;;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-j")
                            (quote eval-print-last-sexp))))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))


;;; ------------------------------------------
;;; dumb jump
(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;;; M-. => xref-find-definitions | M-, xref-pop-marker-stack


;; ;;; ------------------------------------------
;; ;;; winner mode
;; (use-package winner
;;   :ensure t
;;   :hook after-init
;;   :commands (winner-undo winner-redo)
;;   :custom
;;   (winner-boring-buffers '("*Completions*" "*Help*" "*Apropos*"
;;                            "*Buffer List*" "*info*" "*Compile-Log*")))


;;; ------------------------------------------
;;; winum
(use-package winum
  :ensure t
  :config
  (global-set-key (kbd "M-0") 'treemacs-select-window)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  (winum-mode))


;;; ------------------------------------------
;;; fancy compilation
(use-package fancy-compilation
  :ensure t
  :defer 3
  :config
  (fancy-compilation-mode)
  :custom
  (fancy-compilation-scroll-output 'first-error))


;;; ------------------------------------------
;;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))


;;; =============================================
;; KEY BINDINGS:
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

(global-set-key (kbd "C-c h t") 'helm-cmd-t)
(global-set-key (kbd "C-c h g g") 'helm-git-grep)
(global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h a") 'helm-org-agenda-files-headings)
(global-set-key (kbd "C-c h r") 'helm-recentf)

(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(global-set-key (kbd "<f5>") #'recompile)
(global-set-key (kbd "C-c .") 'xref-find-definitions-other-window)

(defun startup-dictonary ()
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "shuf -n 5 ~/Documents/Org/Words.org"
   ;; output buffer
   (current-buffer)
   ;; replace?
   nil
   ;; name of the error buffer
   "*Dictonary Error Buffer*"
   ;; show error buffer?
   t))

(add-hook 'after-init-hook #'startup-dictonary)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org/" "~/Documents/Agenda/"))
 '(package-selected-packages
   '(yaml-mode winum which-key visual-fill-column use-package undo-tree typescript-mode tuareg treemacs toml-mode tide sml-mode smex scala-mode rfc-mode recompile-on-save rainbow-mode rainbow-delimiters racket-mode quelpa qml-mode purescript-mode proof-general powershell pkg-info php-mode paredit org-drill org-cliplink no-littering nix-mode nim-mode nginx-mode multiple-cursors move-text mood-line marginalia magit lua-mode kotlin-mode jinja2-mode ido-completing-read+ hindent helpful helm-org helm-ls-git helm-git-grep gruber-darker-theme graphviz-dot-mode golden-ratio go-mode gnu-elpa-keyring-update glsl-mode format-all flycheck-rust fancy-compilation exec-path-from-shell eshell-git-prompt elpy eat dumb-jump dockerfile-mode dired-single dired-hide-dotfiles devdocs dash-functional d-mode cursory counsel corfu command-log-mode cmake-mode clojure-mode ccc cargo-mode bind-map bar-cursor auto-package-update ag)))
;; (setq 'treesit-language-source-alist
;;       '(rust "https://github.com/tree-sitter/tree-sitter-rust"))
