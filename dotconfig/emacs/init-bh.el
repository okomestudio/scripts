;;; emacs --- Emacs configuration
;;
;;; Commentary:
;;
;; This file should be placed at ~/.config/emacs/init.el.
;;
;;
;;; Code:

(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; '(inhibit-splash-screen t)
;; '(inhibit-startup-screen t)
;; '(load-home-init-file t t)
;; '(global-font-lock-mode t nil (font-lock))
;; '(make-backup-files nil)
;; '(default-input-method "rfc1345")
;; '(current-language-environment "UTF-8")
;; '(global-whitespace-mode nil)

;; UTILITY VARIABLES AND FUNCTIONS

(defconst ts/path-plantuml (expand-file-name "/usr/local/share/plantuml/plantuml.jar")
  "Path to PlantUML JAR")

;; Custom emacs lisp directory for .el files
(defconst my-lispdir
  (expand-file-name (concat user-emacs-directory "lisp/")))
(if (not (file-directory-p my-lispdir))
    (make-directory my-lispdir :parents))
(if (file-directory-p my-lispdir)
    (add-to-list 'load-path my-lispdir))


;; Cache directory
(defconst my-cachedir
  (expand-file-name "~/.cache/emacs-backups"))
(if (not (file-directory-p my-cachedir))
    (make-directory my-cachedir :parents))


(defun ensure-file-from-url (src &optional dest)
  "Ensure that file at URL gets downloaded and exists.

SRC is the source URL, DEST is the local destination path for the
downloaded file. If DEST is not given, the filename is inferred
from the source path. If DEST is not an absolute path, the file
will be created in the my-lipdir directory."
  (let ((dest (if dest
                 (if (string-prefix-p "/" dest) dest (concat my-lispdir dest))
               (concat my-lispdir (file-name-nondirectory src)))))
    (if (not (file-exists-p dest))
        (url-copy-file src dest))))


(defun ensure-file-from-github (src &optional dest)
  "Ensure that a file hosted by GitHub gets downloaded and exists.

SRC is the source path in GitHub, DEST is the local destination
path for the downloaded file. See ensure-file-from-url for
detail."
  (ensure-file-from-url
   (concat "https://raw.githubusercontent.com/" src) dest))


(defun remove-trailing-whitespaces-on-save ()
  "Remove trailing whitespaces on save.

  Use this function with a mode hook."
  (add-hook 'local-write-file-hooks
            '(lambda () (save-excursion (delete-trailing-whitespace)))))


(defun ts/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGS

(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'other-window-or-frame)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;(set-face-attribute 'default nil :height 75)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq-default frame-title-format '("" "%f - Emacs"))  ;; for frame-cmds.el
(setq-default scroll-bar-width 6)
(setq-default indent-tabs-mode nil)
(fringe-mode 0)

(setq apropos-sort-by-scores t)
(setq byte-compile-warnings '(cl-functions))
(setq case-fold-search t)
(setq column-number-mode t)
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq ring-bell-function 'ignore)  ;; Disable beeping
(setq sentence-end-double-space nil)
(setq size-indication-mode t)
(setq tab-always-indent t)
(setq tab-width 2)
(setq tramp-default-method "ssh")

(fido-mode 1)                           ; using over ido
(menu-bar-mode 1)
(savehist-mode 1)
(winner-mode 1)                         ; C-c <left> undoes, C-c <right> redoes

;; (tooltip-mode -1)
;; (fset 'menu-bar-open nil)
(when window-system
  (scroll-bar-mode t)
  (tool-bar-mode -1)
  (setq select-enable-clipboard t))

;; Use UTF-8 when possible
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Fonts
(defun ts-get-display-width ()
  "Get the pixel with per display."
  (when window-system
    (let ((monn (length (display-monitor-attributes-list))))
      (/ (display-pixel-width) monn))))

(defvar ts-display-width (ts-get-display-width))
(defvar ts-font-size (if (> ts-display-width 2550) 18 12))

(when ts-font-size
  (create-fontset-from-ascii-font
   (format "Hack:weight=normal:slant=normal:size=%d" ts-font-size)
   nil "hackandjp")
  (set-fontset-font "fontset-hackandjp"
                    'unicode
                    (font-spec :family "Noto Sans Mono CJK JP")
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-hackandjp")))

;; BUGFIX: For fixing a startup error message
;;
;;   http.elpa.gnu.org:443*-257153" has a running process; kill it? (y or n) y
;;
;; This bug should be fixed on and after Emacs version 26.3.
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;; PACKAGE CONFIGURATION

(require 'package)

(defvar package-archives)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

;; For important compatibility libraries like cl-lib
;; (when (< emacs-major-version 24)
;;   (add-to-list 'package-archives
;;                '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package startup
  :no-require t
  :ensure nil
  :custom
  (inhibit-splash-screen t)
  (inhibit-startup-screen t))

(use-package use-package-ensure-system-package
  :ensure t)

;; Theme
(use-package professional-theme
  :config
  (load-theme 'professional t))

;; Utility packages
(use-package epc
  :ensure t)


;; CUSTOM KEYBINDINGS

;; frame-comds is used to add C-x o and C-x p to go back and forth between windows.
(use-package frame-cmds
  :ensure nil

  :bind
  (("C-x o" . (lambda () (interactive) (other-window-or-frame 1)))
   ("C-x p" . (lambda () (interactive) (other-window-or-frame -1))))

  :init
  (ensure-file-from-url
   "https://www.emacswiki.org/emacs/download/frame-fns.el")
  (ensure-file-from-url
   "https://www.emacswiki.org/emacs/download/frame-cmds.el"))


(use-package simple
  :ensure nil

  :bind
  (("<f5>" . 'ts/revert-buffer-no-confirm)
   ("C-S-o" . 'ts/newline-above)
   ("C-o" . 'ts/newline-below))

  :hook
  (before-save . delete-trailing-whitespace)

  :init
  (defun ts/revert-buffer-no-confirm (&optional force-reverting)
    "Interactive call to 'revert-buffer'.

    Ignoring the auto-save file and not requesting for confirmation.
    When the current buffer is modified, the command refuses to
    revert it, unless you specify the optional argument:
    FORCE-REVERTING to true."
    (interactive "P")
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))

  (defun ts/newline-above ()
    (interactive)
    (back-to-indentation)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))

  (defun ts/newline-below ()
    (interactive)
    (end-of-line)
    (newline-and-indent)))


;; PACKAGES

;; Dired -- ignore some files
;; (require 'dired-x)
;; (setq-default dired-omit-files-p t)
;; (setq dired-omit-files "^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.pyo$\\|\#$")


;; Based on the number of characters used for search, ace-isearch uses
;; different mode.
(use-package ace-isearch
  :config
  (global-ace-isearch-mode 1)
  (setq ace-isearch-input-length 20
        ace-isearch-jump-delay 0.4))


(use-package ace-jump-mode)


(use-package add-node-modules-path)


(use-package anki-editor
  :custom
  (anki-editor-use-math-jax t)
  (request-log-level 'debug)
  :ensure-system-package
  (curl . "sudo apt install curl"))


(use-package ansible
  :after (yaml-mode)

  :custom
  (ansible-vault-password-file nil)

  :hook (((yaml-mode) . my-ansible-mode-hook)
         ((ansible) . ansible-auto-decrypt-encrypt))

  :config
  (defun my-ansible-mode-hook ()
    (if (locate-dominating-file default-directory "ansible.cfg")
        (progn (ansible 1)))))


(use-package any-ini-mode
  :ensure nil
  :mode ".*\\.ini$" ".*\\.conf$" ".*\\.service$"
  :init
  (ensure-file-from-url
   "https://www.emacswiki.org/emacs/download/any-ini-mode.el"))


(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 7)
  (auto-package-update-maybe))


(use-package bats-mode)


;; if using multiple virtual env, this might become useful:
;;
;;   http://stackoverflow.com/questions/21246218/how-can-i-make-emacs-jedi-use-project-specific-virtualenvs
;; (setq jedi:server-args (list (or (buffer-file-name) default-directory)))
;; (push "--sys-path" jedi:server-args)
;; (message "for jedi:server-args %s" jedi:server-args)

;; black -- The opinionated Python code formatter
;;
;; To activate blacken-mode per project basis, place
;;
;;   ((python-mode . ((eval . (blacken-mode 1)))))
;;
;; in .dir-locals.el.
(use-package blacken
  :after python

  :bind
  (:map python-mode-map
   ("C-M-b" . ts/blacken-buffer))

  :custom
  (blacken-line-length 150)

  :ensure-system-package
  (black . "pip install black")

  :if (not (version< emacs-version "25.2"))

  :init
  (defun ts/blacken-buffer ()
    (interactive)
    (blacken-buffer)
    (py-isort-buffer)))


(use-package cc-mode
  :ensure nil
  :config
  (setq c-basic-offset 2))


(use-package cfrs)


(use-package company
  :custom
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers 'left)
  (company-tooltip-limit 20)

  :init
  (global-company-mode)
  (add-to-list 'company-backends '(company-files))

  ;; See https://emacs.stackexchange.com/a/24800/599 for tweaking the enter key
  ;; behavior.
  (dolist (key '("<return>" "RET"))
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil)

  ;; Company appears to override the above keymap based on company-auto-complete-chars.
  ;; Turning it off ensures we have full control.
  (setq company-auto-complete-chars nil))


(use-package company-box
  :after company

  :hook
  (company-mode . company-box-mode)

  :custom
  (company-box-backends-colors
   '((company-capf . (:candidate (:foreground "black")
                      :selected (:background "yellow" :foreground "black"))))))


(use-package company-graphviz-dot
  :ensure nil
  :init
  (ensure-file-from-github
   "ppareit/graphviz-dot-mode/master/company-graphviz-dot.el"))


(use-package company-jedi
  :disabled t
  :after company
  :config
  (add-to-list 'company-backends 'company-jedi))


(use-package company-restclient
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))


(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))


(use-package company-tern
  :after (company dash dash-functional tern)
  :ensure nil
  :init
  (ensure-file-from-url
   (concat "https://gist.githubusercontent.com/"
           "okomestudio/de8c59960ce8f195ee0224de5db5a168/"
           "raw/1193992ffeeca8193ebf459b377c27f628ac3246/company-tern.el"))
  (add-to-list 'company-backends 'company-tern))


(use-package company-web
  :after company
  :init
  (add-to-list 'company-backends 'company-web-html))


(use-package cython-mode
  :after python)


(use-package dash)


(use-package dash-functional)


(use-package direnv
  :config
  (direnv-mode)
  :ensure-system-package
  ((direnv . "sudo apt install direnv")))


(use-package dockerfile-mode)


(use-package emacs-lisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . eldoc-mode)))


(use-package elpy
  :disabled t

  :config
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
  (defun ts/elpy-hooks ()
    (hs-minor-mode))
  :custom
  (elpy-folding-fringe-indicator t)
  (elpy-rpc-backend "jedi")
  (elpy-rpc-virtualenv-path (or (getenv "VIRTUAL_ENV")
                                "~/.pyenv/versions/3.8.2"
                                (concat user-emacs-directory "elpy/rpc-venv")))
  :defer t
  :hook (elpy-mode . ts/elpy-hooks)
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setenv "WORKON_HOME" "~/.pyenv/versions/"))


(use-package files
  :ensure nil
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist '(("." . "~/.cache/emacs-backups")))
  (auto-save-file-name-transforms `((".*" "~/.cache/emacs-backups" t))))


(use-package flycheck
  :custom
  (flycheck-flake8-maximum-line-length 150)

  :init
  (global-flycheck-mode))


(use-package flycheck-pos-tip
  :init
  (setq flycheck-pos-tip-timeout 60)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))


(use-package flymake-shellcheck
  :disabled t
  :ensure-system-package (shellcheck . "sudo apt install shellcheck")
  :if (executable-find "shellcheck")
  :commands flymake-shellcheck-load
  :hook ((sh-mode) . flymake-shellcheck-load)
  :init
  (setq sh-basic-offset 2
        sh-indentation 2)
  (add-to-list 'auto-mode-alist '("/bashrc\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("/bash_.*\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.bats\\'" . sh-mode))
  (add-to-list 'interpreter-mode-alist '("bats" . sh-mode)))


(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (shell-script-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))


(use-package graphviz-dot-mode
  :custom
  (graphviz-dot-indent-width 2)

  :ensure-system-package
  (dot . "sudo apt install graphviz"))


(use-package helm-swoop)


(use-package highlight-indent-guides
  :disabled t
  :hook ((emacs-lisp-mode python-mode sh-mode) . highlight-indent-guides-mode)
  :config
  ;; (setq highlight-indent-guides-auto-enabled nil)
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┆
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0
        highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-character-face "light yellow")
  (set-face-background 'highlight-indent-guides-top-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-top-character-face "gray"))


(use-package htmlize)


(use-package icomplete
  :ensure nil)


(use-package ido
  :disabled t                           ; disabled to use fido
  :ensure nil
  :config
  (setq ido-enable-flex-matching t)
  ;; (setq ido-everywhere t)
  (ido-mode 1)
  (add-to-list 'ido-ignore-files "\\.egg-info/$")
  (add-to-list 'ido-ignore-files "^\\.eggs/$")
  (add-to-list 'ido-ignore-files "^\\.pytest_cache/$")
  (add-to-list 'ido-ignore-files "^__pycache__/$")
  (add-to-list 'ido-ignore-files "^build/$")
  (add-to-list 'ido-ignore-files "^dist/$")
  ;; Note that ido-ignore-directories does not appear to affect C-x C-f
  ;; (add-to-list 'ido-ignore-directories "src")
  )


;; jedi.el -- Autocompletion for python
;;
;; On first install, the following needs to be run within Emacs:
;;
;;   M-x jedi:install-server RET
(use-package jedi-core
  :disabled t
  :config
  (setq jedi:complete-on-dot nil
        jedi:get-in-function-call-delay 500
        jedi:tooltip-method nil  ; or '(pos-tip)
        jedi:use-shortcuts t)
  :hook ((elpy-mode) . jedi:setup))


(use-package jq-mode
  :ensure-system-package (jq . "sudo apt install jq"))


(use-package json-mode
  :mode "\\.json\\'" "\\.json.j2\\'"
  :init
  (setq js-indent-level 2))


(use-package lsp-mode
  :commands lsp

  :custom
  (lsp-pyls-configuration-sources ["flake8"])

  :ensure-system-package
  ((isort . "pip install isort[pyproject]==4.3.21") ; NOTE: https://github.com/PyCQA/pylint/pull/3725
   (javascript-typescript-langserver . "sudo npm i -g javascript-typescript-langserver")
   (pyls . "pip install python-language-server[flake8,mccabe,pydocstyle,pylint,rope,test] pyls-black pyls-isort")
   (bash-language-server . "sudo npm i -g bash-language-server"))

  :hook
  ((sh-mode . lsp)
   (python-mode . lsp)))


(use-package lsp-ui
  :custom
  ((lsp-ui-doc-delay 0.5)
   (lsp-ui-doc-position 'at-point)
   (lsp-ui-doc-use-webkit nil))

  :commands lsp-ui-mode)


(use-package lsp-treemacs
  :bind ([f7] . lsp-treemacs-symbols)
  :commands lsp-treemacs-errors-list)


;; Allows browser preview with C-c C-c v
(use-package markdown-mode
  :hook ((markdown-mode) . remove-trailing-whitespaces-on-save)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.lr\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))


(use-package magit)


(use-package minions
  :commands minions-mode
  :init (minions-mode 1))


(use-package ob-typescript)


(use-package openwith
  :custom
  ((openwith-associations '(("\\.pdf\\'" "okular" (file)))))

  :init
  (ensure-file-from-url "https://www.metalevel.at/misc/openwith.el")
  (openwith-mode t))


(use-package org-bullets)


(use-package org-plus-contrib
  :bind
  (("C-c l" . 'org-store-link))

  :custom
  ((fill-column 80)
   (org-adapt-indentation nil)
   (org-agenda-files
    (when (> (length (directory-files default-directory t "\\.org$")) 0)
      (directory-files-recursively default-directory "\\.org$")))
   (org-babel-python-command "~/.pyenv/shims/python")
   (org-image-actual-width nil)
   (org-list-allow-alphabetical t)
   (org-plantuml-jar-path ts/path-plantuml)
   (org-preview-latex-image-directory ".ltximg/")
   (org-startup-folded t)
   (org-support-shift-select t)
   (org-todo-keywords '((sequence "TODO" "|" "DONE" "SKIP"))))

  :hook
  ((org-mode . (lambda () (org-bullets-mode 1)))
   (org-mode . auto-fill-mode)
   (org-mode . org-indent-mode))

  :init
  (plist-put org-format-latex-options :scale 1.5)

  ;; Add a few characters usable for bounding emphasis markup
  (setcar org-emphasis-regexp-components "-—[:space:]('\"{")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "-—[:space:].,:!?;'\")}\\[")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (dot . t)
     (emacs-lisp . t)
     (js . t)
     (plantuml . t)
     (python . t)
     (shell . t)
     (sql . t)
     (typescript . t))))


(use-package plantuml-mode
  :custom
  ((plantuml-default-exec-mode 'jar)
   (plantuml-jar-path ts/path-plantuml))

  :mode
  ("\\.plantuml\\'" . plantuml-mode)
  ("\\.puml\\'" . plantuml-mode))


(use-package pos-tip
  :init
  (setq pos-tip-background-color "white")
  (if ts-font-size
      (setq pos-tip-internal-border-width
            (truncate (* ts-font-size 1.5)))))


;; JavaScript code formatter
(use-package prettier-js
  :ensure-system-package (prettier . "sudo npm install -g prettier")
  :config
  (setq prettier-js-args
        '(
          "--arrow-parens" "always"
          "--print-width" "88"
          "--single-quote"
          "--trailing-comma" "all"
          )))


(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . remove-trailing-whitespaces-on-save)
         (prog-mode . show-paren-mode)))


(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(use-package py-isort
  :after python

  :ensure nil                           ; Use patched version till PR #21 gets merged

  :custom
  (py-isort-options '("--multi-line=6" "--line-width=150"))

  :init
  (ensure-file-from-github
   "okomestudio/py-isort.el/ts/provide-default-settings-path/py-isort.el")
  (add-hook 'before-save-hook 'py-isort-before-save))


(use-package pyenv-mode
  :disabled t
  :config
  (pyenv-mode 1))


(use-package python
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i"))
  :custom
  ((python-indent-guess-indent-offset-verbose nil)
   (python-indent-offset 4)))


(use-package python-pytest
  :after (direnv)
  :bind
  (:map python-mode-map
   ("C-c t" . python-pytest-dispatch)))


(use-package restclient
  :after (jq-mode)
  :init
  (ensure-file-from-github "pashky/restclient.el/master/restclient-jq.el"))


(use-package rst-mode
  :ensure nil
  :hook ((rst-mode . remove-trailing-whitespaces-on-save)))


(use-package rust-mode
  :custom
  ((rust-format-on-save t)))


(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))


(use-package sh-mode
  :custom
  ((sh-basic-offset 2)
   (sh-indentation 2))
  :ensure nil
  :mode ("\\.sh\\'"
         "bash_*"
         "bashrc\\'"))


(use-package sql
  :config
  (sql-set-product 'postgres))


(use-package sqlformat
  :after (sql)

  :bind
  (:map sql-mode-map
   ("C-M-b" . 'ts/sqlformat))

  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-f2" "-g" "-s2" "-U2"))

  :ensure-system-package
  (pg_format . "sudo apt install pgformatter")

  :hook
  ((sql-mode . sqlformat-on-save-mode))

  :init
  (require 'sqlformat)

  (defun ts/sqlformat ()
    (interactive)
    (save-excursion
      (sqlformat (point-min) (point-max))
      (delete-trailing-whitespace))))


(use-package sql-upcase
  :ensure nil
  :init
  (ensure-file-from-github "emacsmirror/emacswiki.org/master/sql-upcase.el")
  :hook ((sql-mode sql-interactive-mode) . sql-upcase-mode))


(use-package tern
  :ensure nil

  :custom
  (tern-command '("tern" "--no-port-file"))

  :ensure-system-package
  ((tern . "sudo npm install -g tern"))

  :init
  (ensure-file-from-github "ternjs/tern/master/emacs/tern.el"))


(use-package treemacs
  :ensure t
  :defer t
  :after cfrs

  :bind
  (([f8] . treemacs)
   ([mouse-1] . treemacs-single-click-expand-action))

  :config
  (when window-system
    (setq treemacs-indentation 1
          treemacs-is-never-other-window t
          treemacs-space-between-root-nodes nil
          treemacs-width 35))

  (setq treemacs-collapse-dirs 0
        treemacs-file-event-delay 500
        treemacs-follow-after-init t
        treemacs-missing-project-action 'keep
        treemacs-no-png-images nil
        treemacs-show-cursor t
        treemacs-show-hidden-files nil)

  (treemacs-follow-mode t)
  (treemacs-resize-icons 11)  ;; needs imagemagick support on Emacs build
  ;; (treemacs-display-current-project-exclusively)

  (with-eval-after-load 'treemacs
    (defun ts/treemacs-ignore-emacs (filename absolute-path)
      (or (string-match-p "\\.elc$" filename)
          (string-match-p "^#.*#$" filename)
          (string-match-p "~$" filename)))
    (add-to-list 'treemacs-ignored-file-predicates #'ts/treemacs-ignore-emacs)
    (defun ts/treemacs-ignore-python (filename absolute-path)
      (or (string-match-p "\\(^\\|/\\)__pycache__$" filename)
          (string-match-p "\\(^\\|/\\)build$" filename)
          (string-match-p "\\(^\\|/\\)dist$" filename)
          (string-match-p "\\.egg-info$" filename)
          (string-match-p "\\.pyc$" filename)))
    (add-to-list 'treemacs-ignored-file-predicates #'ts/treemacs-ignore-python)))


(use-package treemacs-magit
  :after treemacs magit)


(use-package treemacs-projectile
  :after treemacs projectile)


(use-package typescript-mode)


(use-package typo
  :hook ((text-mode . typo-mode)))


(use-package undo-tree
  :init (global-undo-tree-mode))


(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))
;; '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode for JavaScript, HTML, and CSS
;;
;; For linters, install the following:
;;
;;   $ sudo npm install -g eslint babel-eslint eslint-plugin-react
;;   $ sudo apt install tidy
;;   $ sudo npm install -g csslint
;;
(use-package web-mode
  :after (company-tern prettier-js add-node-modules-path)
  :mode ("\\.css\\'"
         "\\.html?\\'"
         "\\.j2\\'"
         "\\.jsx?\\'")

  :config
  (defun ts/web-mode-hook ()
    (add-node-modules-path)
    (require 'flycheck)
    ;; Disable checkers not in use
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist
                            javascript-jshint
                            javascript-jscs)))
    (let (checker)
      (cond ((string= web-mode-content-type "html")
             (when (executable-find "tidy")
               (setq checker 'html-tidy)))
            ((string= web-mode-content-type "css")
             (when (executable-find "csslint")
               (setq checker 'css-csslint)))
            ((or (string= web-mode-content-type "javascript")
                 (string= web-mode-content-type "jsx"))
             (when (executable-find "eslint")
               (setq checker 'javascript-eslint))
             (web-mode-set-content-type "jsx")
             (prettier-js-mode)
             (lsp)
             (tern-mode)))

      (flycheck-add-mode checker 'web-mode)
      (flycheck-select-checker checker)))

  (defun ts/web-mode-flyspell-verify ()
    ;; For detail, see:
    ;; http://blog.binchen.org/posts/effective-spell-check-in-emacs.html
    (cond ((string= web-mode-content-type "html")
           (let* ((f (get-text-property (- (point) 1) 'face))
                  rlt)
             (cond
              ;; Check the words with these font faces, possibly.
              ;; This *blacklist* will be tweaked in next condition.
              ((not (memq f '(web-mode-html-attr-value-face
                              web-mode-html-tag-face
                              web-mode-html-attr-name-face
                              web-mode-constant-face
                              web-mode-doctype-face
                              web-mode-keyword-face
                              web-mode-comment-face  ;; focus on get html label right
                              web-mode-function-name-face
                              web-mode-variable-name-face
                              web-mode-css-property-name-face
                              web-mode-css-selector-face
                              web-mode-css-color-face
                              web-mode-type-face
                              web-mode-block-control-face)))
               (setq rlt t))
              ;; Check attribute value under certain conditions:
              ((memq f '(web-mode-html-attr-value-face))
               (save-excursion
                 (search-backward-regexp "=['\"]" (line-beginning-position) t)
                 (backward-char)
                 (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                         (thing-at-point 'symbol)))))
              ;; Finalize the blacklist.
              (t (setq rlt nil)))
             rlt))
          ((or (string= web-mode-content-type "javascript")
               (string= web-mode-content-type "jsx"))
           (let* ((f (get-text-property (- (point) 1) 'face)))
             ;; *whitelist*
             ;; only words with following font face will be checked
             (memq f '(js2-function-call
                       js2-function-param
                       js2-object-property
                       font-lock-variable-name-face
                       font-lock-string-face
                       font-lock-function-name-face))))))

  (put 'web-mode 'flyspell-mode-predicate 'ts/web-mode-flyspell-verify)

  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)

  :ensure-system-package
  (;(csslint . "npm install --save-dev csslint")
   ;(eslint . "npm install --save-dev eslint babel-eslint eslint-plugin-react")
   (tidy . "sudo apt install tidy"))

  :hook (web-mode . ts/web-mode-hook)

  :init
  (add-to-list 'company-backends '(company-css)))


(use-package whole-line-or-region)


(use-package yaml-mode
  :hook
  ((yaml-mode . (lambda () (typo-mode -1)))
   (yaml-mode . remove-trailing-whitespaces-on-save))

  :mode "\\.ya?ml\\'" "\\.ya?ml.j2\\'")


(use-package yascroll
  :init
  (ensure-file-from-github "emacsorphanage/yascroll/master/yascroll.el")
  (when (not window-system)
    (require 'yascroll)
    (global-yascroll-bar-mode 1)))


(provide 'init)
;;; init.el ends here