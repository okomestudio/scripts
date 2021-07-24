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

;; '(load-home-init-file t t)
;; '(global-font-lock-mode t nil (font-lock))
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

;; (set-face-attribute 'default nil :height 75)
;; (setq inter program-paste-function 'x-cut-buffer-or-selection-value)
;; (setq byte-compile-warnings '(cl-functions))
;; (setq column-number-mode t)
;; (setq size-indication-mode t)

(when window-system
  (scroll-bar-mode t)
  (tool-bar-mode -1)
  (setq select-enable-clipboard t))


(defun ts/setup-frame (frame)
  (progn
    (when (display-graphic-p)
      (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

    ;; Fonts
    (defun ts-get-display-width ()
      "Get the pixel with per display."
      (when window-system
        (let ((monn (length (display-monitor-attributes-list))))
          (/ (display-pixel-width) monn))))

    (defvar ts-display-width (ts-get-display-width))
    (defvar ts-font-size (if (and ts-display-width
                                  (> ts-display-width 2550))
                             18 12))
    (when ts-font-size
      (create-fontset-from-ascii-font
       (format "Hack:weight=normal:slant=normal:size=%d" ts-font-size)
       nil "hackandjp")
      (set-fontset-font "fontset-hackandjp"
                        'unicode
                        (font-spec :family "Noto Sans Mono CJK JP")
                        nil
                        'append)
      (add-to-list 'default-frame-alist '(font . "fontset-hackandjp")))))


(if (daemonp)
    (add-hook 'after-make-frame-functions 'ts/setup-frame)
  (ts/setup-frame (selected-frame)))


(defun sort-lines-ci ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))


;; PACKAGE CONFIGURATION

(require 'package)

(defvar package-archives)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;; BUILT-IN CUSTOMIZATION

(use-package bytecomp
  :ensure nil
  :custom
  (byte-compile-warnigns '(cl-functions)))

(use-package files
  :ensure nil
  :custom
  (make-backup-files nil))

(use-package fringe
  :ensure nil
  :init
  (fringe-mode 0))

(use-package hippie-exp
  :ensure nil
  :init
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

(use-package icomplete
  :ensure nil
  :init
  (fido-mode 1))

(use-package imenu
  :ensure nil
  :init
  (global-set-key (kbd "M-i") 'imenu))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-progressive-speed t)
  (mouse-wheel-scroll-amount '(3 ((shift) . 1))))

(use-package simple
  :ensure nil

  :bind
  (("<f5>" . 'ts/revert-buffer-no-confirm)
   ("C-S-o" . 'ts/newline-above)
   ("C-c C-x *" . 'ts/insert-zero-width-space)
   ("C-x C-y" . (lambda ()
                  (interactive)
                  (insert (shell-command-to-string "pbocr"))))
   ("C-o" . 'ts/newline-below)
   ("M-Q" . 'ts/unfill-paragraph))

  :hook
  (before-save . delete-trailing-whitespace)

  :init
  (defun ts/insert-zero-width-space ()
    (interactive)
    (insert-char #x200b))

  (defun ts/newline-above ()
    (interactive)
    (back-to-indentation)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))

  (defun ts/newline-below ()
    (interactive)
    (end-of-line)
    (newline-and-indent))

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

  (defun ts/unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  (setq-default indent-tabs-mode nil)

  (setq apropos-sort-by-scores t)
  (setq case-fold-search t)             ; in C source code
  (setq sentence-end-double-space nil)  ; in paragraphs.el
  (setq tab-always-indent t)            ; in indent.el
  (setq tab-width 2)                    ; in C source code
  )

(use-package startup
  :ensure nil
  :no-require t
  :custom
  (column-number-mode t)
  (inhibit-splash-screen nil)
  (inhibit-startup-screen nil)
  (size-indication-mode t)

  :init
  (setq-default scroll-bar-width 6)
  (setq ring-bell-function 'ignore)     ; Disable beeping (in C source code)
  (menu-bar-mode -1)
  (prefer-coding-system 'utf-8)         ; Use UTF-8 when possible
)

(use-package tooltip
  :ensure nil
  :init
  (tooltip-mode 1))

;; C-c <left> undoes, C-c <right> redoes
(use-package winner
  :ensure nil
  :init
  (winner-mode 1))

;; frame-comds is used to add C-x o and C-x p to go back and forth between windows.
(use-package frame-cmds
  :ensure nil

  :bind
  (("C-x o" . (lambda () (interactive) (other-window-or-frame 1)))
   ("C-x p" . (lambda () (interactive) (other-window-or-frame -1)))
   ("M-o" . 'other-window-or-frame))

  :init
  (ensure-file-from-url
   "https://www.emacswiki.org/emacs/download/frame-fns.el")
  (ensure-file-from-url
   "https://www.emacswiki.org/emacs/download/frame-cmds.el")

  (setq-default frame-title-format '("" "%f - Emacs")))

;; Line positioning
(use-package mwim
  :bind
  (("C-a" . 'mwim-beginning)
   ("C-e" . 'mwim-end)))


;; THEME

(use-package professional-theme
  :disabled t

  :config
  (load-theme 'professional t))

(use-package spacemacs-common
  :ensure spacemacs-theme

  :config
  (load-theme 'spacemacs-light t))

(use-package all-the-icons
  :init
  (if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
      (all-the-icons-install-fonts +1)))

(use-package cfrs)

(use-package treemacs
  :defer t
  :after (cfrs treemacs-all-the-icons)

  :bind
  (([f8] . treemacs)
   ([mouse-1] . treemacs-single-click-expand-action))

  :config
  (when window-system
    (setq treemacs-indentation 2
          treemacs-is-never-other-window t
          treemacs-space-between-root-nodes nil
          treemacs-width 40))

  (setq treemacs-collapse-dirs 0
        treemacs-file-event-delay 500
        treemacs-follow-after-init t
        treemacs-missing-project-action 'keep
        treemacs-no-png-images nil
        ;treemacs-recenter-after-project-jump nil
        ;treemacs-recenter-after-project-expand nil
        treemacs-show-cursor t
        treemacs-show-hidden-files nil)

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
    (add-to-list 'treemacs-ignored-file-predicates #'ts/treemacs-ignore-python))

  :init
  (treemacs-load-theme "all-the-icons")
  (treemacs-follow-mode -1))

(use-package treemacs-all-the-icons
  :after (all-the-icons))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))


;; ORG MODE

(use-package org-plus-contrib
  :bind
  (("C-c l" . 'org-store-link))

  :config
  (plist-put org-format-latex-options :scale 1.5)

  ;; Add a few characters usable for bounding emphasis markup
  (setcar org-emphasis-regexp-components "-—[:space:]('\"{\x200B")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "-—[:space:].,:!?;'\")}\\[\x200B")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))

  :custom
  ((fill-column 80)
   (org-adapt-indentation nil)
   (org-agenda-files
    (when (> (length (directory-files default-directory t "\\.org$")) 0)
      (directory-files-recursively default-directory "\\.org$")))
   (org-babel-python-command "~/.pyenv/shims/python")
   (org-file-apps '(("\\.mp4\\'" . "vlc --repeat %s")))
   (org-image-actual-width nil)
   (org-list-allow-alphabetical t)
   (org-plantuml-jar-path ts/path-plantuml)
   (org-preview-latex-image-directory ".ltximg/")
   (org-startup-folded t)
   (org-support-shift-select t)
   (org-todo-keywords '((sequence "TODO" "|" "DONE" "SKIP"))))

  :hook
  ((org-mode . (lambda () (org-superstar-mode 1)))
   (org-mode . auto-fill-mode)
   (org-mode . org-indent-mode))

  :init
  (defun org-agenda-gather-files ()
    "Gather org agenda files."
    (interactive)
    (setq org-agenda-files (directory-files-recursively default-directory "\\.org$")))

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

(use-package org-roam
  :custom
  ((org-roam-db-location (file-truename "~/github.com/okomestudio/docs/.org-roam.db"))
   (org-roam-directory (file-truename "~/github.com/okomestudio/docs/")))

  :hook
  (after-init-hook . org-roam-mode))

(use-package org-superstar
  :custom
  (org-superstar-headline-bullets-list '("◉" "🞛" "○" "▷")))


;; UTILITY PACKAGES
(use-package epc
  :ensure t)

;; Dired -- ignore some files
;; (require 'dired-x)
;; (setq-default dired-omit-files-p t)
;; (setq dired-omit-files "^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.pyo$\\|\#$")


;; Based on the number of characters used for search, ace-isearch uses
;; different mode.
(use-package ace-isearch
  :after (ace-jump-mode)

  :config
  (global-ace-isearch-mode 1)

  :custom
  ((ace-isearch-input-length 20)
   (ace-isearch-jump-delay 0.75)))


(use-package ace-jump-mode
  :after (helm-swoop))


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

  :hook (company-mode . company-box-mode)

  :custom
  (company-box-backends-colors
   '((company-capf . (:candidate
                      (:foreground "black")

                      :selected
                      (:background "yellow" :foreground "black"))))))


(use-package company-graphviz-dot
  :after (graphviz-dot-mode)
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


(use-package direnv
  :config
  (direnv-mode)
  :ensure-system-package
  ((direnv . "sudo apt install direnv")))


(use-package dockerfile-mode)


(use-package edit-server
  :if window-system
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))


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


;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config
  (exec-path-from-shell-initialize))


(use-package files
  :ensure nil
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist '(("." . "~/.cache/emacs-backups")))
  (auto-save-file-name-transforms `((".*" "~/.cache/emacs-backups" t))))


(use-package flycheck
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
  (setq sh-basic-offset 4
        sh-indentation 4)
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
  :bind
  (:map json-mode-map
   ("C-M-b" . ts/json-format))

  :config
  ;; See, e.g., https://emacs.stackexchange.com/a/12152/599
  (defun ts/json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min)
                             (point-max)
                             "python -m json.tool"
                             (buffer-name) t)))

  :mode "\\.json\\'" "\\.json.j2\\'"

  :init
  (setq js-indent-level 4))


(use-package lsp-mode
  :commands lsp

  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "~/.pyenv/shims/pylsp")
  ;;                   :major-modes '(python-mode)
  ;;                   :remote? t
  ;;                   :server-id 'pylsp-remote))

  :custom
  (lsp-pylsp-configuration-sources ["flake8"])
  ;(lsp-pylsp-disable-warning t)
  (lsp-pylsp-plugins-flake8-enabled t)
  ;(lsp-pylsp-plugins-flake8-max-line-length 150)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-add-ignore '("D100" "D103"))
  (lsp-pylsp-plugins-pydocstyle-convention "google")
  (lsp-pylsp-plugins-pydocstyle-enabled t)
  (lsp-pylsp-server-command "~/.pyenv/shims/pylsp")

  :ensure-system-package
  ((isort-with-pyenv . "~/.pyenv/versions/$(pyenv global)/bin/pip3 install isort[pyproject]")
   (pylsp-with-pyenv . "~/.pyenv/versions/$(pyenv global)/bin/pip3 install python-lsp-server[all] pyls-black pyls-isort")
   (bash-language-server . "sudo npm i -g bash-language-server")
   (javascript-typescript-langserver . "sudo npm i -g javascript-typescript-langserver")
   (sqls . "go get github.com/lighttiger2505/sqls")
   (unified-language-server . "sudo npm i -g unified-language-server")
   (vscode-json-languageserver . "sudo npm i -g vscode-json-languageserver"))

  :hook
  ((dockerfile-mode . lsp)
   (json-mode . lsp)
   (markdown-mode . lsp)
   (python-mode . lsp)
   (sh-mode . lsp)
   (sql-mode . lsp)
   (yaml-mode . lsp))

  :init
  (defun ts/get-global-pypath (exe)
    (let ((ver (car (split-string (shell-command-to-string "pyenv global")))))
      (concat "~/.pyenv/versions/" ver "/bin/" exe)))
  (setq pylsp-with-pyenv (ts/get-global-pypath "pylsp"))
  (setq isort-with-pyenv (ts/get-global-pypath "isort")))


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
  :commands (markdown-mode gfm-mode)

  :ensure-system-package
  ((pandoc . "sudo apt install pandoc"))

  :hook ((markdown-mode) . remove-trailing-whitespaces-on-save)

  :init
  (setq markdown-command "pandoc")

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.lr\\'" . markdown-mode)))


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
  ((sh-basic-offset 4)
   (sh-indentation 4))
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
  (sqlformat-command 'sqlfluff)
  ;(sqlformat-command 'pgformatter)
  ;(sqlformat-args '("-f2" "-g" "-s4" "-U2"
  ;                  "-M" "-p" "\n[ ]*-- sqlfmt: off\n(?:.*)?-- sqlfmt: on\n"))

  :ensure-system-package
  ;(pg_format . "sudo apt install pgformatter")
  ("~/.pyenv/shims/sqlfluff" . "~/.pyenv/shims/pip3 install sqlfluff")

  ;:hook
  ;((sql-mode . sqlformat-on-save-mode))

  :init
  (require 'sqlformat)

  (defun ts/sqlformat ()
    (interactive)
    (save-excursion
      (sqlformat (point-min) (point-max))
      (delete-trailing-whitespace))))


(use-package sql-upcase
  :disabled t

  :ensure nil

  :init
  (ensure-file-from-github "emacsmirror/emacswiki.org/master/sql-upcase.el")

  :hook
  ((sql-mode sql-interactive-mode) . sql-upcase-mode))


(use-package tern
  :ensure nil

  :custom
  (tern-command '("tern" "--no-port-file"))

  :ensure-system-package
  ((tern . "sudo npm install -g tern"))

  :init
  (ensure-file-from-github "ternjs/tern/master/emacs/tern.el"))


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


(use-package yasnippet
  :config
  (yas-reload-all)

  :hook
  ((prog-mode . yas-minor-mode)))


;; mini-buffer completion

(use-package consult
  :init
  (global-set-key [remap goto-line] 'consult-goto-line))

(use-package embark)

(use-package embark-consult
  :after (embark consult)
  ;:demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  ;:hook
  ;(embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  ;; Completion style that matches multiple regexps in any order
  :init
  (setq completion-styles '(orderless)))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :custom
  (vertico-count 20)

  :init
  (vertico-mode))


;; solaire-mode : visually distinguish "real" buffers from "unreal" buffers

(use-package solaire-mode
  :init
  (solaire-global-mode +1))


(use-package tramp
  :defer t

  :custom
  (tramp-default-method "ssh"))


(use-package google-translate
  :bind
  (("C-c t" . 'google-translate-at-point)
   ("C-c T" . 'google-translate-query-translate))

  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  :custom
  ((google-translate-backend-method 'curl)
   (google-translate-default-source-language "auto")
   (google-translate-default-target-language "ja")))


(provide 'init)
;;; init.el ends here
