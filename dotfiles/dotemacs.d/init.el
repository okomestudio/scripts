;; -*- mode: emacs-lisp -*-
;;; emacs --- Emacs configuration
;;
;;; Commentary:
;;
;; This should be placed at ~/.emacs.d/init.dl.
;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN CUSTOM CONFIGS BY EMACS; DO NOT MODIFY

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-input-length 20)
 '(ace-isearch-jump-delay 0.6)
 '(c-basic-offset 2)
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(fringe-mode 0 nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(global-whitespace-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(package-selected-packages
   (quote
    (files yascroll sql-upcase dired dired-x pyvenv pyenv auto-package-update bats-mode sh-mode sh flymake-mode sqlformat frame-cmds multiple-cursors prettier-js py-isort company-jedi company-tern company highlight-indent-guides popup flyckeck-popup-tip blacken flyspell-prog blacken-mode any-ini-mode professional-theme github-modern-theme magit web-mode use-package helm-swoop ace-jump-mode epc flycheck plantuml-mode yaml-mode scala-mode neotree markdown-mode json-mode flymake-cursor dockerfile-mode cython-mode ansible ace-isearch)))
 '(scroll-bar-mode t)
 '(scroll-bar-width 6 t)
 '(select-enable-clipboard t)
 '(show-paren-mode t nil (paren))
 '(size-indication-mode t)
 '(tab-always-indent t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM CONFIGS BY TS; CAN MODIFY

;;(set-face-attribute 'default nil :height 75)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq frame-title-format '("" "%f"))  ;; for frame-cmds.el
(setq ring-bell-function 'ignore)  ;; Disable beeping

(setq-default indent-tabs-mode nil)
(savehist-mode 1)

;; Use UTF-8 when possible
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Fonts
(when window-system
  (setq monn (length (display-monitor-attributes-list)))
  (if (> (/ (display-pixel-width) monn) 2550)
      (create-fontset-from-ascii-font
       "Hack:weight=normal:slant=normal:size=18" nil "hackandjp")
    (create-fontset-from-ascii-font
     "Hack:weight=normal:slant=normal:size=14" nil "hackandjp"))
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

(defvar package-archives)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; For important compatibility libraries like cl-lib
;; (when (< emacs-major-version 24)
;;   (add-to-list 'package-archives
;;                '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Theme
(use-package professional-theme
  :config
  (load-theme 'professional t))

;; Utility packages
(use-package epc
  :ensure t)

(use-package popup
  :ensure t)

(use-package url
  :ensure t)


;; UTILITY VARIABLES AND FUNCTIONS

;; Custom emacs lisp directory for .el files
(defconst my-lispdir "~/.emacs.d/lisp/")

(if (not (file-directory-p my-lispdir))
    (make-directory my-lispdir :parents))
(if (file-directory-p my-lispdir)
    (add-to-list 'load-path my-lispdir))


(defun ensure-downloaded-file (src dest)
  "Download a file from a URL and save to the disk.

  SRC is the source URL, DEST is the destination file path."
  (if (not (file-exists-p dest))
      (url-copy-file src dest)))


(defun remove-trailing-whitespaces-on-save ()
  "Remove trailing whitespaces on save.

  Use this function with a mode hook."
  (add-hook 'local-write-file-hooks
            '(lambda () (save-excursion (delete-trailing-whitespace)))))


;; CUSTOM KEYBINDINGS

;; frame-comds is used to add C-x o and C-x p to go back and forth between windows.
(use-package frame-cmds
  :ensure nil
  :init
  (ensure-downloaded-file
   "https://www.emacswiki.org/emacs/download/frame-fns.el"
   (concat my-lispdir "frame-fns.el"))
  (ensure-downloaded-file
   "https://www.emacswiki.org/emacs/download/frame-cmds.el"
   (concat my-lispdir "frame-cmds.el"))
  (define-key (current-global-map) (kbd "C-x o") (lambda () (interactive) (other-window-or-frame 1)))
  (define-key (current-global-map) (kbd "C-x p") (lambda () (interactive) (other-window-or-frame -1))))


(defun ts/insert-line-before (times)
  "Insert a newline(s) above the line containing the cursor."
  (interactive "p") ; called from M-x
  (save-excursion ; store position
    (move-beginning-of-line 1)
    (newline times))) ; insert new line
(global-set-key (kbd "C-S-o") 'ts/insert-line-before)


;; [F5] to trigger revert-buffer without confirmation
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to 'revert-buffer'.

  Ignoring the auto-save file and not requesting for confirmation.
  When the current buffer is modified, the command refuses to
  revert it, unless you specify the optional argument:
  FORCE-REVERTING to true."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)


;; Ido ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
(ido-mode 1)

;; Python files and directories
(add-to-list 'ido-ignore-files "\\.egg-info/$")
(add-to-list 'ido-ignore-files "^\\.eggs/$")
(add-to-list 'ido-ignore-files "^\\.pytest_cache/$")
(add-to-list 'ido-ignore-files "^__pycache__/$")
(add-to-list 'ido-ignore-files "^build/$")
(add-to-list 'ido-ignore-files "^dist/$")

;; Note that ido-ignore-directories does not appear to affect C-x C-f
;; (add-to-list 'ido-ignore-directories "src")


;; PACKAGES

;; Dired -- ignore some files
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.pyo$\\|\#$")


;; Based on the number of characters used for search, ace-isearch uses
;; different mode.
(use-package ace-isearch
  :config
  (global-ace-isearch-mode 1))


(use-package ace-jump-mode)


(use-package ansible
  :after (yaml-mode)
  :hook (((yaml-mode) . my-ansible-mode-hook)
         ((ansible) . ansible-auto-decrypt-encrypt))
  :config
  (defun my-find-vault-password-file (name)
    (setq dir (locate-dominating-file default-directory name))
    (if dir (concat dir name) "~/.vault-password"))

  (defun my-ansible-mode-hook ()
    (if (locate-dominating-file default-directory "ansible.cfg")
        (progn
          (setq ansible-vault-password-file
                (my-find-vault-password-file ".vault-password"))
          (ansible 1)))))


(use-package any-ini-mode
  :ensure nil
  :mode ".*\\.ini$" ".*\\.conf$" ".*\\.service$"
  :init
  (ensure-downloaded-file
   "https://www.emacswiki.org/emacs/download/any-ini-mode.el"
   (concat my-lispdir "any-ini-mode.el")))


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
;; Need `pip install black` to actually use it.
;;
;; To activate blacken-mode per project basis, place
;;
;;   ((python-mode . ((eval . (blacken-mode 1)))))
;;
;; in .dir-locals.el.
(use-package blacken
  :after python
  :if (not (version< emacs-version "25.2")))


(use-package company
  :init
  (global-company-mode)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t))


(use-package company-jedi
  :after (company)
  :config
  (add-to-list 'company-backends 'company-jedi))


(use-package company-tern
  :after (company)
  :disabled t
  :ensure nil)


(use-package cython-mode
  :after python)


(use-package dockerfile-mode)


(use-package files
  :ensure nil
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist '(("." . "~/.cache/emacs-backups")))
  (auto-save-file-name-transforms '((".*" "~/.cache/emacs-backups/" t))))


(use-package flycheck
  :init
  (global-flycheck-mode))


(use-package flyckeck-popup-tip
  :after (flycheck-mode popup)
  :ensure nil
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))


;; Need shellcheck: apt install shellcheck
(use-package flymake-shellcheck
  :disabled t
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
  :hook ((text-mode) . flyspell-mode))


(use-package flyspell-prog
  :after (flyspell)
  :ensure nil
  :hook ((prog-mode) . flyspell-prog-mode))


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


;; jedi.el -- Autocompletion for python
;;
;; On first install, the following needs to be run within Emacs:
;;
;;   M-x jedi:install-server RET
(use-package jedi-core
  :after (company-jedi)
  :hook ((python-mode) . jedi:setup)
  :config
  (setq jedi:complete-on-dot t
        jedi:use-shortcuts t))


(use-package json-mode
  :mode "\\.json\\'" "\\.json.j2\\'"
  :init
  (setq js-indent-level 2))


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


(use-package neotree
  :init
  (setq neo-hidden-regexp-list '("^\\."
                                 "\\.cs\\.meta$"
                                 "\\.pyc$"
                                 "\\.egg-info$"
                                 "/?build$"
                                 "/?dist$"
                                 "~$"
                                 "^#.*#$"
                                 "\\.elc$"
                                 "/?__pycache__$"))
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t)
  (neotree-toggle))


(use-package org
  :init
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  :config
  (setq org-support-shift-select t))


(use-package plantuml-mode
  :if (not (version< emacs-version "25.0"))
  :init
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))


;; JavaScript code formatter
;;
;; $ sudo npm install -g prettier
(use-package prettier-js
  :config
  (setq prettier-js-args '("--print-width" "88")))


(use-package py-isort
  :init
  (add-hook 'before-save-hook 'py-isort-before-save))


;; For Flycheck: pip install flake8
(use-package python
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i"))
  (add-hook 'python-mode-hook 'flyspell-prog-mode))


(use-package pyvenv
  :disabled t
  :config
  (pyvenv-mode 1))


(use-package sql
  :config
  (sql-set-product 'postgres))


;; For PostgreSQL formatting, need apt install pgformatter
(use-package sqlformat
  :hook ((sql-mode) . sqlformat-on-save-mode)
  :init
  (require 'sqlformat)
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-f2" "-g" "-s2" "-U2")))


(use-package sql-upcase
  :ensure nil
  :init
  (ensure-downloaded-file
   "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/sql-upcase.el"
   (concat my-lispdir "sql-upcase.el"))
  :hook ((sql-mode sql-interactive-mode) . sql-upcase-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode for JavaScript, HTML, and CSS
;;
;; For linters, install the following:
;;
;;   $ sudo npm install -g eslint babel-eslint eslint-plugin-react
;;   $ sudo apt install tidy
;;   $ sudo npm install -g csslint
;;   $ sudo npm install -g tern
;;
(use-package web-mode
  :after (prettier-js)
  :mode "\\.css\\'" "\\.html?\\'" "\\.js\\'" "\\.j2\\'"
  :init
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-quoting t
        web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2
        web-mode-script-padding 2
        web-mode-style-padding 2)

  (add-to-list 'company-backends '(company-css
                                   company-web-html
                                   company-tern
                                   company-files))
  :config
  (defun my-web-mode-hook ()
    (cond ((string= web-mode-content-type "html")
           (when (executable-find "tidy")
             (flycheck-select-checker 'html-tidy)))
          ((string= web-mode-content-type "css")
           (when (executable-find "csslint")
             (flycheck-select-checker 'css-css-lint)))
          ((or (string= web-mode-content-type "javascript")
               (string= web-mode-content-type "jsx"))
           (when (executable-find "eslint")
             (flycheck-select-checker 'javascript-eslint))
           (web-mode-set-content-type "jsx")
           (prettier-js-mode)
           (tern-mode))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)

  (require 'flycheck)

  ;; Disable checkers not in use
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(json-jsonlist)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))


(use-package yaml-mode
  :hook ((yaml-mode) . remove-trailing-whitespaces-on-save)
  :mode "\\.ya?ml\\'" "\\.ya?ml.j2\\'")


(use-package yascroll
  :init
  (ensure-downloaded-file
   "https://raw.githubusercontent.com/emacsorphanage/yascroll/master/yascroll.el"
   (concat my-lispdir "yascroll.el"))
  (require 'yascroll)
  (global-yascroll-bar-mode 1))


;; Global prog-mode hook
(add-hook 'prog-mode-hook 'remove-trailing-whitespaces-on-save)
(add-hook 'rst-mode-hook 'remove-trailing-whitespaces-on-save)


;; Miscellanous settings
(setq sh-basic-offset 2
      sh-indentation 2)


(provide 'init)
;;; init.el ends here
