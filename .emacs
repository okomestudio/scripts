(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(fringe-mode 0 nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(global-whitespace-mode nil)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(show-paren-mode t nil (paren))
 '(size-indication-mode t)
 '(tab-always-indent t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CONFIGS BY TS
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24) ; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT

;; for clipboard
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;(set-face-attribute 'default nil :height 75)
(setq ring-bell-function 'ignore) ; disable beeping

;; for frame-cmds.el
(setq frame-title-format '("" "%f"))


;; Dired -- ignore some files
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.pyo$\\|\#$")
;;(setq dired-omit-files "^\\.[^.]\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.pyo$\\|\#$")


;; autocomplete
;; (unless (package-installed-p 'auto-complete)
;;   (package-refresh-contents) (package-install 'auto-complete))
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-show-menu-immediately-on-auto-complete t)

;; NeoTree
(unless (package-installed-p 'neotree)
  (package-refresh-contents) (package-install 'neotree))
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Every time when the neotree window is opened, let it find current
;; file and jump to node.
(setq neo-smart-open t)

;; Open the Neo Tree window on start.
(neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell/BASH

(defun setup-sh-mode ()
  (interactive)
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook 'setup-sh-mode)
(add-to-list 'auto-mode-alist '("\\.bats\\'" . sh-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

(setq c-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq c-indent-level 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVASCRIPT

;; for javascript.el
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;;(autoload 'javascript-mode "javascript" nil t)


;; for js2 mode
;;(autoload 'js2-mode "js2" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; js2 major mode
;; ref: http://8-p.info/emacs-javascript.html
;; (when (load "js2" t)
;;   (setq js2-basic-offset 2
;;         js2-cleanup-whitespace nil
;;         js2-mirror-mode nil
;;         js2-bounce-indent-flag t)
  
;;   (defun indent-and-back-to-indentation ()
;;     (interactive)
;;     (indent-for-tab-command)
;;     (let ((point-of-indentation
;;            (save-excursion
;;              (back-to-indentation)
;;              (point))))
;;       (skip-chars-forward "\s " point-of-indentation)))
;;   (define-key js2-mode-map "\C-i" 'indent-and-back-to-indentation)
  
;;   (define-key js2-mode-map "\C-m" nil)
  
;;   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; (global-set-key "\C-m" 'newline-and-indent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML/CSS

;; recognize .tpl as .html
;; (add-to-list 'auto-mode-alist '("\\.tpl$" . html-helper-mode))

;; css-mode
;;(setq cssm-indent-function #'cssm-c-style-indenter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON

(unless (package-installed-p 'python)
  (package-refresh-contents) (package-install 'python))
(require 'python)



;; (global-font-lock-mode 1)



;; (load-library "python")

;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (setq interpreter-mode-alist
;;       (cons '("python" . python-mode)
;;             interpreter-mode-alist)
;;       python-mode-hook
;;       '(lambda () (progn
;;                     (set-variable 'py-indent-offset 4)
;;                     (set-variable 'indent-tabs-mode nil))))



;; automatically remove trailing whitespace when file is saved
(add-hook 'python-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))

;; use ipython if available
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i"))


;; cython-mode
(unless (package-installed-p 'cython-mode)
  (package-refresh-contents) (package-install 'cython-mode))
(require 'cython-mode)


;; jedi.el -- autocomplete for python
;;
;;   M-x jedi:install-server RET
(unless (package-installed-p 'epc)
  (package-refresh-contents) (package-install 'epc))
(require 'epc)

(unless (package-installed-p 'jedi)
  (package-refresh-contents) (package-install 'jedi))
(require 'jedi)

(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method '(popup))

;; if using multiple virtual env, this might become useful:
;;
;;   http://stackoverflow.com/questions/21246218/how-can-i-make-emacs-jedi-use-project-specific-virtualenvs

(setq jedi:server-args (list (or (buffer-file-name) default-directory)))
(push "--sys-path" jedi:server-args)
(message "for jedi:server-args %s" jedi:server-args)


;; flymake - Python code checking.
(unless (package-installed-p 'flymake-cursor)
  (package-refresh-contents) (package-install 'flymake-cursor))
(require 'flymake)
(load-library "flymake-cursor")
(setq pycodechecker "pychecker") ; script that flymake uses to check code.
(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))
(add-hook 'python-mode-hook 'flymake-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS

(defun ts/insert-line-before (times)
  "Insert a newline(s) above the line containing the cursor."
  (interactive "p") ; called from M-x
  (save-excursion ; store position
    (move-beginning-of-line 1)
    (newline times))) ; insert new line
(global-set-key (kbd "C-S-o")
                'ts/insert-line-before)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM KEYBINDINGS

;; window resize
(global-set-key [C-s-left] 'shrink-window-horizontally)
(global-set-key [C-s-right] 'enlarge-window-horizontally)
(global-set-key [C-s-down] 'shrink-window)
(global-set-key [C-s-up] 'enlarge-window)

;; window switch (in place of C-x-o)
(define-key global-map "\C-q" (make-sparse-keymap)) ; define prefix
(global-set-key "\C-qh" 'windmove-left)
(global-set-key "\C-qj" 'windmove-down)
(global-set-key "\C-qk" 'windmove-up)
(global-set-key "\C-ql" 'windmove-right)

;; Ace Jump -- Enables fast/direct cursor movement in current view.
;;
;; Example: (1) type C-c SPC, (2) type a character, and (3) type one
;; of the highlighted characters to move to that location in view.
;;
(unless (package-installed-p 'ace-jump-mode)
  (package-refresh-contents) (package-install 'ace-jump-mode))
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC.
