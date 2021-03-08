;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;  Emacs Startup File --- initialization for Emacs

;;; Code:

(let ((minver "25.3.2"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too olds - this config requires v%d or higher" minver)))

;; ********************************************************
;; Packages
;; --------------------------------------------------------

;; Use more package-archives (M-x list-packages)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/")
       ("marmalade" . "https://marmalade-repo.org/packages/")
       ("melpa" . "https://melpa.org/packages/")))

;; Tell Emacs where to look for packages
(let ((default-directoy "~/.emacs.d/custom-packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(package-initialize)

;; Fetch the list of package available
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

;; ********************************************************
;; Appearance
;; --------------------------------------------------------

;; Disable the menu bar
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;; Disable the tool bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Disable the scroll bar
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Turn off splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; Use the entire screen
(setq scroll-margin 0)

;; When in doubt, use text-mode
(setq major-mode 'text-mode)

;; Show time using Swedish format
(setq display-time-day-and-format t
      display-time-24hr-format t)
(display-time)

(use-package hc-zenburn-theme
  :ensure t
  :init
  (setq hc-zenburn t)
  :config
  (load-theme 'hc-zenburn t))

;; ********************************************************
;; Navigation
;; --------------------------------------------------------

;; Show both line and column number in the bottom of the buffer
(column-number-mode t)

;; Show parenthesis matching the one below the cursor
(show-paren-mode t)

;; Show line numbers to the left of all buffers
(global-linum-mode t)
(setq linum-format "%d ")

;; Sentences are not followed by two spaces
;; Makes navigating with M-e  and M-a (forward/backward sentence)
;; behave like you would expect
(setq sentence-end-double-space nil)

;; C-SPC after C-u C-SPC cycles mark stack
(setq-default set-mark-command-repeat-pop t)

;; Move entire paragraph with M-n/M-p
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)

;; Back to indentation with M-a
(global-set-key "\M-a" 'back-to-indentation)

;; Avy mode
;; Jump anywhere on screen in four keystrokes or less.
(use-package avy
   :ensure t
   :bind (("C-j" . avy-goto-word-or-subword-1)
    ("C-M-j" . avy-goto-char)
    ("C-M-l" . avy-goto-char))
   :config
   (setq avy-background t))

;; Ace window
;; Select which window to switch to, rather than shuffling through them all
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

;; imenu
;; Language-aware navigation
(use-package imenu
  :ensure t
  :config
  (setq imenu-auto-rescan t)
  :bind
  (("M-i" . idomenu)))

;; ********************************************************
;; Editing
;; --------------------------------------------------------

;; Allow deletion of selected text with <DEL>
(delete-selection-mode 1)

;; Use multiple spaces instead of tab characters
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 66)

;; hippie-expand instead of dabbrev-expand
;; dabbrev-expand will try to expand the word under the cursor by
;; searching your open buffers for words beginning with the same
;; characters. For example, if you have written "printf" in an open
;; buffer you can just write "pr" and expand it to the full
;; word. hippie-expand does the same kind of search, plus some
;; additional searching, such as in your kill ring or the names of the
;; files you have open.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Alternative mapping
; (global-set-key [(control tab)] 'hippie-expand)

;; Expand from everything!
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-dabbrev-from-kill
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Auto complete mode
;; Always suggest completions.
(use-package auto-complete
  :ensure t)

(use-package auto-complete-config
  :ensure auto-complete
  :bind ("M-<tab>" . my--auto-complete)
  :init
  (defun my--auto-complete ()
    (interactive)
    (unless (boundp 'auto-complete-mode)
      (global-auto-complete-mode 1))
    (auto-complete)))

;; Visual Regexp
;; Replace normal query-replace with a better one
(use-package visual-regexp
  :ensure t
  :bind ("\M-%" . vr/query-replace))

;; Expand region
;; Select the thing I'm currently inside
(use-package expand-region
  :ensure t
  :bind ("M-h" . er/expand-region))

;; ********************************************************
;; Interface
;; --------------------------------------------------------

(use-package git-gutter-fringe+
  :ensure t)

;; Setup GitGutter+
(use-package git-gutter+
  :ensure t
  :config
  (progn
    (when (not (equal (window-system) nil))
      (require 'git-gutter-fringe+))
    (global-git-gutter+-mode t)
    (setq git-gutter+-hide-gutter t))
  :diminish git-gutter+
  :bind
  (("C-x g t" . git-gutter+-mode)
   ("C-x g n" . git-gutter+-next-hunk)
   ("C-x g p" . git-gutter+-previous-hunk)
   ("C-x g v" . git-gutter+-revert-hunk)))

;; ido mode
;; Automatic auto-complete for many things, including opening files.
(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10))

;; Don't automatically select things though...
(setq ido-auto-merge-delay-time 9999)

;; smex
;; Fuzzy matching for M-x
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . execute-extended-command)))

;; Uniquify buffernames
;; Give better names to buffers of same name
(use-package uniquify)

;; Save-place
;; Remember the cursor position when you close a file, so that you
;; start with the cursor in the same position when opening it again
(use-package saveplace
  :ensure t
  :init (save-place-mode))

;; Recent files
;; Enable a command to list your most recently edited files. If you
;; know you are opening a file that you have edited recently, this
;; should be faster than using find-file ("C-x C-f"). The code below
;; binds this to the keyboard shortcut "C-x C-r", which replaces the
;; shortcut for the command find-file-read-only
(use-package recentf
  :ensure t
  :config
  (recentf-mode t))

(setq recentf-max-meny-items 25)
;(setq recentf-max-saved-items 50)

;(defun recentf-ido-find-file ()
;  "Find a recent file using Ido."
;  (interactive)
;  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
;    (when file
;      (find-file file))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
        (message "Aborting")))

;(global-set-key "\C-x\C-r" 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; ********************************************************
;; Programming
;; --------------------------------------------------------

;; Flycheck (2020-12-28)
(use-package flycheck
  :ensure t
  ;:init
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Key binding for finding next error
(dolist (hook '(prog-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-n")
                             'next-error))))

;; Key binding for finding previous error
(dolist (hook '(prog-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-p")
                             'previous-error))))

;; YASnippet
;; Expand e.g. "for<tab>" to "for(int i = 0; i < N; i++) {}"
(use-package  yasnippet
  :ensure t)
  ;:config (yas-global-mode 1))


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

;; ********************************************************
;; C
;; --------------------------------------------------------

;; Settings for C
(add-hook 'c-mode-common-hook
          (lambda ()
            (progn
              (setq comment-start "//")
              (setq comment-end "")
              (global-company-mode)
              (flycheck-mode t)
              )))

;; Trying out irony-mode (2017-10-10)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;(use-package flycheck-rtags
;  :ensure t)

; Highlight dangerous C functions
(add-hook 'c-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(malloc\\|calloc\\|free\\|realloc\\)\\>"
                                       . font-lock-warning-face)))))

;; ********************************************************
;; Python
;; --------------------------------------------------------

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setq elpy-rpc-python-command "python3"))

;; Use backend jedi for autocomplete
(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode)))

;; ********************************************************
;; Custom
;; --------------------------------------------------------

;;; init.el ends here
