;;; Init.el1

;; ********************************************************
;; Appearance
;; --------------------------------------------------------

;; Disable the menu bas
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;; Disable the tool bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Disable the scroll bar
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Turn off annoying splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; When in doubt, use text-mode
(setq default-major-mode 'text-mode)

;; Show time using Swedish format
(setq display-time-day-and-format t
      display-time-24hr-format t)
(display-time)

;; Set a custom color theme
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(load-theme 'wombat t)

;; ********************************************************
;; Packages
;; --------------------------------------------------------

;; Use more package-archives (M-x list-packages)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			                   ("marmalade" . "https://marmalade-repo.org/packages/")
			                   ("melpa" . "https://melpa.org/packages/")))

;; Tell Emacs where to look for packages
(let ((default-directoy "~/.emacs.d/custom-packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(package-initialize)

(load-theme 'hc-zenburn)

;; ********************************************************
;; Navigation
;; --------------------------------------------------------

;; Show both line and column number in the bottom of the buffer
(column-number-mode t)

;; Show parenthesis matching the one below the cursor
(show-paren-mode t)

;; Show line numbers to the left of all buffers
; (global-linum-mode t)

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
(require 'avy)
(define-key global-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key global-map (kbd "C-M-j") 'avy-goto-char)
(define-key global-map (kbd "C-M-l") 'avy-goto-char)

;; Ace window
;; Select which window to switch to, rather than shuffling through them all
(require 'ace-window)
;(global-set-key (kbd "C-x o") 'ace-window)

;; imenu
;; Language-aware navigation
(setq imenu-auto-rescan t)
(global-set-key (kbd "C-.") 'imenu-anywhere)

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
;(require 'popup)
;(require 'auto-complete)
;(global-set-key (kbd "C-<return>")
;		(lambda () (interactive)
;		  (progn (auto-complete-mode 1) (auto-complete))))

;; Visual Regexp
;; Replace normal query-replace with a better one
(require 'visual-regexp)
(global-set-key "\M-%" 'vr/query-replace)

;; Multiple cursors
;; What it sounds like
;(require 'multiple-cursors)

;; Expand region
;; Select the thing I'm currently inside
(require 'expand-region)
(global-set-key (kbd "M-h") 'er/expand-region)

;; ********************************************************
;; Interface
;; --------------------------------------------------------

;; ido mode
;; Automatic auto-complete for many things, including opening files.
(ido-mode)

;; Don't automatically select things though...
(setq ido-auto-merge-delay-time 9999)

;; smex
;; M-x on steroids
(require 'smex)
(global-set-key "\M-x" 'smex)

;; Uniquify buffernames
;; Give better names to buffers of same name
(require 'uniquify)

;; Save-place
;; Remember the cursor position when you close a file, so that you
;; start  with the cursor in the same position when opening it again
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Recent files
;; Enable a command to list your most recently edited files. If you
;; know you are opening a file that you have edited recently, this
;; should be faster than using find-file ("C-x C-f"). The code below
;; binds this to the keyboard shortcut "C-x C-r", which replaces the
;; shortcut for the command find-file-read-only
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-meny-items 25)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key "\C-x\C-r" 'recentf-ido-find-file)

;; ********************************************************
;; Programming
;; --------------------------------------------------------

;; Flycheck (2020-12-28)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

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
(require  'yasnippet)
(yas-global-mode 1)


;; ********************************************************
;; Python
;; --------------------------------------------------------

(use-package elpy
             :ensure t
             :init
             (elpy-enable))

;; ********************************************************
;; Keybindings
;; --------------------------------------------------------



;; ********************************************************
;; Custom
;; --------------------------------------------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a37d20710ab581792b7c9f8a075fcbb775d4ffa6c8bce9137c84951b1b453016" default)))
 '(package-selected-packages
   (quote
    (elpy use-package hc-zenburn-theme smex visual-regexp flycheck yasnippet seq dash ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
