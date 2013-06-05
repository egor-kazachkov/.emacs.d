;; paths
;; keep all custom code under ~/.emacs.d/elisp
(setq load-path (cons (concat (concat "/home/" user-login-name) "/.emacs.d/elisp") load-path))

;; backups
(setq make-backup-files t ;; do make backups
  backup-by-copying t     ;; and copy them here
  backup-directory-alist '(("." . "~/.emacs.d/cache/backups")) 
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)


;; basic editing
(setq kill-whole-line t
      tab-width 4
      auto-fill-mode t          ;; always turn on auto-fill mode
      delete-selection-mode 1   ;; delete the sel with a keyp
      require-final-newline t)  ;; end files with a newline
(set-fill-column 92)            ;; wrap lines after column 92

;; setup info bars and look&feel
(setq display-time-24hr-format t)
(display-time)
(setq tool-bar-mode -1            ;; turn-off toolbar 
      inhibit-startup-screen t)   ;; turn-off welcome screen
;; colors and themes
(set-face-foreground font-lock-comment-face "dimgray")

;; useful keyboard shortcuts
(global-set-key "\M-\C-r" 'query-replace)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-ca" 'org-agenda)
;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(c-basic-offset 4)
;;  '(c-default-style (quote ((c-mode . "stroustrup") (c++-mode . "stroustrup") (other . "stroustrup"))))
;;  '(c-style-variables-are-local-p nil)
;;  '(c-syntactic-indentation t)
;;  '(c-tab-always-indent nil)
;;  '(case-fold-search t)
;;  '(column-number-mode t)
;;  '(cperl-continued-statement-offset 4)
;;  '(cperl-indent-level 4)
;;  '(current-language-environment "UTF-8")
;;  '(custom-buffer-indent 4)
;;  '(desktop-globals-to-save (quote (desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history)))
;;  '(desktop-restore-eager 4)
;;  '(desktop-save-mode t)
;;  '(fill-individual-varying-indent t)
;;  '(global-font-lock-mode t nil (font-lock))
;;  '(hi-lock-mode t t (hi-lock))
;;  '(indent-tabs-mode nil)

;;  '(lisp-tag-body-indentation 4)
;;  '(mail-indentation-spaces 4)
;;  '(normal-erase-is-backspace t)
;;  '(octave-auto-indent nil)
;;  '(octave-block-offset 4)
;;  '(org-agenda-files (quote ("~/notes/notes.org")))
;;  '(org-support-shift-select t)
;;  '(parse-sexp-ignore-comments t)
;;  '(save-place t nil (saveplace))
;;  '(tab-always-indent nil)
;;  '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100)))
;;  '
;;  '(transient-mark-mode t)
;;  '(winsav-handle-also-desktop nil)
;;  '(winsav-save-mode t)
;;  '(x-select-enable-clipboard t))

;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))

;;  '(font-lock-keyword-face ((((class color) (min-colors 8)) (:foreground "orange")))))


;; 


;; (setq font-lock-maximum-decoration t)
;; (size-indication-mode t)
;; ;
;; (add-hook 'c-mode-hook 'turn-on-font-lock)
;; (setq shift-select-mode nil)
;; ; -- system specifics --
;; ;enable this for specific remote terminals
;; ;(normal-erase-is-backspace-mode)
;; ; use only under X
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


;; ; enabled 'dangerous' commands
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)

;; ; packages setup
;; (require 'org-inlinetask)

;; (windmove-default-keybindings)
;; ;; Make windmove work in org-mode:
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)

;; (require 'winsav)


;; ; -- language specs --
;; (setq auto-mode-alist
;;       (append '(("\\.m$"  . octave-mode)) auto-mode-alist ))

;; ;; for R
;; (require 'ess-site)
;; (setq auto-mode-alist
;;       (append '(("\\.R$"  . R-mode)) auto-mode-alist ))

;; ;; for LaTeX
;; ;(load "auctex.el" nil t t)
;; ;(require 'auctex)
;; ;(setq auto-mode-alist
;; ;      (append '(("\\.latex$"  . latex-mode)) auto-mode-alist ))

;; ;; for ORG-mode
;; (setq org-hierarchical-todo-statistics t)


; haskell setup
;(setq auto-mode-alist
;      (append auto-mode-alist
;              '(("\\.[hg]s$"  . haskell-mode)
;                ("\\.hi$"     . haskell-mode)
;                ("\\.l[hg]s$" . literate-haskell-mode))))
;(autoload 'haskell-mode "haskell-mode"
;   "Major mode for editing Haskell scripts." t)
;(autoload 'literate-haskell-mode "haskell-mode"
;   "Major mode for editing literate Haskell scripts." t)

;adding the following lines according to which modules you want to use:
;(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
