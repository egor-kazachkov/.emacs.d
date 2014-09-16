;; paths
;; On Win7 "~" == "C:\Users\<username>\AppData\Roaming"
(defvar emacs-d-path (expand-file-name ".emacs.d" "~"))

;; keep all custom code under ~/.emacs.d/elisp
(setq load-path (cons (expand-file-name "elisp" emacs-d-path) load-path))

(defvar cache-path (expand-file-name "cache" emacs-d-path))
(unless (file-exists-p cache-path) (make-directory cache-path))

;; backups & auto-save
(defvar backups-path (expand-file-name  "backups" cache-path))
(unless (file-exists-p backups-path) (make-directory backups-path))
(add-to-list 'backup-directory-alist (cons "." backups-path))
(setq make-backup-files t ;; do make backups
      backup-by-copying t     ;; and copy them here
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)
(setq auto-save-list-file-prefix (expand-file-name ".saves-" (expand-file-name "auto-save-list" cache-path)))

;; persistence
;; open files, positions, history
(require 'desktop)
(setq desktop-enable t
      desktop-save-mode 1
      desktop-base-file-name "emacs-desktop"
      desktop-dirname cache-path
      desktop-path (cons cache-path nil)
      history-length 256)
(setq desktop-globals-to-save (quote (tags-file-name tags-table-list search-ring
   regexp-search-ring file-name-history)))
;; window geometry http://www.gentei.org/~yuuji/software/revive.el
(require 'revive)
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(setq revive:configuration-file (expand-file-name "emacs-revive" cache-path))
(add-hook 'kill-emacs-hook  (lambda() (save-current-configuration 1)))
(add-hook 'after-init-hook  (lambda() (resume 1)))

;; general settings
;; transient-mark-mode changes many command to use only selected region when mark active
(setq transient-mark-mode t)

;; Use utf8 by default
(set-language-environment "UTF-8")

;; basic editing
(setq kill-whole-line t
      tab-width 4
      fill-column 92            ;; wrap lines after column 92 ;; TODO: Doesn't work - fix it
      auto-fill-mode t          ;; always turn on auto-fill mode
      delete-selection-mode 1   ;; delete the sel with a keyp
      require-final-newline t   ;; end files with a newline
      case-fold-search t        ;; ignore case when search interactively in lower case
      shift-select-mode nil     ;; disable shift+arrow select (as these keys needed by windmove/org mode)
      indent-tabs-mode nil)     ;; use only spaces for indentation

;; setup look&feel
;; use -mm switch to start Emacs in fullscreen mode
(setq display-time-24hr-format t)
(display-time)
(tool-bar-mode -1)            ;; turn-off toolbar 
(setq inhibit-startup-screen t   ;; turn-off welcome screen
      column-number-mode t
      size-indication-mode t)

;; colors and themes
;; Use Ubuntu Mono Regular 12 as default font for Linux (Ubuntu) and Courier for Windows
;; Need tuning for other systems
(if (eq system-type 'gnu/linux)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal))))))
(if (eq system-type 'windows-nt)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))))



;; use dark gray italic for comments 
(set-face-attribute font-lock-comment-face nil :slant 'italic :foreground "dimgray")
 ;; max decoration for all modes, rarely affect anything
(setq font-lock-maximum-decoration t)
;; show fill-column - http://www.emacswiki.org/emacs/download/fill-column-indicator.el
(require 'fill-column-indicator)
; TODO add fill-column 92
;(defun my-auto-fill-mode (&optional arg) (auto-fill-mode arg) (fci-mode arg))
;(defun my-auto-fill-mode() (auto-fill-mode) (fci-mode))

;; useful keyboard shortcuts
(global-set-key "\M-\C-r" 'query-replace)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-c M-q") 'auto-fill-mode) ;; TODO - change to my-auto-fill-mode
;; Treat 'y' or 'RET' as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
;; Since windmove doesn't work well with org-mode, define easy button for window switch.
(global-set-key   [f1]   'other-window)



;; Lang/mode specific settings

(defun untabify-on-save-hook ()
  "Hook to unabify buffer before saving. Add to specific major modes."
  (add-hook 'write-contents-functions (lambda () (untabify (point-min) (point-max)) nil))
  nil
)

;; Org mode
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-hierarchical-todo-statistics t)
(setq org-agenda-files (quote ("~/notes/notes.org")))
(add-hook 'org-mode-hook 'auto-fill-mode) ; TODO replace with my-auto-fill-mode
(add-hook 'org-mode-hook 'fci-mode)


;; Julia mode
(require 'julia-mode)
(add-hook 'julia-mode-hook 'untabify-on-save-hook)
;;(setq auto-mode-alist (append '(("\\.jl$"  . julia-mode)) auto-mode-alist ))

;; C/C++ mode
(setq c-basic-offset 4 ;; set default tab offset to 4 to all C-related modes
      c-default-style (quote ((c-mode . "stroustrup") 
			      (c++-mode . "stroustrup") 
			      (other . "stroustrup")))
      c-style-variables-are-local-p nil ;; make c-style related variables global
      c-syntactic-indentation t)

(add-hook 'c-mode-common-hook 'untabify-on-save-hook)

;;  '(c-tab-always-indent nil)


;; Git extensions for vc-git, from http://snarfed.org/emacs-vc-git-tweaks
;; In vc-git and vc-dir for git buffers, 
;; make (C-x v) a run git add, u run git
;; reset, and r run git reset and checkout from head.
(defun my-vc-git-command (verb fn)
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

(defun my-vc-git-add (&optional revision vc-fileset comment)
  (interactive "P")
  (my-vc-git-command "Staged" 'vc-git-register))

(defun my-vc-git-reset (&optional revision vc-fileset comment)
  (interactive "P")
  (my-vc-git-command "Unstaged"
    (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))


(define-key vc-prefix-map [(r)] 'vc-revert-buffer)
;;(define-key vc-dir-mode-map [(r)] 'vc-revert-buffer)
(define-key vc-prefix-map [(a)] 'my-vc-git-add)
;;(define-key vc-dir-mode-map [(a)] 'my-vc-git-add)
(define-key vc-prefix-map [(u)] 'my-vc-git-reset)
;;(define-key vc-dir-mode-map [(u)] 'my-vc-git-reset)
;; hide up to date files after refreshing in vc-dir
;;(define-key vc-dir-mode-map [(g)]
;;  (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))


;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(cperl-continued-statement-offset 4)
;;  '(cperl-indent-level 4)
;;  '(current-language-environment "UTF-8")
;;  '(custom-buffer-indent 4)
;;  '(fill-individual-varying-indent t)
;;  '(global-font-lock-mode t nil (font-lock))
;;  '(hi-lock-mode t t (hi-lock))
;;  '(indent-tabs-mode nil)

;;  '(lisp-tag-body-indentation 4)
;;  '(mail-indentation-spaces 4)
;;  '(normal-erase-is-backspace t)
;;  '(octave-auto-indent nil)
;;  '(octave-block-offset 4)
;;  '(parse-sexp-ignore-comments t)
;;  '(save-place t nil (saveplace))
;;  '(tab-always-indent nil)
;;  '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100)))
;;  '

;;  '(x-select-enable-clipboard t))


;; 
;; 
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

;; 


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(put 'dired-find-alternate-file 'disabled nil)
