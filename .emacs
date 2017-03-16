;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; melpa archive could be used to install following packages with M-x list-package
;;  mark package to install (i), delete (d); execute install/delete (x)
;; Packages to install:
;;
;;    volatile-highlights
;;    undo-tree
;;    projectile
;;    helm
;;    helm-projectile

(package-initialize)

(require 'volatile-highlights)
(volatile-highlights-mode t)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'projectile)
(projectile-mode)

;;================================================================

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

;; (defun spacemacs//helm-hide-minibuffer-maybe ()
;;   "Hide minibuffer in Helm session if we use the header line as input field."
;;   (when (with-helm-buffer helm-echo-input-in-header-line)
;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;       (overlay-put ov 'window (selected-window))
;;       (overlay-put ov 'face
;;                    (let ((bg-color (face-background 'default nil)))
;;                      `(:background ,bg-color :foreground ,bg-color)))
;;       (setq-local cursor-type nil))))


;; (add-hook 'helm-minibuffer-set-up-hook
;;           'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

;;================================================================

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

;;================================================================

;; persistence
;; open files, positions, history
(require 'desktop)
(setq desktop-enable t
      desktop-save-mode 1
      desktop-base-file-name "emacs-desktop"
      desktop-dirname cache-path
      desktop-path (cons cache-path nil)
      history-length 256)
(setq desktop-globals-to-save (quote (tags-file-name
				      tags-table-list
				      search-ring
				      register-alist
				      extended-command-history
				      regexp-search-ring
				      file-name-history)))
;; window geometry http://www.gentei.org/~yuuji/software/revive.el
(require 'revive)
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(setq revive:configuration-file (expand-file-name "emacs-revive" cache-path))
(add-hook 'kill-emacs-hook  (lambda() (save-current-configuration 1)))
(add-hook 'after-init-hook  (lambda() (resume 1)))

;;================================================================

;; general settings
;; transient-mark-mode changes many command to use only selected region when mark active
(setq transient-mark-mode t)
;; disable any beeps. To select what should ring and what not, see 
;; http://stackoverflow.com/questions/11679700/emacs-disable-beep-when-trying-to-move-beyond-the-end-of-the-document
(setq ring-bell-function 'ignore)

(setq column-number-mode t)
(setq display-time-mode t)
(setq size-indication-mode t)
(setq tool-bar-mode nil)
(put 'dired-find-alternate-file 'disabled nil)

;; update any change made on file to the current buffer
(global-auto-revert-mode)

;;================================================================

;; basic editing
(setq kill-whole-line t
      fill-column 92            ;; wrap lines after column 92 ;; TODO: Doesn't work - fix it
      auto-fill-mode t          ;; always turn on auto-fill mode
      global-mark-ring-max 5000 ;; increase mark ring to contains 5000 entries
      mark-ring-max 5000        ;; increase kill ring to contains 5000 entries
      kill-ring-max 5000        ;; increase kill-ring capacity
      delete-selection-mode 1   ;; delete the sel with a keyp
      require-final-newline t   ;; end files with a newline
      case-fold-search t        ;; ignore case when search interactively in lower case
      shift-select-mode nil     ;; disable shift+arrow select (as these keys needed by windmove/org mode)
      indent-tabs-mode nil)     ;; use only spaces for indentation

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Use utf8 by default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)



;; useful keyboard shortcuts
(global-set-key "\M-\C-r" 'query-replace)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-c M-q") 'auto-fill-mode) ;; TODO - change to my-auto-fill-mode
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [f1]   'other-window) ;; easy button for window switch.

(fset 'yes-or-no-p 'y-or-n-p) ;; Treat 'y' or 'RET' as yes, 'n' as no.
(define-key query-replace-map [return] 'act)

;;================================================================

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

;;================================================================


;; Lang/mode specific settings

(defun untabify-on-save-hook ()
  "Hook to unabify buffer before saving. Add to specific major modes."
  (add-hook 'write-contents-functions (lambda () (untabify (point-min) (point-max)) nil))
  nil
)

;; All prog modes
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Org mode
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-hierarchical-todo-statistics t)
(setq org-agenda-files (quote ("~/notes/notes.org")))
(add-hook 'org-mode-hook 'auto-fill-mode) ; TODO replace with my-auto-fill-mode
(add-hook 'org-mode-hook 'fci-mode)

;; Shell mode
(require 'dirtrack)
(add-hook 'shell-mode-hook
      (lambda ()
        (shell-dirtrack-mode 0) ;stop the usual shell-dirtrack mode
	(setq dirtrack-list '("^.*:\\(.*\\)[$|#]" 1))
        ;(dirtrack-debug-mode) ;this shows any change in directory that dirtrack mode sees
        (dirtrack-mode 1)))

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

;; Python mode
(setq auto-mode-alist (append '(("\\.parts$"  . python-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("SConstruct"  . python-mode)) auto-mode-alist))
(setq python-indent-guess-indent-offset nil)
(setq python-indent-offset 4)

;; Git extensions for vc-git, from http://snarfed.org/emacs-vc-git-tweaks
;; In vc-git and vc-dir for git buffers, 
;; make (C-x v) a run git add, u run git
;; reset, and r run git reset and checkout from head.
(require 'vc-dir)

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
(define-key vc-dir-mode-map [(r)] 'vc-revert-buffer)
(define-key vc-prefix-map [(a)] 'my-vc-git-add)
(define-key vc-dir-mode-map [(a)] 'my-vc-git-add)
(define-key vc-prefix-map [(u)] 'my-vc-git-reset)
(define-key vc-dir-mode-map [(u)] 'my-vc-git-reset)
;; hide up to date files after refreshing in vc-dir
(define-key vc-dir-mode-map [(g)]
  (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))


;; show important whitespace in diff-mode
(add-hook 'diff-mode-hook
	  (lambda ()
	    (setq-local whitespace-style
			'(face
			  tabs
			  tab-mark
			  spaces
			  space-mark
			  trailing
			  indentation::space
			  indentation::tab
			  newline
			  newline-mark))
	    (whitespace-mode 1)))

; 
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))
