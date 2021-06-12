(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(defvar machearn/gc-cons-threshold (if (display-graphic-p) 64000000 1600000))
(defvar machearn/gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000))
(defvar machearn/gc-timer (run-with-idle-timer 10 t #'garbage-collect))
(defvar default/file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold machearn/gc-cons-upper-limit
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist default/file-name-handler-alist)
	    (setq gc-cons-threshold machearn/gc-cons-threshold
		  gc-cons-percentage 0.1)
	    (if (boundp 'after-focus-change-function)
		(add-function :after after-focus-change-function
			      (lambda ()
				(unless (frame-focus-state)
				  (garbage-collect))))
	      (add-hook 'focus-out-hook 'garbage-collect))
	    (defun machearn/mimibuffer-setup-hook ()
	      (setq gc-cons-threshold machearn/gc-cons-upper-limit))
	    (defun machearn/minibuffer-exit-hook ()
	      (setq gc-cons-threshold machearn/gc-cons-threshold))
	    (add-hook 'minibuffer-setup-hook #'machearn/mimibuffer-setup-hook)
	    (add-hook 'minibuffer-exit-hook #'machearn/minibuffer-exit-hook)))

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-key)

(require 'cl)

(defvar machearn/custom-packages '(
			  company
			  solarized-theme
			  evil
			  evil-surround
			  evil-nerd-commenter
			  hungry-delete
			  key-chord
			  evil-terminal-cursor-changer
			  ivy
			  swiper
			  counsel
			  smartparens
			  rainbow-delimiters
			  popwin
			  expand-region
			  general
			  iedit
			  wgrep
			  doom-modeline
			  doom-themes
			  window-numbering
			  which-key
			  magit
			  )  "Default packages")

(setq package-selected-packages machearn/custom-packages)

(defun machearn/custom-packages-installed-p ()
  (loop for pkg in machearn/custom-packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(defun occur-dwim ()
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(unless (machearn/custom-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg machearn/custom-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode t)
(window-numbering-mode t)

(global-linum-mode t)
(global-hl-line-mode t)

(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(setq ring-bell-function 'ignore)
(set-face-attribute 'default nil
		    :family "Ubuntu Mono"
		    :height 150
		    :weight 'normal
		    :width 'normal)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(setq popwin:popup-window-position 'right)
(setq popwin:popup-window-width '90)



(load-theme 'doom-solarized-dark t)

(global-company-mode t)
(which-key-mode t)

(require 'hungry-delete)
(global-hungry-delete-mode t)

(require 'evil)
(evil-mode t)
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))
(setq evil-motion-state-cursor 'box)
(setq evil-visual-state-cursor 'box)
(setq evil-normal-state-cursor 'box)
(setq evil-insert-state-cursor 'bar)
(setq evil-emacs-state-cursor  'hbar)
(evil-set-initial-state 'occur-mode 'normal)

(require 'general)
(general-define-key
 "<f5>" 'open-init-file)
(general-define-key
 :states '(normal emacs insert)
 :keymaps 'occur-mode-map
 "C-x C-q" 'occur-edit-mode)
(general-define-key :states 'normal "U" 'evil-redo)
(general-create-definer my-leader-def
  :states '(normal insert visual emacs)
  :prefix "<SPC>"
  :non-normal-prefix "C-,")
(general-create-definer my-local-leader-def
  ;; :prefix local leader
  :prefix "M-s")
(my-local-leader-def
  "o" 'occur-dwim
  "i" 'counsel-imenu)
(my-leader-def
  :keymaps 'occur-mode-map
  "e" 'occur-edit-mode)

(require 'key-chord)
(key-chord-mode t)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 15)
(evil-set-initial-state 'ivy-occur-grep-mode 'normal)
(general-define-key "C-s" 'swiper)
(general-define-key "C-c C-r" 'ivy-resume)
(general-define-key "<f6>" 'ivy-resume)
(general-define-key "M-x" 'counsel-M-x)
(general-define-key "C-x C-f" 'counsel-find-file)
(general-define-key "C-x C-S-f" 'counsel-fzf)
(general-define-key "C-x C-b" 'counsel-ibuffer)
(general-define-key "<f1> f" 'counsel-describe-function)
(general-define-key "<f1> v" 'counsel-describe-variable)
(general-define-key "<f1> o" 'counsel-describe-symbol)
(general-define-key "<f1> l" 'counsel-find-library)
(general-define-key "<f2> i" 'counsel-info-lookup-symbol)
(general-define-key "<f2> u" 'counsel-unicode-char)
(general-define-key "C-c g" 'counsel-git)
(general-define-key "C-c j" 'counsel-git-grep)
(general-define-key "C-c k" 'counsel-rg)
(general-define-key "C-x l" 'counsel-locate)
(general-define-key "C-S-o" 'counsel-rhythmbox)
(general-define-key "C-x C-o" 'ivy-occur)

(my-leader-def
  "f" '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find file")
  "fp" '(open-init-file :which-key "init file")
  "fz" '(counsel-fzf :which-key "fzf")
  "fg" '(counsel-git :which-key "git project")
  "b" '(:ignore t :which-key "buffer")
  "bk" '(kill-buffer :which-key "kill buffer")
  "bd" '(kill-current-buffer :which-key "kill current buffer")
  "bb" '(counsel-ibuffer :which-key "switch buffer")
  "bn" '(switch-to-next-buffer :which-key "next buffer")
  "bp" '(switch-to-prev-buffer :which-key "prev buffer")
  "w" '(:ignore t :which-key "window")
  "wh" '(windmove-left :which-key "move left")
  "wj" '(windmove-down :which-key "move down")
  "wk" '(windmove-up :which-key "move up")
  "wl" '(windmove-right :which-key "move right")
  "wd" '(:ignore t :which-key "delete")
  "wdd" '(delete-window :which-key "delete current")
  "wdh" '(windmove-delete-left :which-key "delete left")
  "wdj" '(windmove-delete-down :which-key "delete down")
  "wdk" '(windmove-delete-up :which-key "delete up")
  "wdl" '(windmove-delete-right :which-key "delete right")
  "ws" '(:ignore t :which-key "split")
  "wsh" '(split-window-below :which-key "split below")
  "wsv" '(split-window-right :which-key "split right")
  "s" '(:ignore t :which-key "search")
  "sb" '(swiper :which-key "search buffer")
  "sp" '(counsel-rg :which-key "search dir")
  "g" '(:ignore t :which-key "magit")
  "gg" '(magit-status :which-key "magit status")
  ":" '(counsel-M-x :which-key "M-x"))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)

(require 'popwin)
(popwin-mode t)

(require 'expand-region)
(with-eval-after-load 'evil
  (setq expand-region-contract-fast-key "j"))
(general-define-key :keymaps 'visual "KK" 'er/expand-region)

(require 'iedit)
(my-local-leader-def
  "e" 'iedit-mode)

(require 'doom-modeline)
(doom-modeline-mode t)
(setq doom-modeline-minor-modes t)

(require 'evil-surround)
(global-evil-surround-mode t)

(evilnc-default-hotkeys)



(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (general-define-key "s-c" 'kill-ring-save)
  (general-define-key "s-x" 'kill-region)
  (general-define-key "s-v" 'yank)
  (general-define-key "s-a" 'mark-whole-buffer)
  (general-define-key "s-z" 'undo)
  (general-define-key "s-s" 'save-buffer)
  (set-face-attribute 'default nil
		    :family "Ubuntu Mono"
		    :height 180
		    :weight 'normal
		    :width 'normal))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
