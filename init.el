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

;; HACK: DO NOT copy package-selected-packages to custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun machearn/save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'machearn/save-selected-packages)

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
(use-package use-package-chords
  :demand t
  :config (key-chord-mode 1))

(use-package gnu-elpa-keyring-update)

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
  (find-file "~/.config/emacs/init.el"))



(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode t)

(global-display-line-numbers-mode t)
(global-hl-line-mode t)

(setq display-line-numbers-type 'relative)
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
(setq-default fill-column '100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)


(use-package general)

(use-package restart-emacs)

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-solarized-dark t))

(use-package company
  :demand t
  :config
  (global-company-mode t)
  :bind
  ("C-c f" . 'company-files))

(use-package which-key
  :demand t
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.5))

(use-package popwin
  :config
  (popwin-mode)
  (setq popwin:popup-window-position 'right
        popwin:popup-window-width 0.4))

(use-package magit)

(use-package evil
  :demand t
  :config
  (evil-mode t)
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor  'hbar)
  (evil-set-initial-state 'occur-mode 'normal)
  (unless (display-graphic-p)
    (use-package evil-terminal-cursor-changer
      :config
      (evil-terminal-cursor-changer-activate)))
  :chords (("jk" . evil-normal-state)))

(use-package evil-surround
  :demand t
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-nerd-commenter
  :demand t
  :after evil
  :config (evilnc-default-hotkeys))

(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 15)
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  :bind (("C-c C-r" . 'ivy-resume)
         ("C-x C-o" . 'ivy-occur)))

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-posframe-display-at-frame-center)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-frame-center)
        (counsel-find-file . ivy-posframe-display-at-frame-center)
        (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
        (t               . ivy-posframe-display)))
  (ivy-posframe-mode 1))

(use-package counsel
  :bind (("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("C-x C-M-f" . 'counsel-fzf)
         ("C-x C-b" . 'counsel-ibuffer)
         ("<f1> f" . 'counsel-describe-function)
         ("<f1> v" . 'counsel-describe-variable)
         ("<f1> o" . 'counsel-describe-symbol)
         ("<f1> l" . 'counsel-find-library)
         ("<f2> i" . 'counsel-info-lookup-symbol)
         ("<f2> u" . 'counsel-unicode-char)
         ("C-c g" . 'counsel-git)
         ("C-c j" . 'counsel-git-grep)
         ("C-c k" . 'counsel-rg)
         ("C-x l" . 'counsel-locate)
         ("C-S-o" . 'counsel-rhythmbox)))

(use-package swiper
  :bind (("C-s" . 'swiper)))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  :hook ((prog-mode . smartparens-mode)))

(use-package rainbow-delimiters
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package expand-region
  :demand t
  :after evil
  :config
  (setq expand-region-contract-fast-key "j")
  :bind (:map evil-visual-state-map
         ("KK" . er/expand-region)))

(use-package iedit
  :bind (("M-s e" . 'iedit-mode)))

(use-package wgrep)

(use-package doom-modeline
  :demand t
  :config
  (doom-modeline-mode t))

(use-package window-numbering
  :demand t
  :config
  (window-numbering-mode t))

(use-package git-gutter
  :demand t
  :config
  (global-git-gutter-mode t))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package ace-window
  :bind ("M-o" . 'ace-window))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  ((c++-mode . lsp)
   (c-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :config
  (lsp-ui-mode t)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list)


(general-define-key
 "<f5>" 'open-init-file)
(general-define-key
 :states '(normal emacs insert)
 :keymaps 'occur-mode-map
 "C-x C-q" 'occur-edit-mode)
(general-define-key :states 'normal "U" 'evil-redo)
(general-define-key :states '(normal visual emacs) "[b" 'previous-buffer)
(general-define-key :states '(normal visual emacs) "]b" 'next-buffer)
(general-define-key :states 'normal "C-u" 'evil-scroll-up)
(general-create-definer my-leader-def
  :states '(normal insert visual emacs)
  :prefix "<SPC>"
  :non-normal-prefix "C-,")
(general-create-definer my-local-leader-def
  :prefix "M-s")
(my-local-leader-def
  "o" 'occur-dwim
  "i" 'counsel-imenu)
(my-leader-def
  :keymaps 'occur-mode-map
  "e" 'occur-edit-mode)

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
  "u" '(universal-argument :which-key "universal argument")
  ":" '(counsel-M-x :which-key "M-x"))



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
                      :height 200
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
