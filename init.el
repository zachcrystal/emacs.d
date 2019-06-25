;; Package Config
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; UI Config
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-frame-font "Hack 10" nil t)
(global-hl-line-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Editor
(setq-default
 indent-tabs-mode nil
 tab-width 2
 fill-column 80)
(show-paren-mode t)

;; Misc Config
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)
 
;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Crux
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("C-c I" . crux-find-user-init-file)
         ("C-<backspace>" . crux-kill-whole-line-backwards)
         ("C-S-o" . crux-smart-open-line-above)
         ("C-o" . crux-smart-open-line)))


;; Theme
(use-package atom-one-dark-theme
  :init(load-theme 'atom-one-dark t))

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Helm/Projectile
(use-package helm
  :bind
  (("C-c h" . helm-command-prefix)
   ("C-x b" . helm-mini)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files))
  :config
  (require 'helm-config)
  (bind-key "C-c h" helm-command-prefix)
  (setq helm-quick-update t
	helm-autoresize-mode t
	helm-ff-skip-boring-files t)
  (helm-mode t))

;; Describe bindings
(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds)
  :config
  (helm-descbinds-mode))

(use-package projectile
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project))
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm
	projectile-enable-caching t
	projectile-indexing-method 'alien))

(use-package helm-projectile
  :config
  (helm-projectile-on))

;; Movement
(use-package avy
  :bind (("C-'" . avy-goto-char-2)
	 ("M-g f" . avy-goto-line))
  :config
  (setq avy-background t))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

(use-package adaptive-wrap
  :config
  (setq-default adaptive-wrap-extra-indent 1)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  (global-visual-line-mode +1))

;; Prog
(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package aggressive-indent)

;(use-package rainbow-delimiters
 ; :hook (prog-mode . rainbow-delimiters-mode))

;; Web
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config
  (set-face-background 'web-mode-current-element-highlight-face "#3E4451")
  (set-face-foreground 'web-mode-current-element-highlight-face nil)
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 4
	web-mode-enable-css-coloraization t
	web-mode-enable-auto-pairing t
	web-mode-enable-current-element-highlight t))

(use-package css-mode
  :custom (css-indent-offset 2))

(use-package rainbow-mode
  :hook (prog-mode))

(use-package emmet-mode
  :hook (css-mode web-mode))

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/usr/bin/fish")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; Org
(use-package org-bullets
  :config (progn (add-hook 'org-mode-hook
			   (lambda ()
			     (org-bullets-mode 1)))))
