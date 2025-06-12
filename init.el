;; use more memory (LSP recommendations)
(setq gc-cons-threshold 100000000
read-process-output-max (* 1024 1024)) ;; 1mb

(eval-when-compile
  (require 'use-package))
;; always install missing dependencies
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
;; (package-initialize)

;; load time profiling
;; (setq use-package-compute-statistics t)

;; ignore cites in markdown
;; - [ ] TODO: fixme?
(add-to-list 'ispell-skip-region-alist '("@\\w*"))

;; modeline
(setq-default mode-line-format
              '((:eval (when (file-remote-p default-directory)
                         "tramp:"))
                (:eval (propertize "%b" 'face 'bold))
                (:eval (if (buffer-modified-p) "*" " ")) "   "
                mode-line-position mode-line-read-only-help-echo "  " mode-line-modes))

(add-to-list 'auto-mode-alist '("\\.txt\\'" . 'conf-unix-mode))
(delete-selection-mode 1)

;; #  UI
;; (xterm-mouse-mode 1) ;; only needed for terminal
(pixel-scroll-precision-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(savehist-mode t)

;; #  emacs
(use-package emacs
  :config
  ;; context-menu stuff
  (add-hook 'text-mode-hook 'context-menu-mode)
  (add-hook 'shell-mode-hook 'context-menu-mode)
  (add-hook 'prog-mode-hook 'context-menu-mode)
  (add-hook 'dired-mode-hook 'context-menu-mode)

   
   ;; # 📔 Global functions
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local imenu-generic-expression
                          '((nil ";; # \\(.*\\)" 1)))))

  (defun markdown-to-rich-text ()
    "Convert markdown to rich text, and put it in clipboard"
    (interactive)
    (when (region-active-p)
      (let* ((input-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (command (format "pandoc --embed-resources -s -f markdown -t html -V colorlinks=true -V linkcolor=blue -V urlcolor=blue --highlight-style pygments  2> /dev/null | wl-copy -t text/html")))
        (shell-command-on-region (region-beginning) (region-end) command nil nil "*pandoc-output*")
        (message "Copied rich text to clipboard"))))

  (defun kill-current-buffer ()
    "kill current buffer without prompt"
    (interactive)
    (kill-buffer (current-buffer)))

  (defun current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at "[[:space:]]*$")))

  (defun comment-or-uncomment-region-or-line ()
    (interactive)
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end))
    (when (and (current-line-empty-p) (not (region-active-p)))
      (comment-dwim nil)))

  (defun duplicate-current-line-or-region (arg)
    (interactive "p")
    (let (beg end (origin (point)))
      (if (and mark-active (> (point) (mark)))
          (exchange-point-and-mark))
      (setq beg (line-beginning-position))
      (if mark-active
          (exchange-point-and-mark))
      (setq end (line-end-position))
      (let ((region (buffer-substring-no-properties beg end)))
        (dotimes (i arg)
          (goto-char end)
          (newline)
          (insert region)
          (setq end (point)))
        (goto-char (+ origin (* (length region) arg) arg)))))

  (defun insert-quotations (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\' ?\'))

  (defun insert-ticks (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\` ?\`))

  (defun insert-braces (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\{ ?\}))

  (defun insert-brackets (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\[ ?\]))

  (defun insert-signs (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\< ?\>))

  ;; QMK unicode compability
  (define-key global-map (kbd "C-S-u") 'read-unicode-char)

  (defun read-unicode-char (c1 c2 c3 c4 c5 _trailing_space_ignored)
    "Convert unicode input C1 C2 C3 C4 C5  to the corresponding insert char call."
    (interactive "c\nc\nc\nc\nc\nc")
    (insert-char (string-to-number (format "%c%c%c%c%c" c1 c2 c3 c4 c5) 16)))

  (defun insert-dollars (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\$ ?\$))

  ;; stop emacs from closing all windows after pressing ESC three times
  (defun my-keyboard-escape-quit (fun &rest args)
    (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
      (apply fun args)))

  (defun copy-buffer-file-name ()
  "Copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message "Copied %s" file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))
  
  (advice-add 'keyboard-escape-quit :around #'my-keyboard-escape-quit)

  ;; This is what the dev recommends
  (with-eval-after-load 'transient
    (transient-bind-q-to-quit))

  (setq-default major-mode 'shell-script-mode
                indent-tabs-mode nil
                tab-width 4
                frame-title-format '("   %b")
                initial-major-mode 'markdown-mode)
  ;; treesitter syntax depth
  (setopt treesit-font-lock-level 5)
  ;; # 👱🏼 variables
  (setq tramp-allow-unsafe-temporary-files t
        ;; treesitter
        major-mode-remap-alist '((python-mode . python-ts-mode))
        blink-cursor-mode nil
        disabled-command-function nil ;; emacs disables some commands by default; don't.
        delete-pair-blink-delay 0
        auto-save-default nil
        vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)
        ring-bell-function 'ignore
        auto-revert-verbose nil ;; turn off autosave messages
        pgtk-use-im-context-on-new-connection nil ;; fixes S-SPC
        native-comp-async-report-warnings-errors nil
        native-comp-compiler-options '("-O2" "-march=znver4")
        context-menu-functions '(context-menu-local
                                 context-menu-middle-separator occur-context-menu
                                 context-menu-ffap context-menu-buffers)
        epa-pinentry-mode 'loopback
        large-file-warning-threshold 700000000
        display-time-default-load-average nil
        mouse-sel-mode t
        mouse-autoselect-window t ;; focus-follows-mouse
        use-short-answers t
        make-backup-files nil
        temporary-file-directory "~/.emacs.d/emacs-backups"
        inhibit-startup-screen t
        initial-scratch-message nil
        mouse-drag-copy-region 'non-empty ;; autocopy on mouse selection
        parens-require-spaces nil ;; pairs do not need extra spaces
        ;; store all backup and autosave files in the tmp dir
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  :bind
  ;; # ⌨ global binds
  (("M-\"" . insert-double-quotes)
   ("M-p $" . insert-dollars)
   ("M-p <" . insert-signs)
   ("M-p `" . insert-ticks)
   ("M-p [" . insert-brackets)
   ("M-p {" . insert-braces)
   ("M-p d" . delete-pair)
   ("M-'" . insert-quotations)
   ("M-\"" . insert-pair)
   ("C-x k" . kill-current-buffer)
   ("C-x C-k" . kill-current-buffer)
   ("C-x y" . copy-buffer-file-name)
   ("C-x C-y" . copy-buffer-file-name)
   ("C-x R" . revert-buffer-quick)
   ("C-x \~" . (lambda () (interactive)
                 (let ((default-directory "~")) (ido-find-file))))
   ;; makes current window "dedicated" to its buffer
   ;; make this a toggle
   ("C-x 4 d" . (lambda () (interactive)
                  (message (concat "Dedicated window to buffer " (buffer-name)))
                  (set-window-dedicated-p nil "dedicated")))
   ("C-x C-S-f" . find-file-other-window)
   ("M-?" . nil)
   ("C-z" . nil)
   ("M-;" . comment-or-uncomment-region-or-line)
   ("M-D" . duplicate-current-line-or-region)
   ("M-g" . goto-line)
   ("C-x B" . buffer-menu)
   ("C-+" . text-scale-increase)
   ("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease)
   ("s-=" . text-scale-increase)
   ("s--" . text-scale-decrease)
   ("H-=" . text-scale-increase)
   ("H--" . text-scale-decrease)
   ("C-x C-w" . whitespace-mode)
   ("M-i" . insert-char)
   ("C-x C-Z" . nil) ; Don't  quit by accident with ctrl+z
   ("M-<mouse-3>" . mouse-buffer-menu)
   ("<mouse-2>" . yank) ; yank with middle
   ;; #   window binds
   ("H-b" . balance-windows)
   ("H-r" . transpose-frame)
   ("H-h" . windmove-left)
   ("H-l" . windmove-right)
   ("H-k" . windmove-up)
   ("H-j" . windmove-down)
   ("H-<left>" . windmove-left)
   ("H-<right>" . windmove-right)
   ("H-<up>" . windmove-up)
   ("H-<down>" . windmove-down)
   ("H-y" . ace-swap-window)
   ("C-x <left>" . windmove-left)
   ("C-x <right>" . windmove-right)
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ("C-x 0" . delete-window))
)

(keymap-global-set "M-SPC" "C-u C-SPC")

;; # 📦 use-package

(use-package adwaita-dark-theme
  :init
  (load-theme 'adwaita-dark t)
  :custom
  (adwaita-dark-theme-bold-vertico-current t)
  (adwaita-dark-theme-pad-mode-line t)
  (adwaita-dark-theme-pad-tab-bar t)
  (adwaita-dark-theme-pad-tab-line t)
  :custom-face
  (font-lock-keyword-face ((t (:foreground "#ffa348" :weight normal))))
  ;; these variables influence treesitter highlighting
  ;; (font-lock-variable-name-face ((t (:foreground "#78aeed" :weight normal))))
  ;; (font-lock-variable-name-face ((t (:foreground "#78aeed" :weight normal))))
  (font-lock-function-name-face ((t (:foreground "#7d8ac7" :weight normal))))
  ;; (font-lock-constant-face ((t (:foreground "#596ab8" :weight normal))))
  (font-lockf-comment-face ((t (:foreground "#656565" :slant italic))))
  (show-paren-match ((t (:background "steelblue3"))))
  :config
  (adwaita-dark-theme-arrow-fringe-bmp-enable))

(use-package systemd :mode ("\\.service\\'" . systemd-mode))
(use-package diminish)
(use-package sudo-edit :commands (sudo-edit))

(use-package image-mode
  :ensure nil
  :bind
  (:map image-mode-map
  (("h" . image-backward-hscroll)
   ("j" . image-next-line)
   ("k" . image-previous-line)
   ("l" . image-forward-hscroll)
   ("+" . image-increase-size)
   ("-" . image-descrease-size)
   ("s" . image-transform-reset-to-initial)
   ("a" . image-transform-fit-both)))
)

(use-package avy
  :bind (("C-;" . avy-goto-word-1)))

(use-package multiple-cursors
  :commands (mc/edit-lines mc/mark-all-like-this mc/mark-all-in-region)
  :bind
  (("C-z n" . mc/mark-next-like-this)
   ("C-z p" . mc/mark-previous-like-this)
   ("C-z a" . mc/mark-all-like-this)
   ("C-z e" . mc/edit-lines))
  :config (setq mc/always-run-for-all t))

(use-package windresize :bind (("s-s" . windresize)
                               ("H-s" . windresize)))
(use-package ripgrep :ensure-system-package rg :commands (ripgrep-regexp))
(use-package rainbow-mode :diminish rainbow-mode :commands (rainbow-mode))
(use-package yaml-mode :mode ("\\.yaml\\'" . yaml-mode))

(use-package rcirc
  :ensure nil
  :config
  (load-library "~/.emacs.d/irc-secrets.el.gpg")
  :commands (rirc irc)
)

(use-package proced
  :ensure nil
  :commands (proced)
  :hook
  (proced-mode . (lambda () (proced-toggle-auto-update 1)))
  :config
  (setq proced-enable-color-flag t
        proced-auto-update-flag t
        proced-auto-update-interval 1)
)

;; use-package with build-in package
(use-package ido
  :ensure nil
  :bind (("C-x C-b" . ido-switch-buffer))
  :config
  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always ;; skip asking
        ido-everywhere t)
  (ido-mode))

(use-package compilation
  :ensure nil
  :bind (:map compilation-mode-map
              ("p" . compilation-previous-error)
              ("n" . compilation-next-error)
              ("M-p" . (lambda () (interactive)
                         (previous-error-no-select 1)))
              ("M-n" . (lambda () (interactive)
                         (next-error-no-select 1)))
              )
  )

(use-package ido-vertical-mode
  :after (ido)
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package nerd-icons-completion
  :commands (nerd-icons-insert)
  :config
  (nerd-icons-completion-mode))

(use-package all-the-icons
  :commands (nerd-icons-insert)
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :commands (dired)
  :after (dired)
  :diminish all-the-icons-dired-mode
  :after all-the-icons-nerd-fonts
  :custom-face
  (all-the-icons-dired-dir-face
   ((t (:foreground "#6cb2eb" :weight bold)))))

(use-package ido-completing-read+
  :after (ido)
  :defer ;; fucks with startup time otherwhise
  :config
  (ido-ubiquitous-mode)
  (setq ido-cr+-max-items 60000))

(use-package smex
  :bind
  (("M-x" . smex)))

(use-package imenu-anywhere
  :bind
  (("C-j" . ido-imenu-anywhere)))

(use-package eldoc :diminish eldoc-mode)

(use-package yasnippet
  :ensure t
  :defer 2 ;; fixes LSP; yas needs to be loaded
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  :diminish yas-minor-mode
  :commands (yas-insert-snippet)
  :hook ((prog-mode
          nxml-mode
          html-mode
          sgml-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :bind ("C-'" . yas-insert-snippet)
)

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)
         ("H-x" . magit-status)
         ("H-g" . magit-status)))

(use-package dockerfile-mode
  :mode
  (("\\Dockerfile\\'" . dockerfile-mode)
   ("\\docker-compose.yml\\'" . dockerfile-mode)))

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :hook (csv-mode . csv-align-mode)
  :config
  (setq csv-align-max-width 70))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter "lua")

;; color compile buffers (e.g. those in PKGBUILD mode)
(use-package ansi-color :hook (compilation-filter . ansi-color-compilation-filter))

(use-package pkgbuild-mode
  :hook
  ((pkgbuild-mode . (lambda () (flymake-mode -1))))
  :config
  (setq pkgbuild-makepkg-command "yes | makepkg -sirf")
  :bind
  (:map pkgbuild-mode-map
   ("C-c C-c" . compile)
   )
  :mode
  ("\\PKGBUILD\\'" . pkgbuild-mode))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode)
  :interpreter "fish")

(use-package ace-window :commands (ace-swap-window ace-window))
(use-package transpose-frame :commands (transpose-frame))
(use-package olivetti
  :config (setq olivetti-minimum-body-width 60)
  (setq olivetti-style 'fancy)
  :diminish olivetti-mode
  :bind ("C-x c" . olivetti-mode))

(use-package company
  :diminish company-mode
  :hook
  ((prog-mode . company-mode)
   (markdown-mode . company-mode))
  :config
  (setq company-idle-delay 0.06
        company-selection-wrap-around t
        company-minimum-prefix-length 2))

(use-package company-box
  :defer 1
  :after (company))

;; # 📖 lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-keymap-prefix "C-c l")
  ;; disable some distractions
  (setq lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-ui-doc-show-with-cursor nil
        lsp-diagnostics-provider :none
        lsp-ui-sideline-enable nil
        lsp-signature-render-documentation nil
        lsp-inhibit-message t
        lsp-auto-guess-root t
        lsp-modeline-workspace-status-enable nil
        lsp-pyls-plugins-pycodestyle-enabled t
        lsp-pyls-plugins-pyflakes-enabled nil
        lsp-pylsp-plugins-flake8-enabled nil
        lsp-ruff-lsp-log-level "off")
  ;; believe it or not, but this makes pylsp over tramp work more consistently (at least atm)
  (lsp-toggle-trace-io)
  :bind (:map lsp-mode-map
              ("M-p" . nil)
              ;; to use lsp context menu even with `context-menu-mode` enabled
              ("<down-mouse-3>" . lsp-mouse-click))
  :hook
  ;; override default prefix
  ((lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
   (python-mode . lsp)
   (python-ts-mode . lsp)
   (go-mode . lsp)
   (nxml-mode . lsp)
   (vala-mode . lsp)
   (c-mode . lsp)
   (c-ts-mode . lsp)))

(use-package lsp-ui :after (lsp-mode))

(use-package python-pytest
  :commands (python-pytest-dispatch)
  :config
  ;; remove some commands from transient
  (transient-remove-suffix 'python-pytest-dispatch '(1))
  (transient-remove-suffix 'python-pytest-dispatch '(-2))
  (transient-remove-suffix 'python-pytest-dispatch "--tr")
  (transient-remove-suffix 'python-pytest-dispatch "--rx")
  :bind (:map python-pytest-finished-mode-map
              ("p" . compilation-previous-error)
              ("n" . compilation-next-error)
              ("M-p" . (lambda () (interactive)
                         (previous-error-no-select 1)))
              ("M-n" . (lambda () (interactive)
                         (next-error-no-select 1)))
              ;; show error "in other window"
              ("o" . compilation-display-error)
              )
  )

(use-package python-ts-mode
  :ensure nil
  :interpreter ("python" . python-ts-mode)
  :mode (("\\.py\\'" . python-ts-mode))
  :hook
  ;; bind key to pytest.el in hook, to avoid issue with `:bind`:
  (python-ts-mode . (lambda ()
                      (define-key python-ts-mode-map
                                  (kbd "C-c t") #'python-pytest-dispatch)
                      (define-key python-ts-mode-map (kbd "C-c C-t") #'python-pytest-dispatch)
                      )
                  )
  )



(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
)

(use-package recentf
  :ensure nil
  :defer 1
  :bind
  (("C-x C-r" . ido-recentf-open))
  :config
  (defun no-msg (function)
    "Prevent `function` from echoing"
    (let ((inhibit-message  t))
      (funcall function)))

  (advice-add 'recentf-auto-cleanup :around 'no-msg)

  (recentf-mode t)
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  ;; disable running recentf cleanup on startup
  (setq recentf-auto-cleanup 300)
  (setq recentf-max-saved-items 300))

(use-package nxml
  :ensure nil
  :init
  (add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))
  (setq-default nxml-child-indent 4 nxml-attribute-indent 4
                nxml-outline-child-indent 4)
  :mode
  (("\\.xsd\\'" . nxml-mode)
   ("\\.xml\\'" . nxml-mode))
  :custom-face
  (nxml-attribute-local-name ((t (:inherit nil :foreground "#64a6f4"))))
  (nxml-element-local-name ((t (:inherit font-lock-keyword-face))))
  :hook
  ((nxml-mode . hs-minor-mode)
   (nxml-mode . display-line-numbers-mode)
   (nxml-mode . (lambda ()
                  ;; doesn't work in :map for some reason
                  (local-set-key (kbd "C-c h") 'hs-toggle-hiding)
                  ;; move cursor into tag (i.e. <tag>█...</tag>)
                  (local-set-key (kbd "C-M-i") 'nxml-down-element)))) 
  :config
  (setq nxml-slash-auto-complete-flag t))

;; maybe fix this with treesitter instead?
(use-package mhtml-mode
  :ensure nil
  :config
  (add-hook 'mhtml-mode-hook
            (lambda ()
              ;; needs `face-remap-add-relative` to be mode specific
              (face-remap-add-relative 'font-lock-function-name-face nil :foreground "#ffa348")))
  :mode ("\\.html\\'" . mhtml-mode))


;; # 📁 dired
(use-package dired
  :ensure nil
  :commands (dired)
  :diminish all-the-icons-dired-mode
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . all-the-icons-dired-mode)
   (dired-mode . (lambda () (dired-omit-mode 1))))
  :config
  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file)))
  (setq dired-listing-switches "-ltX --group-directories-first")
  (setq dired-switches-in-mode-line 4)
  (setq dired-recursive-deletes (quote top)) ;; top means ask once
  (setq dired-kill-when-opening-new-dired-buffer nil)
  :custom-face
  (dired-directory
   ((t (:foreground "#6cb2eb" :weight bold))))
  :bind
  (("C-x C-d" . dired-jump)
   ("C-x d" . dired-jump))
  (:map dired-mode-map
        ("j" . dired-next-dirline)
        ("k" . dired-prev-dirline)
        ("l" . dired-find-file)
        ("W" . (lambda () (interactive)
                    (let ((current-prefix-arg 0))
                             (call-interactively 'dired-copy-filename-as-kill))))
        ;; ("y" . (lambda () (interative)
        ;;          (let ((current-prefix-arg 0))
        ;;                     (call-interactively 'dired-copy-filename-as-kill))))
        ("h" . dired-up-directory)
        ("H" . dired-up-directory)
        ("DEL" . dired-up-directory)
        ("/" . isearch-forward)
        ("O" . dired-open-file)
        ("C-c C-t" . dired-toggle-read-only)
        ("<mouse-1>" . dired-find-file)
        ("<mouse-3>" . dired-mouse-find-file-other-window)
        ("<mouse-2>" . dired-find-file)))

(use-package wdired
  :ensure nil
  :after (dired)
  :bind
  (:map wdired-mode-map
        ("C-c C-t" . wdired-finish-edit)))

;; (use-package casual-dired
;;   :defer 2
;;   :after (dired)
;;   :bind (:map dired-mode-map ("C-x d" . 'casual-dired-tmenu)))

(use-package move-text
  :bind
  (("M-<down>" . move-text-down)
   ("M-<up>" . move-text-up)))

(use-package yapfify :commands (yapfify-buffer yapfify-region
                                               yapfify-region-or-buffer))

(use-package checkbox
  :custom
  (checkbox-states '("- [ ]" "- [x]"))
  :bind ("C-c C-c" . checkbox-toggle))


(use-package tex-mode
  :ensure nil
  :config
  (setq-local comment-region-function 'comment-region-default)
  (setq my-latex-command "pdflatex --interaction nonstopmode")
  (defun latex-to-pdf ()
    "Compile buffer to pdf using my-latex-command"
    (interactive)
    (save-buffer)
    (message (concat "Compiling " (buffer-name) "..."))
    ;; (message (concat my-latex-command " " (buffer-file-name) " & ")
    (call-process-shell-command
     (concat my-latex-command " '" buffer-file-name "' & ") nil 0))
  :bind
  (:map latex-mode-map
        ("C-c C-s" . latex-insert-block)
        ("C-c C-c" . latex-to-pdf)
        ("C-c c" . latex-to-pdf))
  :mode
  (("\\.latex\\'" . latex-mode)
   ("\\.tex\\'" . latex-mode)))


;; # ✏ markdown-mode
(use-package markdown-mode
  :custom
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  :init
  (defun inline-markdown-comment (&optional arg)
    (interactive "*P")
    (cond
     ((equal current-prefix-arg nil) ; no C-u
      (insert-pair arg "<!-- " "-->"))
     (t (uncomment-sexp))))
  (defun un-indent-by-removing-2-spaces ()
    "remove 2 spaces from beginning of of line"
    (interactive)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        (when (looking-at "^\\s-+")
          (untabify (match-beginning 0) (match-end 0)))
        (when (looking-at "^  ")
          (replace-match "")))))
  ;; fix expand region in markdown comments
  (defun my-advice--point-is-in-comment-p (orig-fun &rest args)
  "Return t if point is in comment or markdown comment."
  (or (apply orig-fun args)
      (eq (get-text-property (point) 'face) 'markdown-comment-face)))
  (advice-add 'er--point-is-in-comment-p :around #'my-advice--point-is-in-comment-p)
  :config
  (defun context-menu-markdown-toggle-markup (menu click)
    "Populate MENU with command to toggle markdown markup"
    (save-excursion
      (define-key-after menu [highlight-search-mouse]
        '(menu-item "Toggle Markup Hiding" markdown-toggle-markup-hiding
                    :style radio
                    :help "Hide/show markdown markup"
                    :selected markdown-hide-markup)))
    menu)
  ;; replace ∞ by …
  (setq markdown-url-compose-char '(8230 8943 35 9733 9875))
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-add-footnotes-to-imenu nil)
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-disable-tooltip-prompt t)
  (set-face-attribute 'markdown-header-face-1 nil
                      :height 1.55)
  ;; treesitter config overrides this, put it back
  (set-face-foreground 'markdown-header-face-1
                     (face-foreground 'default))
  (set-face-foreground 'markdown-header-face-2
                       (face-foreground 'default))
  (set-face-foreground 'markdown-header-face-3
                     (face-foreground 'default))
  (set-face-attribute 'markdown-header-face-2 nil
                      :height 1.2)
  (set-face-attribute 'markdown-header-face-3 nil
                      :height 1.1)
  :bind
  (:map markdown-mode-map
        ("<backtab>". un-indent-by-removing-2-spaces)
        ("M-p". nil)
        ("C-c C-c". nil)
        ("C-M-n" . markdown-forward-block)
        ("C-M-p" . markdown-backward-block)
        ("C-c C-s I" . markdown-insert-image)
        ("C-c C-s ~" . markdown-insert-strike-through)
        ("C-c RET" . markdown-toggle-markup-hiding)
        ("C-c m" . markdown-toggle-markup-hiding)
        ("M-p c" . inline-markdown-comment)
        ("M-p ;" . inline-markdown-comment))
  :hook
  (markdown-mode . (lambda ()
                         (add-hook 'context-menu-functions
                                   #'context-menu-markdown-toggle-markup
                                   nil t)))
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package evil-numbers
  :config (setq evil-numbers-use-cursor-at-end-of-number 1)
  :bind
  (("M-_" . evil-numbers/dec-at-pt)
   ("M-+" . evil-numbers/inc-at-pt)))

(use-package daemons
  :commands (daemons)
)

;; #   vterm
(use-package vterm
  :hook
  (vterm-mode . (lambda ()
                  (setq-local global-hl-line-mode nil)
                  (text-scale-decrease 1)
                  ;; (with-editor-export-editor) ;; nice but really slow
                  (setq-local show-paren-mode nil)))
  :config
  (defun vterm-new-window (arg)
    "Open a new vterm in a split window"
    (interactive "P")
    (split-window-sensibly)
    (other-window 1)
    (vterm arg))
  (diminish 'vterm-copy-mode
          '(:propertize " VTermCopy" face '(:weight bold)))
  (setq vterm-buffer-name-string "vterm %s")
  (setq vterm-shell "/bin/fish")
  (setq vterm-tramp-shells '(("scp" "/bin/fish") ("ssh" "/bin/fish") ("docker" "/bin/sh")))
  (setq vterm-eval-cmds
    '(("find-file" find-file)
      ("sudo-edit" sudo-edit)
      ("sudo-edit-file" sudo-edit-file)
      ("message" message)
      ("vterm-clear-scrollback" vterm-clear-scrollback)
      ("find-file-other-window" find-file-other-window)))
  (setq vterm-max-scrollback 16000)
  (setq vterm-min-window-width 70)
  ;; the :map mechanism doesn't work?
  (define-key vterm-copy-mode-map (kbd "q")
              #'(lambda () (interactive) (vterm-copy-mode -1)))
  :bind (("C-M-S-<return>" . vterm-new-window)
         ("C-M-S-v" . vterm)
         ("H-o" . vterm-other-window)
         ("H-v" . vterm)
         ("H-<return>" . vterm-other-window)
         :map vterm-mode-map
         ("<escape>" . vterm-copy-mode)
         ("C-S-K" . previous-line)
         ("C-S-J" . next-line)
         ("C-r" . (lambda () (interactive)
                    (vterm-copy-mode)
                    (isearch-backward)))
         ("C-;" . (lambda () (interactive)
                    (vterm-copy-mode)
                    (avy-goto-char-1)))
         ("C-_" . nil)
         ("C-u" . vterm--self-insert)
         ("M->" . end-of-buffer)
         :map vterm-copy-mode-map
         ("q" . vterm-copy-mode-done)
         ("<escape>" . vterm-copy-mode-done))
  )

;; # 📔 pdf-tools
(use-package pdf-tools
  :init
  (defun pdf-highlight-blue ()
    (interactive)
    (pdf-annot-add-highlight-markup-annotation
     (pdf-view-active-region) "#7EB7E7"))
  :mode
  (("\\.pdf" . pdf-view-mode))
  :magic ("%PDF" . pdf-view-mode)
  :config
  (defun delete-current-page ()
    (interactive)
    (setq page-number (pdf-view-current-page))
    (let ((start (- page-number 1))
          (end (+ page-number 1))
          (pdf (buffer-file-name)))
      (cond
       ((eq page-number 1)
        (setq qpdf-command (format
                            "qpdf \"%s\" --replace-input --pages /tmp/redacted.pdf 1 \"%s\" 2-z --"
                            pdf pdf)))
       ;; case for handling deletion of the last page
       ((eq page-number (pdf-cache-number-of-pages))
        (setq qpdf-command (format
                            "qpdf \"%s\" --replace-input --pages \"%s\" 1-%d /tmp/redacted.pdf --"
                            pdf pdf start)))
       ;; base case
       (t
        (setq qpdf-command (format
                            "qpdf \"%s\" --replace-input --pages \"%s\" 1-%d /tmp/redacted.pdf 1 \"%s\" %d-z --"
                            pdf pdf start pdf end)))))
    (call-process-shell-command qpdf-command nil 0)
    )

  (defun process-csv-field ()
    "Perform an action based on the csv field at point."
    (interactive)
    ;; switch to csv buffer
    (other-window -1)
    (let ((word (thing-at-point 'sexp t)))
      (cond
       ;; word ends with ".pdf"
       ((string-match-p "\\.pdf\\'" word)
        ;; kill current pdf to save memory 
        (other-window -1)
        (kill-current-buffer)
        ;; jump back to csv buffer
        (other-window -1)
        (forward-sexp)
        (setq new-page (string-to-number (thing-at-point 'sexp t)))
        (insert "*")
        (forward-sexp)
        ;; switch to pdf window with new document
        (find-file-other-window word)
        (pdf-view-goto-page new-page))
       ;; word is an integer
       ((string-match-p "^[0-9]+" word)
        (insert "*")
        (forward-sexp)
        (other-window -1)
        (pdf-view-goto-page (string-to-number word)))))
    )

  (defun restore-current-page ()
    ;; restore page from redacted folder
    )
  (pdf-tools-install :no-query)
  (setq pdf-cache-prefetch-delay 0.1
        pdf-cache-image-limit 128
        pdf-view-midnight-colors '("white smoke" . "black"))
  (setq pdf-annot-edit-contents-setup-function
        (lambda (_annot) (markdown-mode)))
  (setq-default pdf-view-display-size 'fit-page)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  (with-eval-after-load "pdf-history"
    (define-key pdf-history-minor-mode-map (kbd "l") nil))
  ;; wait until map is available
  (with-eval-after-load "pdf-annot"
    (define-key pdf-annot-edit-contents-minor-mode-map
                (kbd "<return>") 'pdf-annot-edit-contents-commit)
    (define-key pdf-annot-edit-contents-minor-mode-map
                (kbd "<S-return>") 'newline))
  :hook
  (pdf-view-mode . (lambda ()
                     (pdf-history-minor-mode -1)
                     (pdf-misc-size-indication-minor-mode)))
  :bind
  (:map pdf-view-mode-map
        ("a" . pdf-view-fit-page-to-window)
        ("s" . pdf-view-fit-width-to-window)
        ("j" . pdf-view-next-line-or-next-page)
        ("u" . pdf-view-next-page)
        ("k" . pdf-view-previous-line-or-previous-page)
        ("i" . pdf-view-previous-page)
        ("l" . image-forward-hscroll)
        ("h" . image-backward-hscroll)
        ("C-=" . pdf-view-enlarge)
        ("g" . pdf-view-first-page)
        ("G" . pdf-view-last-page)
        ("M-g" . pdf-view-goto-page)
        ("H" . image-bob)
        ("L" . image-eob)
        ("K" . pdf-view-previous-page-command)
        ("J" . pdf-view-next-page-command)
        ("S-SPC" . pdf-view-scroll-down-or-previous-page)
        ("TAB" . ido-imenu-anywhere)
        ("/" . isearch-forward)
        ;; ("n" . nil)
        ("d" . delete-current-page)
        ("n" . process-csv-field)
        ("p" . nil)
        ("!" . pdf-view-midnight-minor-mode)
        ("m" . pdf-annot-add-highlight-markup-annotation)
        ("M" . pdf-highlight-blue)
        ("t" . pdf-annot-add-text-annotation)
        ("D" . pdf-annot-delete)
        ("C-SPC" . (lambda () (interactive)
                     (message "Position saved") (pdf-view-position-to-register ?x)))
        ("M-SPC" . (lambda () (interactive)
                     (pdf-view-jump-to-register ?x)))))

(use-package lorem-ipsum :commands
  (lorem-ipsum-insert-list lorem-ipsum-insert-sentences
                           lorem-ipsum-insert-paragraphs))

(use-package sqlite-mode-extras
  :commands (sqlite-mode-open-file)
  :defer 1
  :load-path "lisp/"
  :demand
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("+" . sqlite-mode-extras-add-row)
         ("D" . sqlite-mode-extras-delete-row-dwim)
         ("C" . sqlite-mode-extras-compose-and-execute)
         ("E" . sqlite-mode-extras-execute)
         ("S" . sqlite-mode-extras-execute-and-display-select-query)
         ("DEL" . sqlite-mode-extras-delete-row-dwim)
         ("g" . sqlite-mode-extras-refresh)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

(use-package dired-rainbow
  :commands (dired)
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

(use-package god-mode
  :bind (("<escape>" . god-local-mode)
         (:map god-local-mode-map
               ("z" . repeat)
               ("[" . markdown-backward-paragraph)
               ("]" . markdown-forward-paragraph)
               ("i" . god-local-mode))))

(use-package emojify
  :config
  (defun prefix-emojify-insert-emoji (arg)
    "insert emoji using ido"
    (interactive "P")
    (if arg
        (insert
         (apply 'concat (make-list arg (emojify-completing-read "Insert Emoji: "))))
      (insert (emojify-completing-read "Insert Emoji: "))))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji") nil 'prepend)
  :bind
  (("C-x C-e" . prefix-emojify-insert-emoji)))

(use-package expand-region :bind (("M-=" . er/expand-region) ("M--" . er/contract-region)))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; #   tab-bar
(use-package tab-bar
  :ensure nil
  :defer t
  :config
  (defun tab-bar-tab-name-format-dot (tab i)
    "Add a dot between the tab number and tab name"
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (format "● %d " i)
               (alist-get 'name tab))
       'face (funcall tab-bar-tab-face-function tab))))
  (setq tab-bar-tab-name-format-function #'tab-bar-tab-name-format-dot)
  (setq tab-bar-show 1)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-new-tab-to 'rightmost)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
  (setq tab-bar-auto-width t)
  (setq tab-bar-auto-width-max '(380 35))
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-format
        '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
  ;; add clock to bar
  ;; this slows down emacs for some reason
  ;; (setq display-time-format "  %a %b %R")
  (setq display-time-format "%a %d %b %R")
  (setq display-time-interval 15)
  (display-time-mode)
  :bind
  (
   ("H-1" . (lambda () (interactive) (tab-bar-select-tab 1)))
   ("H-2" . (lambda () (interactive) (tab-bar-select-tab 2)))
   ("H-3" . (lambda () (interactive) (tab-bar-select-tab 3)))
   ("H-4" . (lambda () (interactive) (tab-bar-select-tab 4)))
   ("H-t" . tab-bar-new-tab)
   ("<tab-bar> <wheel-up>" . nil)
   ("<tab-bar> <wheel-down>" . nil)
   ("H-w" . tab-bar-close-tab)
   ("H-<" . tab-previous)
   ("H->" . tab-next)
   ("H-K" . tab-next)
   ("H-J" . tab-previous)
   ("<mouse-9>" . tab-next)
   ("<mouse-8>" . tab-previous)))

;; mode for merging .pacnew files
(use-package pacfiles-mode :commands (pacfiles))

(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)
  (setq Man-width-max 140)
  :bind
  ("C-h M" . man)
  (:map Man-mode-map
        ("h" . backward-char)
        ("l" . forward-char)
        ("k" . previous-line)
        ("j" . next-line)))


;; # TODO
;; Stuff that is good to save, but needs to be looked at

;; compile markdown
;; (setq my-markdown-command "markdowntopdf")
;; (defun markdown-to-pdf ()
;;   (interactive)
;;   (save-buffer)
;;   (setq fn (markdown-export-file-name ""))
;;   (message (concat "Compiling " (buffer-name) "..."))
;;   (call-process-shell-command
;;    (concat my-markdown-command " " fn " & ") nil 0))

;; # 🔨 customized settings
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ABDO" :slant normal :weight regular :height 108 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ee0785c299c1d228ed30cf278aab82cf1fa05a2dc122e425044e758203f097d2"
     "6ebdb33507c7db94b28d7787f802f38ac8d2b8cd08506797b3af6cdfd80632e0"
     default))
 '(package-selected-packages
   '(ace-window adwaita-dark-theme all-the-icons-completion
                all-the-icons-dired all-the-icons-nerd-fonts checkbox
                company-box csv-mode daemons diminish dired-rainbow
                dockerfile-mode emojify evil evil-numbers
                expand-region fish-mode fringe-helper go-mode god-mode
                goto-chg idlwave ido-completing-read+
                ido-vertical-mode imenu-anywhere lorem-ipsum lsp-ui
                lua-mode magit meson-mode move-text multiple-cursors
                nerd-icons-completion olivetti pacfiles-mode pdf-tools
                php-mode pkgbuild-mode po-mode pug-mode python-pytest
                rainbow-mode ripgrep smex sudo-edit systemd
                transpose-frame vala-mode vterm windresize yaml-mode
                yapfify yasnippet-snippets ztree))
 '(warning-suppress-log-types '((unlock-file))))

