;; use more memory (LSP recommendations)
(setq gc-cons-threshold 100000000)
read-process-output-max (* 1024 1024) ;; 1mb

(eval-when-compile
  (require 'use-package))
;; always install missing dependencies
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; (package-initialize)

;; TODO: move to use-package
;; this fucks with elisp behavior, i.e. removes all other imenu items
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local imenu-generic-expression
                        '((nil ";; # \\(.*\\)" 1)))))

;; # 👱🏼 variables
(setq tramp-allow-unsafe-temporary-files t
      ;; confirm-kill-processes nil
      vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)
      ring-bell-function 'ignore
      pgtk-use-im-context-on-new-connection nil ;; fixes S-SPC
      native-comp-async-report-warnings-errors nil
      native-comp-compiler-options '("-O2" "-march=znver4")
      large-file-warning-threshold 700000000
      display-time-default-load-average nil
      mouse-sel-mode t
      mouse-autoselect-window t ;; focus-follows-mouse
      use-short-answers t
      make-backup-files nil
      temporary-file-directory "~/.emacs.d/emacs-backups"
      real-auto-save-interval 120 ;; in seconds
      inhibit-startup-screen t
      initial-scratch-message nil
      mouse-drag-copy-region 'non-empty ;; autocopy on mouse selection
      parens-require-spaces nil ;; pairs do not need extra spaces
      ;; store all backup and autosave files in the tmp dir
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
)

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

(delete-selection-mode 1)

(setq-default major-mode 'shell-script-mode)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . 'conf-unix-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-indent-mode 0)

;; #  UI
;; set initial window size. NOTE: fucks with GNOME's tiling
;; (setq initial-frame-alist
;;        '((top . 20) (left . 20) (width . 69) (height . 32)))
(setq blink-cursor-mode nil)
;; (xterm-mouse-mode 1) ;; only needed for terminal
(pixel-scroll-precision-mode)
(setq-default frame-title-format '("%b - Emacs"))
;; doesn't work, annoyingly
;; (setq-default frame-title-format '((:eval (string-replace "" "  " "%b")) "- Emacs "))
;; (setq-default frame-title-format '("   Emacs"))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(savehist-mode t)

;; # 📔 Functions

(use-package emacs
  :config
  (defun copy-thing (begin-of-thing end-of-thing &optional arg)
    "copy thing between beg & end into kill ring"
    (save-excursion
      (let ((beg (get-point begin-of-thing 1))
            (end (get-point end-of-thing arg)))
        (shell-command
         (concat "echo -n '" (buffer-substring beg end) "' | xsel -b")))))

  (defun get-point (symbol &optional arg)
    "get the point"
    (funcall symbol arg)
    (point))

  (defun copy-word (&optional arg)
    "Copy words at point into kill-ring"
    (interactive "P")
    (copy-thing 'forward-word 'backward-word arg)
    (message "Copied word"))

  (defun copy-line (&optional arg)
    "Save current line into Kill-Ring without mark the line"
    (interactive "P")
    (copy-thing 'beginning-of-line 'end-of-line arg)
    (message "Copied line"))

  (defun markdown-to-rich-text ()
    "Convert markdown to rich text, and put it in clipboard"
    (interactive)
    (when (region-active-p)
      (let* ((input-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (command (format "pandoc -s -f markdown -t html -V colorlinks=true -V linkcolor=blue -V urlcolor=blue 2> /dev/null | wl-copy -t text/html")))
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

  (defun insert-braces (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\{ ?\}))

  (defun insert-brackets (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\[ ?\]))

  (defun insert-signs (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\< ?\>))

  (defun insert-dollars (&optional arg)
    (interactive "*P")
    (insert-pair arg ?\$ ?\$))
  :bind
  ;; # ⌨ global binds
  (("M-\"" . insert-double-quotes)
   ("M-p $" . insert-dollars)
   ("M-p <" . insert-signs)
   ("M-p [" . insert-brackets)
   ("M-p {" . insert-braces)
   ("M-'" . insert-quotations)
   ("M-\"" . insert-pair)
   ("C-x k" . kill-current-buffer)
   ("C-x C-k" . kill-current-buffer)
   ("C-c w" . copy-word)
   ("C-c l" . copy-line)
   ("M-;" . comment-or-uncomment-region-or-line)
   ("M-D" . duplicate-current-line-or-region))
)

(global-set-key (kbd "M-SPC") (kbd "C-u C-SPC"))
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x B") 'buffer-menu)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-w") 'whitespace-mode)
(global-set-key (kbd "M-i") 'insert-char)
(global-set-key "\C-x\C-z" nil) ; Don't quit by accident with ctrl+z
(global-unset-key [M-mouse-3])
(global-set-key [M-mouse-3] 'mouse-buffer-menu)
;; #   window binds
(global-set-key (kbd "s-y") 'ace-swap-window)
(global-set-key (kbd "s-b") 'balance-windows)
(global-set-key (kbd "s-r") 'transpose-frame)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x 0") 'delete-window)

(keymap-global-set "<mouse-2>" 'yank) ; what does this do?


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
  (font-lockf-comment-face ((t (:foreground "#656565" :slant italic))))
  (show-paren-match ((t (:background "steelblue3"))))
  :config
  (adwaita-dark-theme-arrow-fringe-bmp-enable))

(use-package systemd :mode ("\\.service\\'" . systemd-mode))
(use-package diminish)
(use-package sudo-edit :commands (sudo-edit))

(use-package avy
  :bind (("C-;" . avy-goto-word-1)))

(use-package multiple-cursors
  :commands (mc/edit-lines mc/mark-all-like-this mc/mark-all-in-region)
  :config (setq mc/always-run-for-all t))

(use-package windresize :bind ("s-s" . windresize))
(use-package ripgrep :ensure-system-package rg :commands (ripgrep-regexp))
(use-package rainbow-mode :diminish rainbow-mode :commands (rainbow-mode))
(use-package yaml-mode :mode ("\\.yaml\\'" . yaml-mode))

(use-package proced
  :ensure nil
  :config 
  (setq proced-enable-color-flag t
        proced-auto-update-flag t
        proced-auto-update-interval 1)
)


;; use-package with build-in package
(use-package ido
  :demand t
  :ensure nil
  :bind ("C-x C-b" . ido-switch-buffer)
  :config
  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always ;; skip asking
        ido-everywhere t)
  (ido-mode)
)

;; (define-key ido-completion-map (kbd "C-r") 'ido-recentf-open)

(use-package ido-vertical-mode
  :after (ido)
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package all-the-icons-nerd-fonts)
(use-package nerd-icons-completion
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
  :config
  (ido-ubiquitous-mode)
  (setq ido-cr+-max-items 50000))

(use-package smex
  :bind
  (("M-x" . smex)))

(use-package imenu-anywhere
  :bind
  (("C-j" . ido-imenu-anywhere)))

(use-package eldoc :diminish eldoc-mode)
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
)

(use-package yasnippet-snippets :after (yasnippet))

(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-c C-g" . magit-status)))

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

(use-package pkgbuild-mode
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
  (setq company-idle-delay 0.1 
        company-selection-wrap-around t
        company-minimum-prefix-length 2))

(use-package company-box :after (company))

;; # 📖 lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  ;; disable some distractions
  (setq lsp-keymap-prefix "C-c l" ;; Or 'C-l', 's-l'
        lsp-lens-enable nil
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
  :hook
  ((python-mode . lsp) (c-mode . lsp)))

(use-package recentf
  :ensure nil
  :bind
  (("C-x r" . ido-recentf-open)
   ("C-x C-r" . ido-recentf-open))
  :init
  (recentf-mode t)
  :config
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  (setq recentf-max-saved-items 270))

(use-package hideshow :ensure nil)

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
  :mode
  (("\\.xsd\\'" . nxml-mode)
   ("\\.xml\\'" . nxml-mode))
  :custom-face
  (nxml-attribute-local-name ((t (:inherit nil :foreground "#64a6f4"))))
  (nxml-element-local-name ((t (:inherit font-lock-keyword-face))))
  :hook
  ((nxml-mode . hs-minor-mode)
   (nxml-mode . display-line-numbers-mode)
   (nxml-mode . (lambda ()  (local-set-key (kbd "C-c h") 'hs-toggle-hiding)))) ;; doesn't work in :map for some reason
  :config
  (setq nxml-slash-auto-complete-flag t))

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
  (("C-x C-d" . dired))
  (:map dired-mode-map
        ("j" . dired-next-dirline)
        ("k" . dired-prev-dirline)
        ("l" . dired-find-file)
        ("h" . dired-up-directory)
        ("H" . dired-up-directory)
        ("DEL" . dired-up-directory)
        ("/" . isearch-forward)
        ("O" . dired-open-file)
        ("C-c C-t" . dired-toggle-read-only)
        ("<mouse-1>" . dired-find-file)
        ("<mouse-2>" . dired-find-file)))

(use-package wdired
  :ensure nil
  :after (dired)
  :bind
  (:map wdired-mode-map
        ("C-c C-t" . wdired-finish-edit)))

(use-package lsp-ui :after (lsp-mode))

(use-package move-text
  :bind
  (("M-<down>" . move-text-down)
   ("M-<up>" . move-text-up)))

(use-package yapfify :commands (yapfify-buffer yapfify-region
                                               yapfify-region-or-buffer))

(use-package checkbox
  :custom (checkbox-states '("- [ ]" "- [x]"))
  :bind ("C-c C-c" . checkbox-toggle))

;; # 🖊 markdown-mode
(use-package markdown-mode
  :init
  (defun inline-markdown-comment (&optional arg)
    (interactive "*P")
    (cond
     ((equal current-prefix-arg nil) ; no C-u
      (insert-pair arg "<!-- " "-->"))
     (t (uncomment-sexp))))
  (setq-default initial-major-mode 'markdown-mode)
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
  :config
  (setq markdown-header-scaling 1)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-add-footnotes-to-imenu nil)
  :bind
  (:map markdown-mode-map
        ("<backtab>". un-indent-by-removing-2-spaces)
        ("M-p". nil)
        ("C-c C-c". nil)
        ("M-p c" . inline-markdown-comment)
        ("M-p ;" . inline-markdown-comment))
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
  :unless (memq window-system '(ns w32))  ;; windows+vterm is wonky
  :init
  (defun vterm-new-window (arg)
    "Open a new vterm in a split window."
    (interactive "P")
    (split-window-sensibly)
    (other-window 1)
    (vterm arg))
  (setq-local kill-buffer-query-functions nil) ;; always kill vterm buffers
  :hook
  (vterm-mode . (lambda ()
                  (setq-local global-hl-line-mode nil)
                  (yas-minor-mode -1)
                  (text-scale-decrease 1)
                  (setq-local show-paren-mode nil)))
  ;; (vterm-copy-mode . god-mode)
  :config
  (diminish 'vterm-copy-mode
          '(:propertize " VTermCopy" face '(:weight bold)))
  (setq vterm-buffer-name-string "vterm %s")
  (setq vterm-shell "/bin/fish")
  (setq vterm-eval-cmds
    '(("find-file" find-file)
      ("sudo-edit" sudo-edit)
      ("sudo-edit-file" sudo-edit-file)
      ("message" message)
      ("vterm-clear-scrollback" vterm-clear-scrollback)
      ("find-file-other-window" find-file-other-window)))
  (setq vterm-max-scrollback 15000)
  (setq vterm-min-window-width 70)
  ;; the :map mechanism doesn't work?
  (define-key vterm-copy-mode-map (kbd "q") 
              #'(lambda () (interactive) (vterm-copy-mode -1)))
  :bind (("C-M-S-<return>" . vterm-new-window)
         ("C-M-S-v" . vterm)
         ("C-M-S-o" . vterm-other-window)
         ("s-o" . vterm-other-window)
         ("s-v" . vterm)
         ("s-<return>" . vterm-other-window)
         :map vterm-mode-map
         ("C-S-K" . previous-line)
         ("C-S-J" . next-line)
         ("C-r" . (lambda () (interactive)
                    (vterm-copy-mode)
                    (isearch-backward)))
         ("C-;" . (lambda () (interactive)
                    (avy-goto-word-0 nil)
                    (vterm-copy-mode)))
         ("C-_" . nil)
         ("C-u" . vterm--self-insert)
         ("M->" . end-of-buffer)
         :map vterm-copy-mode-map
         ("q" . (lambda () (interactive)
                  (when (use-region-p)
                    (kill-ring-save (region-beginning) 
                                    (region-end)))
                  (vterm-copy-mode -1)))
         )
)


(use-package pdf-tools
  :init
  (defun pdf-highlight-blue ()
    (interactive)
    (pdf-annot-add-highlight-markup-annotation
     (pdf-view-active-region) "#7EB7E7"))
  :mode
  (("\\.pdf$" . pdf-view-mode))
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
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
        ("n" . nil)
        ("p" . nil)
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
  :commands sqlite-mode-open-file
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

(use-package cmake-ts-mode
  :ensure nil
  :mode ("\\.cmake\\'" . cmake-ts-mode))

(use-package python-ts-mode
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode))

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

(use-package expand-region :bind (("M-=" . er/expand-region)))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; #   tab-bar
(use-package tab-bar
  :ensure nil
  :init
  (defun tab-bar-tab-name-format-dot (tab i)
    "Add a dot between the tab number and tab name"
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format " %d " i) "")
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab))))
  :config
  (setq tab-bar-tab-name-format-function #'tab-bar-tab-name-format-dot)
  ;; (setq tab-bar-mode 1)
  (setq tab-bar-show 1)
  (setq tab-bar-tab-hints t)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-new-tab-to 'rightmost)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
  (setq tab-bar-auto-width t)
  (setq tab-bar-auto-width-max '(380 35))
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-format
        '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
  ;; add clock to bar
  ;; (setq display-time-format "  %R")
  ;; (setq display-time-interval 1)
  ;; (display-time-mode)
  :custom-face
  (tab-bar-tab ((t nil)))
  :bind
  (
   ("s-1" . (lambda () (interactive) (tab-bar-select-tab 1)))
   ("s-2" . (lambda () (interactive) (tab-bar-select-tab 2)))
   ("s-3" . (lambda () (interactive) (tab-bar-select-tab 3)))
   ("s-4" . (lambda () (interactive) (tab-bar-select-tab 4)))
   ("s-t" . tab-bar-new-tab)
   ("<tab-bar> <wheel-up>" . nil)
   ("<tab-bar> <wheel-down>" . nil)
   ("s-w" . tab-bar-close-tab)
   ("s-." . tab-next)
   ("s-," . tab-previous)
   ("s->" . tab-next)
   ("s-<" . tab-previous)
   ("s-K" . tab-next)
   ("s-J" . tab-previous)
   ("C-M-!" . (lambda () (interactive) (tab-bar-select-tab 1)))
   ("C-M-@" . (lambda () (interactive) (tab-bar-select-tab 2)))
   ("C-M-#" . (lambda () (interactive) (tab-bar-select-tab 3)))
   ("C-M-$" . (lambda () (interactive) (tab-bar-select-tab 4)))
   ("C-M-S-t" . tab-bar-new-tab)
   ("C-M-S-w" . tab-bar-close-tab)
   ("C-M->" . tab-next)
   ("C-M-<" . tab-previous)
   ("<mouse-9>" . tab-next)
   ("<mouse-8>" . tab-previous)))

(use-package man
  :ensure nil
  :config
  (setq Man-notify-method 'pushy)
  (setq Man-width-max 140)
  :bind
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
   '("6ebdb33507c7db94b28d7787f802f38ac8d2b8cd08506797b3af6cdfd80632e0" default))
 '(package-selected-packages
   '(systemd magit yasnippet yasnippet-snippets pdf-loader tramp-theme pkgbuild-mode yaml-mode daemons daemons.el all-the-icons-dired nerd-icons-completion all-the-icons-completion all-the-icons-nerd-fonts sudo-edit diminish zenburn-theme yapfify windresize vterm transpose-frame smooth-scrolling smex ripgrep rainbow-mode pdf-tools olivetti multiple-cursors move-text lua-mode lsp-ui lorem-ipsum imenu-anywhere idomenu ido-vertical-mode ido-completing-read+ god-mode fish-mode expand-region evil-numbers emojify dockerfile-mode dired-rainbow csv-mode company-box checkbox all-the-icons adwaita-dark-theme ace-window)))
