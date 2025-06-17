;-*- lexical-binding: t; -*-
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

(pixel-scroll-precision-mode)
(tool-bar-mode -1)

(use-package emacs
  :config

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

  (defun copy-buffer-file-name ()
  "Copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message "Copied %s" file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

  (cua-mode 1)
  (setq cua-enable-cursor-indications t) ;; visual feedback
  (setq cua-auto-tabify-rectangles nil)  ;; don't mess with whitespace
  (setq cua-keep-region-after-copy t)    ;; standard behavior

  (setq make-backup-files nil) ;; don't clutter with backup~ files
  (setq auto-save-default nil)
  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (context-menu-mode 1)

  ;; # 👱🏼 variables
  (setq tramp-allow-unsafe-temporary-files t
        disabled-command-function nil ;; emacs disables some commands by default; don't.
        vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)
        ring-bell-function 'ignore
        auto-revert-verbose nil ;; turn off autosave messages
        display-time-default-load-average nil
        mouse-sel-mode t
        mouse-autoselect-window t ;; focus-follows-mouse
        use-short-answers t
        ;; store all backup and autosave files in the tmp dir
        ;; this produced overly long filenames, apperently
        ;; auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        )
  :bind
  ;; # ⌨ global binds
  (("C-x k" . kill-current-buffer)
   ("C-x C-k" . kill-current-buffer)
   ("C-x y" . copy-buffer-file-name)
   ("C-x C-y" . copy-buffer-file-name)
   ("C-r" . undo-redo)
   ("M-;" . comment-or-uncomment-region-or-line)
   ("M-D" . duplicate-current-line-or-region)
   ("M-g" . goto-line)
   ("C-x B" . buffer-menu)
   ("C-+" . text-scale-increase)
   ("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease)
   ("s-=" . text-scale-increase)
   ("s--" . text-scale-decrease)
   ("C-x C-w" . whitespace-mode)
   ("M-i" . insert-char)
   ;; #   window binds
   ("C-x <left>" . windmove-left)
   ("C-x <right>" . windmove-right)
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ("<mouse-9>" . next-buffer)
   ("<mouse-8>" . previous-buffer)
   ("C-x 0" . delete-window))
)


;; # 📦 use-package
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
  :config (setq mc/always-run-for-all t))

;; use-package with build-in package
(use-package ido
  :ensure nil
  :bind (("C-x C-b" . ido-switch-buffer))
  :config
  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always ;; skip asking
        ido-everywhere t)
  (ido-mode)
  (defun ido-open-vterm ()
  "Open `vterm' in the directory under point in the Ido completion session."
  (interactive)
  (if (memq ido-cur-item '(file dir))
      (let ((dir (if (file-directory-p ido-current-directory)
                     ido-current-directory
                   (file-name-directory ido-current-directory))))
        (setq ido-exit 'vterm)
        (exit-minibuffer)
        (let ((default-directory dir))
          (vterm)))
    (message "Not a valid directory to open vterm.")))

  ;; doesn't work but oh well
  (define-key ido-common-completion-map (kbd "M-v") #'ido-open-vterm)
)

(use-package ido-vertical-mode
  :after (ido)
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-completing-read+
  :after (ido)
  :defer ;; fucks with startup time otherwhise
  :config
  (ido-ubiquitous-mode)
  (setq ido-cr+-max-items 60000))

(use-package smex
  :bind
  (("M-x" . smex)))

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :hook (csv-mode . csv-align-mode)
  :config
  (setq csv-align-max-width 70
        csv-align-padding 2
        csv-field-quotes '("\"" "~"))
  (csv-header-line))

(use-package transpose-frame :commands (transpose-frame))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
                          ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

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

(use-package markdown-mode
  :custom
  (markdown-header-scaling t)
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package vterm
  :config
  (defun vterm-new-window (arg)
    "Open a new vterm in a split window"
    (interactive "P")
    (split-window-sensibly)
    (other-window 1)
    (vterm arg))
  (defun my-vterm-copy-mode-done ()
    "The built-in vterm-copy-mode-done acts weird sometimes; so this is my own implementation"
    (interactive)
                  (when (use-region-p)
                    (kill-ring-save (region-beginning)
                                    (region-end)))
                  (vterm-copy-mode -1))
  (diminish 'vterm-copy-mode
          '(:propertize " VTermCopy" face '(:weight bold)))
  (setq vterm-buffer-name-string "vterm %s")
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
  :commands (vterm vterm-other-window vterm-new-window)
  :bind (("C-M-S-<return>" . vterm-new-window)
         ("C-M-S-v" . vterm)
         ("H-o" . vterm-other-window)
         ("H-v" . vterm)
         ("H-<return>" . vterm-other-window)
         :map vterm-mode-map
         ("<escape>" . vterm-copy-mode)
         ("C-c M-o" . vterm-clear-scrollback)
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
         ("q" . my-vterm-copy-mode-done)
         ("<escape>" . my-vterm-copy-mode-done))
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
    ;; TODO: still to be implemented
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-operandi))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
