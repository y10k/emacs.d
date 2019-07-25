;;; init.el --- initialize emacs  -*- coding: utf-8 -*-

;;; Commentary:

;;; Code:

;; HOME directory
(cd (expand-file-name "~"))

;; Personal information
(setq user-mail-address "toki@freedom.ne.jp")
(setq user-full-name "TOKI Yoshinori")

;; packages for init.el
(require 'subr-x)

;; path for MS-Windows
(if (eq system-type 'windows-nt)
   (progn
     (delete "C:/WINDOWS/System32/OpenSSH/" exec-path)
     (add-to-list 'exec-path "C:/Program Files/Git/usr/bin" t)
     (setenv "PATH"
             (let ((path-list (split-string (getenv "PATH") ";")))
               (delete "C:\\WINDOWS\\System32\\OpenSSH\\" path-list)
               (add-to-list 'path-list "C:\\Program Files\\Git\\usr\\bin")
               (string-join path-list ";")))))

;; Info directories
(setq Info-default-directory-list
      (mapcar
       (lambda (path) (expand-file-name path))
       '("/usr/share/info" "/usr/local/info" "/usr/X11R6/info")))

;; emacs auto settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-try-passive-mode t)
 '(package-selected-packages
   (quote
    (quickrun wanderlust helm-ls-git highlight-indent-guides unfill volatile-highlights undo-tree powerline ddskk magit helm-swoop helm helm-git-grep auto-complete flycheck git-gutter inf-ruby yari yaml-mode markdown-mode)))
 '(safe-local-variable-values (quote ((frozen_string_literal . true))))
 )

;; emacs -q -lした時に、user-emacs-directoryが変わるように
(when load-file-name
    (setq user-emacs-directory (file-name-directory load-file-name)))

;; Local emacs-lisp library path
(add-to-list 'load-path (locate-user-emacs-file "local/work"))
(add-to-list 'load-path (locate-user-emacs-file "local/patch"))
(add-to-list 'load-path (locate-user-emacs-file "local/lib"))
(add-to-list 'load-path (locate-user-emacs-file "local/ruby-test-unit"))

;; Emacs Lisp Package Archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Japanese environment
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; color
(if window-system
    (load-theme 'deeper-blue t)
  (custom-set-faces
   '(font-lock-builtin-face ((t (:foreground "brightblue"))))
   '(font-lock-keyword-face ((t (:foreground "cyan"))))
   '(helm-selection ((t (:background "ForestGreen" :foreground "brightyellow"))))
   '(match ((t (:background "RoyalBlue3" :foreground "brightyellow"))))
   '(region ((t (:background "blue3" :foreground "brightwhite"))))))

;; frame size & alpha
(if window-system
    (let* ((geometry-pixel-list (alist-get 'geometry (car (display-monitor-attributes-list))))
           (geometry-pixel-width (nth 2 geometry-pixel-list))
           (geometry-pixel-height (nth 3 geometry-pixel-list))
           (geometry-pixel-size (list geometry-pixel-width geometry-pixel-height)))
      (cond
       ((and (eq window-system 'x) (>= emacs-major-version 23))
        (modify-all-frames-parameters '((alpha . (75 50 50 50))))
        (cond
         ((equal geometry-pixel-size '(1800 1200))
          (modify-all-frames-parameters
           '((width . 170)
             (height . 51)
             (font . "MS Gothic-12")))
          (push '(left . 30) initial-frame-alist)
          (push '(top . 50) initial-frame-alist))))
       ((eq window-system 'w32)
        (modify-all-frames-parameters '((alpha . (90 60 60 60))))
        (cond
         ((equal geometry-pixel-size '(1800 1200))
          (modify-all-frames-parameters
           '((width . 170)
             (height . 53)
             (font . "ＭＳ ゴシック-12")))))))))

;; frame title
(setq frame-title-format
      '(multiple-frames ("%b - " invocation-name "@" system-name)
                        ("" invocation-name "@" system-name)))

;; Window
(setq split-width-threshold 300)

;; mini buffer
(if (eq emacs-major-version 21)
    (setq resize-mini-windows nil))

;; Mouse Wheel mode
(if (>= emacs-major-version 21)
    (mouse-wheel-mode 1))

;; disable Tool Bar
;; Xresource => Emacs.toolBar: 0
(if (>= emacs-major-version 21)
    (tool-bar-mode 0))

;; no menu in CUI
(if (not window-system)
    (menu-bar-mode 0))

;; Mode line information
(require 'powerline)
(powerline-default-theme)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; No new lines
(setq next-line-add-newlines nil)

;; Scroll 1 by 1
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;; Parenthesis
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Region highlighting
(transient-mark-mode 1)

;; User key bindings
(load "term/bobcat")
(when (fboundp 'terminal-init-bobcat)
  (terminal-init-bobcat))
(if (boundp 'minibuffer-local-filename-completion-map)
    (progn
      (define-key minibuffer-local-filename-completion-map " "
        'minibuffer-complete-word)
      (define-key minibuffer-local-must-match-filename-map " "
        'minibuffer-complete-word)))
(global-set-key (kbd "C-\\") 'help-command)
(global-set-key (kbd "C-\\ C-\\") 'help-for-help)
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Replacing
(defun select-query-replace (enable-regexp)
  (interactive "P")
  (let ((args (query-replace-read-args
               (if enable-regexp "Query replace regexp" "Query replace")
               (if enable-regexp t nil))))
    (if enable-regexp
        (query-replace-regexp (nth 0 args) (nth 1 args))
      (query-replace (nth 0 args) (nth 1 args)))))
(global-set-key (kbd "M-%") 'select-query-replace)

;; Buffer switching
(icomplete-mode)
(define-key icomplete-minibuffer-map (kbd "C-s") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") 'icomplete-backward-completions)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(defadvice switch-to-buffer (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer: " (other-buffer) t))))
(defadvice switch-to-buffer-other-window (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer in other window: " (other-buffer) t))))
(defadvice switch-to-buffer-other-frame (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer in other frame: " (other-buffer) t))))
(defun make-buffer (buffer-name)
  (interactive (list (read-buffer "New buffer: " nil nil)))
  (switch-to-buffer (get-buffer-create buffer-name)))
(defun make-buffer-other-window (buffer-name)
  (interactive (list (read-buffer "New buffer: " nil nil)))
  (switch-to-buffer-other-window (get-buffer-create buffer-name)))
(defun make-buffer-other-frame (buffer-name)
  (interactive (list (read-buffer "New buffer: " nil nil)))
  (switch-to-buffer-other-frame (get-buffer-create buffer-name)))
(defun duplicate-buffer (base-buffer-name)
  (let ((base-buffer (get-buffer base-buffer-name)))
    (let ((default-major-mode (cdr (assq 'major-mode
                                         (buffer-local-variables base-buffer))))
          (copy-buffer (make-indirect-buffer base-buffer
                                             (generate-new-buffer-name
                                              (concat "*" base-buffer-name " (copy)*")))))
      (set-buffer-major-mode copy-buffer)
      copy-buffer)))
(defun copy-buffer (base-buffer-name)
  (interactive (list (buffer-name)))
  (switch-to-buffer (duplicate-buffer base-buffer-name)))
(defun copy-buffer-other-window (base-buffer-name)
  (interactive (list (buffer-name)))
  (switch-to-buffer-other-window (duplicate-buffer base-buffer-name)))
(defun copy-buffer-other-frame (base-buffer-name)
  (interactive (list (buffer-name)))
  (switch-to-buffer-other-frame (duplicate-buffer base-buffer-name)))

;; Window switching
(defun other-window-one-step (previous)
  (interactive "P")
  (if previous
      (other-window -1)
    (other-window 1)))
(global-set-key (kbd "M-o") 'other-window-one-step)
(global-set-key (kbd "C-^") 'other-window-one-step)
(setq truncate-partial-width-windows nil)

;; Timestamp
(defun insert-timestamp ()
  (interactive)
  (insert (current-time-string)))
(defun insert-current-date()
  (interactive)
  (insert (format-time-string "%Y-%m-%d (%a)" (current-time))))
(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%H:%M:%S" (current-time))))
(global-set-key (kbd "C-c ;") 'insert-current-date)
(global-set-key (kbd "C-c :") 'insert-current-time)

;; Shell mode
(setq comint-scroll-show-maximum-output t)
(setq comint-scroll-to-bottom-on-output t)

;; C & C++ mode customization
(add-hook
 'c-mode-common-hook
 (function
  (lambda ()
    (setq c-basic-offset 2)
    (c-set-offset 'substatement 0))))

;; Auto compression
(auto-compression-mode t)

;; Perl mode indent customization
(setq perl-indent-level                2)
(setq perl-continued-statement-offset  2)
(setq perl-continued-brace-offset     -2)
(setq perl-brace-offset                0)
(setq perl-brace-imaginary-offset      0)
(setq perl-label-offset                0)

;; LaTeX mode
(setq tex-default-mode 'latex-mode)
(autoload 'latex-label-insert "latex-label"
  "insertion of a latex label." t)
(add-hook
 'latex-mode-hook
 (function
  (lambda ()
    (define-key tex-mode-map "\C-cl" 'latex-label-insert))))

;; Exciting cite utility
(autoload 'xcite "xcite" "exciting cite" t)
(autoload 'xcite-yank-cur-msg "xcite" "exciting cite" t)
(global-set-key (kbd "C-c c") 'xcite)
(setq xcite:insert-header-function
      (function xcite-toki-header))
(defun xcite-toki-header ()
  (concat
   (if date
       (format "{Date} %s\n" date))
   (if subject
       (format "{Subject} %s\n" subject))
   (if msgid
       (format "{Message ID} %s\n" msgid))
   (if id
       (format "%s wrote...\n"
               (cond
                ((or (string-match "^toki@freedom\\.ne\\.jp$" id)
                     (string-match "^toki@.*phys\\(\\.sci\\)?\\.kobe-u\\.ac\\.jp$" id))
                 (format "自分 <%s>" id))
                (handle
                 (format "%s <%s>" handle id))
                (t id))))))

;; Ruby mode
(require 'ruby-test-unit)
(add-hook 'ruby-mode-hook
          (lambda () (ruby-test-unit-keys)))
(setq ruby-test-unit-runner-options "--no-use-color") ; for test-unit on ruby-2.2.0 or later.
(setq ruby-program-name
      (concat "ruby " (expand-file-name "/usr/local/bin/irb") " --inf-ruby-mode"))
(setq yari-ri-program-name "bundle exec ri")
(setq yari-ruby-program-name "bundle exec ruby")

;; Major mode for RDoc editing
(autoload 'rdoc-mode "rdoc-mode" "Major mode for RD editing." t)
(setq auto-mode-alist
      (append '(("\\.rd$" . rdoc-mode)
                ("\\.rd\\.[A-Za-z]*$" . rdoc-mode))
              auto-mode-alist))

;; Comparing files
(setq diff-switches "-u")

;; Verilog-HDL mode
(setq use-verilog-mode t)
(autoload 'verilog-mode "verilog-mode" "verilog mode" t )
(setq auto-mode-alist
      (append '(("\\.v\\'" . verilog-mode)
                ("\\.tv\\'" . verilog-mode)
                ("\\.dv\\'" . verilog-mode)
                ("\\.vlg\\'" . verilog-mode)
                ("\\.vei\\'" . verilog-mode)) auto-mode-alist))
(setq verilog-indent-level             2)
(setq verilog-indent-level-module      2)
(setq verilog-indent-level-declaration 2)
(setq verilog-indent-level-behavorial  2)
(setq verilog-cexp-indent              1)
(setq verilog-case-indent              2)
(setq verilog-minimum-comment-distance 40)
(setq verilog-auto-newline nil)
(setq verilog-auto-indent-on-newline nil)
(setq verilog-tab-always-indent t)
(setq verilog-indent-begin-after-if nil)
(setq verilog-auto-endcomments nil)
(setq verilog-auto-lineup `(all))

;; Shell script mode
(setq sh-indentation 2)
(setq sh-basic-offset 2)

;; Fetchmail
(autoload 'fetchmail "fetchmail" nil t)
(setq fetchmail-default-server "freedom")
(setq fetchmail-server-option-alist
      '(("sv01.phys.sci.kobe-u.ac.jp" "--check")))
(setq fetchmail-server-alias-alist
      '(("homesrv"  . "babayaga.plutonia.ne.jp")
        ("freedom"  . "mail.freedom.ne.jp")
        ("kobephys" . "sv01.phys.sci.kobe-u.ac.jp")))

;; for Bookmark
(setq bookmark-search-size 32)

;; Grep
(setq grep-command "egrep -ne ")

;; FLIM
(setq mime-field-decoding-max-size (* 64 1024))
(setq mime-header-lexical-analyzer      ; http://lists.airs.net/wl/archive/199909/msg00009.html
      '(;eword-analyze-quoted-string
        eword-analyze-domain-literal
        eword-analyze-comment
        eword-analyze-spaces
        eword-analyze-special
        eword-analyze-encoded-word
        eword-analyze-atom))
(eval-after-load "mime"                 ; http://lists.airs.net/wl/archive/199909/msg00031.html
  '(defadvice mime-entity-filename (around mime-decode activate)
     ad-do-it
     (and ad-return-value 
          (setq ad-return-value (eword-decode-string ad-return-value)))))

;; SKK
(autoload 'skk-mode "skk" nil t)
(autoload 'skk-auto-fill-mode "skk" nil t)
(autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
(autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)
(global-set-key (kbd "C-x C-j") (function skk-mode))
(global-set-key (kbd "C-x j") (function skk-auto-fill-mode))
(add-hook 'isearch-mode-hook (function skk-isearch-mode-setup))
(add-hook 'isearch-mode-end-hook (function skk-isearch-mode-cleanup))
(setq skk-large-jisyo
      (cond
       ((eq system-type 'windows-nt)
        (expand-file-name "~/skk/SKK-JISYO.L"))
       (t "/usr/share/skk/SKK-JISYO.L")))
(setq skk-rom-kana-rule-list
      '(("hh" "h"
         ("ッ" . "っ"))
        ("mm" "m"
         ("ン" . "ん"))
        ; 記号の追加
        ("!" nil "！")))

;; SDIC-mode
(if (>= emacs-major-version 26)
    (defvaralias 'default-fill-column 'fill-column))
(autoload 'sdic-describe-word "sdic"
  "" t nil)
(autoload 'sdic-describe-word-at-point "sdic"
  "" t nil)
(global-set-key (kbd "C-x w") 'sdic-describe-word)
(global-set-key (kbd "C-x W") 'sdic-describe-word-at-point)
(setq sdic-eiwa-dictionary-list
      (mapcar
       (lambda (sdic-dictionary)
         (setcar (cdr sdic-dictionary)
                 (expand-file-name (cadr sdic-dictionary)))
         sdic-dictionary)
       '((sdicf-client "~/dict/gene.sdic.gz"
                       (title "GENE")
                       (strategy direct))
         (sdicf-client "~/dict/eedict.sdic.gz"
                       (title "EEDICT")
                       (strategy direct)))))
(setq sdic-waei-dictionary-list
      (mapcar
       (lambda (sdic-dictionary)
         (setcar (cdr sdic-dictionary)
                 (expand-file-name (cadr sdic-dictionary)))
         sdic-dictionary)
       '((sdicf-client "~/dict/jedict.sdic.gz"
                       (title "JEDICT")
                       (strategy direct))
         (sdicf-client "~/dict/jgene.sdic.gz"
                       (title "JGENE")
                       (strategy direct)))))

;; Use unzip on zip mode
(setq archive-zip-use-pkzip nil)

;; EWB mode
(autoload 'ewb-mode "ewb-mode" "" t)
(setq auto-mode-alist
      (append '(("\\.ewb$" . ewb-mode))
              auto-mode-alist))

;; patch for ediff
(eval-after-load "ediff-init"
  '(defadvice ediff-window-display-p (after disable-window-display activate)
     (setq ad-return-value nil)))
(if (eq window-system 'w32)
    (setq ediff-force-faces t))

;; for dired
(add-hook 'dired-load-hook
          (lambda ()
            (define-key dired-mode-map "W" 'browse-url-of-dired-file)))

;; for python
(setq python-indent 2)

;; for VBScript
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" . 
                                 visual-basic-mode)) auto-mode-alist))
(setq visual-basic-mode-indent 2)

;; memo
(require 'change-log-markdown)
(setq change-log-markdown-log-file "/mnt/c/Users/toki/OneDrive/ドキュメント/change-log.md")
(setq change-log-markdown-header-title "作業メモ")
(global-set-key (kbd "C-c 4") nil)
(global-set-key (kbd "C-c 4 a") 'change-log-markdown-add-entry)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)))
(add-hook 'text-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)))

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'ruby-mode-hook
          (lambda () (flycheck-mode 1))) ; force flycheck enable on tramp

;; git-gutter
(global-git-gutter-mode t)

;; auto-complete
(require 'auto-complete-config)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(ac-config-default)
(setq ac-use-menu-map t)
(setq ac-ignore-case nil)

;; Helm
(require 'helm-config)
(defun select-find-file (arg)
  "Select `find-file' function.
If ARG is true execute `helm-find-files', else do `find-file'."
  (interactive "P")
  (if arg
      (funcall (function find-file)
               (read-file-name "Find file: " nil nil nil))
    (helm-find-files arg)))
(global-set-key (kbd "C-x C-f") 'select-find-file)         ; replace helm command
(global-set-key (kbd "M-x") 'helm-M-x)                     ; replace helm command
(global-set-key (kbd "ESC M-x") 'execute-extended-command) ; backup original command
(global-set-key (kbd "ESC M-y") 'helm-show-kill-ring)

;; imenu
(setq imenu-max-item-length 256)

;; whitespace
(setq-default indent-tabs-mode nil)       ; インデントはタブではなくスペースを使用
(setq-default show-trailing-whitespace t) ; 行末の空白をハイライト

(defun my/disable-trailing-whitespace-mode-hook ()
  "Disable show tail whitespace."
    (setq show-trailing-whitespace nil))

(setq my/disable-trailing-whitespace-mode-list
  '(comint-mode
    eshell-mode
    eww-mode
    completion-setup
    vc-git-log-view-mode
    helm-major-mode
    compilation-mode))

(dolist (mode my/disable-trailing-whitespace-mode-list)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            'my/disable-trailing-whitespace-mode-hook))

;; magit
(setq magit-git-executable "git")
(defadvice magit-status (after magit-fullscreen activate)
  (delete-other-windows))

;; tramp
(if (eq system-type 'windows-nt)
    (setq tramp-ssh-controlmaster-options
          "-tt -o ControlMaster=auto -o ControlPath=/tmp/tramp.%%C -o ControlPersist=no"))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; gnus for gmail
(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-user "yr10ki@gmail.com")
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)

;; highlight-indent-guides
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)

;; vc-mode
(setq vc-git-print-log-follow t)

;; quickrun
(setq quickrun-timeout-seconds 30)

;; recentf
(setq recentf-max-saved-items 1000)
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)) ; start timer and save the timer object for `cancel-timer'
(recentf-mode 1)

;; emacs auto settings
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;; init.el ends here
