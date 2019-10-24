;;; wl.el --- initialize wanderlust  -*- coding: utf-8 -*-

;;; Commentary:

;;; Code:

;; SSL
(setq ssl-program-name "openssl")
(setq ssl-program-arguments
  '("s_client"
    "-quiet"
    "-host" host
    "-port" service
    "-verify" (int-to-string ssl-certificate-verification-policy)
    "-CApath" ssl-certificate-directory
    ))

;; Win32 HOME network settings
(cond
 ((and (eq window-system 'w32)
       (or (string-match "^[Cc][Ee][Rr][Nn][Oo][Bb][Oo][Gg]" (system-name))
	   (string-match "^[Vv][Aa][Rr][Cc][Oo][Ll][Aa][Cc]" (system-name))))
  (setq elmo-msgdb-directory "//cernobog/toki/.elmo")
  (setq elmo-localdir-folder-path "//cernobog/toki/Mail")))

;; Folders
(setq wl-strict-diff-folders '("^\\+inbox$" "@mail\\.freedom\\.ne\\.jp"))
(setq wl-auto-check-folder-list '("^\\+inbox$"))
(setq wl-auto-uncheck-folder-list '("."))
(setq wl-default-folder "+inbox")
(setq wl-default-spec "%")
(setq wl-stay-folder-window t)
(setq wl-folder-check-async t)

;; Servers
(setq elmo-pop3-default-server "mail.freedom.ne.jp")
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-port 993)
(setq elmo-imap4-default-stream-type 'ssl)
(setq elmo-imap4-default-user "yr10ki@gmail.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-use-modified-utf7 t)
(setq wl-smtp-posting-server "mail.freedom.ne.jp")
(setq wl-smtp-posting-port 587)
(setq wl-smtp-posting-user "toki")
(setq wl-smtp-authenticate-type "plain")

;; Local Domain
(setq wl-local-domain "wl-message-id-domain")

;; Offline mode
(setq wl-plugged nil)

;; Message
(setq elmo-msgdb-extra-fields '("X-ML-Name" "Newsgroups"))
(setq elmo-archive-treat-file t)
(setq wl-summary-important-above 0)
(setq wl-summary-target-above 1000)
(setq wl-message-id-domain "mail.freedom.ne.jp")
(setq wl-summary-auto-refile-skip-marks ())
(with-eval-after-load "mime-view"
  (define-key mime-view-mode-default-map (kbd "C-c c") 'xcite))

;; Expire
(setq wl-expire-use-log t)
(setq wl-summary-expire-reserve-marks
      '(; "$"
	"N" "U" "!"))

;; Draft
(setq wl-interactive-send t)
(setq wl-user-mail-address-list
      '("toki@freedom.ne.jp"
	"toki@phys.sci.kobe-u.ac.jp"
	"toki@hep.phys.sci.kobe-u.ac.jp"
	"toki@sv01.phys.sci.kobe-u.ac.jp"
	"toki@icepp.s.u-tokyo.ac.jp"
	"toki@imopc7.icepp.s.u-tokyo.ac.jp"))
(setq wl-from "土岐 仁謙 (TOKI Yoshinori) <toki@freedom.ne.jp>")
(setq wl-fcc "+send")
(setq wl-draft-reply-without-argument-list
      '(("Followup-To" . (nil nil ("Followup-To")))
	("Mail-Followup-To" . (("Mail-Followup-To") nil ("Newsgroups")))
	(("X-ML-Name" "Reply-To") . (("Reply-To") nil nil))
	("From" . (("From") ("To" "Cc") ("Newsgroups")))))
(setq wl-draft-always-delete-myself t)
(setq wl-template-alist
      '(("sig:japanese"
	 ("From" . wl-from)
	 (bottom .
"-
土岐 仁謙 (TOKI Yoshinori) <toki@freedom.ne.jp>
"))
	("sig:english"
	 ("From" . "Yoshinori Toki <toki@freedom.ne.jp>")
	 (bottom .
"-
Yoshinori Toki <toki@freedom.ne.jp>
"))
	))
(add-hook
 'wl-mail-setup-hook
 (function
  (lambda ()
    (set-buffer-file-coding-system 'utf-8))))
(add-hook
 'wl-draft-mode-hook
 (function
  (lambda ()
    (define-key wl-draft-mode-map (kbd "C-c c") 'xcite)
    (define-key wl-draft-mode-map (kbd "C-c C-y") 'xcite-yank-cur-msg))))

;; Encode the Japanese characters by UTF-8 on sending a message
(setq charsets-mime-charset-alist
      (mapcar
       (lambda (i)
         (if (string-prefix-p "iso-2022-jp" (symbol-name (cdr i)))
             (cons (car i) 'utf-8)
           i))
       charsets-mime-charset-alist))

;; Color
(set-face-foreground 'wl-highlight-message-cited-text-2 "DeepPink")
(set-face-foreground 'wl-highlight-summary-new-face "Magenta")

;;; wl.el ends here
