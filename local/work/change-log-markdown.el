;;; change-log-markdown.el --- Markdownでメモ

(defvar change-log-markdown-log-file (expand-file-name "~/change-log.md")
  "メモを残すファイル。")

(defvar change-log-markdown-header-title "ChangeLog"
  "ファイルの表題")

(defvar change-log-markdown-date-section-title-format "%Y-%m-%d (%a)"
  "ログの日付表題のフォーマット")

(defvar change-log-markdown-time-section-title-format "%H:%M:%S"
  "ログの時刻表題のフォーマット")

(defun change-log-markdown-open ()
  (find-file change-log-markdown-log-file))

(defun change-log-markdown-get-date-section-title (time)
  (format-time-string change-log-markdown-date-section-title-format time))

(defun change-log-markdown-get-time-section-title (time)
  (format-time-string change-log-markdown-time-section-title-format time))

(defun change-log-markdown-search-section (title hbar-char)
  (goto-char (point-max))
  (let ((case-fold-search nil))
    (re-search-backward (concat "^"
                                (regexp-quote title) "\n"
                                (regexp-quote (make-string 1 hbar-char)) "\\{3,\\}"
                                "$")
                        nil t)))

(defun change-log-markdown-insert-new-section (title hbar-char)
  (goto-char (point-max))
  (insert (concat "\n"
                  title "\n"
                  (make-string (max (length title) 3) hbar-char) "\n")))

(defun change-log-markdown-insert-new-log-entry (title header-level)
  (goto-char (point-max))
  (insert (concat "\n"
                  (make-string header-level ?#) " " title "\n")))

(defun change-log-markdown-add-entry ()
  "open change-log.md and new entry"
  (interactive)
  (change-log-markdown-open)
  (let ((now-time (current-time)))
    (let ((date-section-tile (change-log-markdown-get-date-section-title now-time))
          (time-section-tile (change-log-markdown-get-time-section-title now-time)))
      (unless (change-log-markdown-search-section change-log-markdown-header-title ?=)
        (change-log-markdown-insert-new-section change-log-markdown-header-title ?=))
      (unless (change-log-markdown-search-section date-section-tile ?-)
        (change-log-markdown-insert-new-section date-section-tile ?-))
      (change-log-markdown-insert-new-log-entry time-section-tile 3))))

(provide 'change-log-markdown)

; Local Variables:
; mode: Emacs-Lisp
; indent-tabs-mode: nil
; End:
