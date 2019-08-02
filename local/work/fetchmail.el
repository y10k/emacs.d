;;; Fetchmail.el --- Emacs ���� fetchmail ��ư���롣
;;; $Id$
;;
;; <<< ��� >>>
;; �ڴ� �θ� <toki@freedom.ne.jp>
;;
;; <<< URL >>>
;; http://www.freedom.ne.jp/toki/elisp/fetchmail.el
;;
;; <<< ��� >>>
;; [2000-09-10]
;; �������ޥ������ѿ��������ѹ�����ޤ�����
;; �����ѿ����ѻߤˤʤä��Τ���դ��Ƥ���������
;;   fetchmail-server-param-alist
;;   fetchmail-param-func-alist
;;
;; <<< ����ˡ >>>
;; �ޤ�fetchmail.el��load-path���̤ä��ǥ��쥯�ȥ���֤���.emacs��
;;   (autoload 'fetchmail "fetchmail" nil t)
;; �Ȥ��������ɤ��ɲä��ޤ���
;; �����ѿ�fetchmail-server-omit-passwd-list��fetchmail��.fetchmailrc��
;; �ѥ���ɤ����ꤷ�Ƥ��륵����̾�����ꤷ�ޤ���
;; �ѿ�fetchmail-server-omit-passwd-list�˥�����̾���ɲä����
;; fetchmail��ư����Ȥ��˥ѥ���ɤ�Ҥͤʤ��ʤ�ޤ���
;; �դ��ѿ�fetchmail-server-omit-passwd-list�Υ�����̾��������
;; .fetchmammilrc����ѥ���ɤΥ���ȥ��������ȡ�
;; fetchmail��ư����Ȥ��ǽ�ΰ������ѥ���ɤ�Ҥͤ�
;; ����ʹߤ�Emacs��λ����ޤǥѥ���ɤ򵭲����ޤ���
;; �ѿ�fetchmail-server-omit-passwd-list��.fetchmailrc�����꤬
;; ̷�⤷�Ƥ���Ȥ��ޤ�ư��ʤ��Τ���դ��Ƥ���������
;; ��Ϲ��ߤ˱�����
;;   fetchmail-default-server
;;   fetchmail-server-option-alist
;;   fetchmail-server-alias-alist
;;   fetchmail-preprocess-hook
;;   fetchmail-postprocess-hook
;;   fetchmail-notify-beep
;;   fetchmail-window
;;   fetchmail-window-time-format
;;   fetchmail-window-height-ratio
;;   fetchmail-window-height-lower-limit
;;   fetchmail-window-height-upper-limit
;; �������ѿ����ͤ�Ŭ�������ꤷ�Ƥ���������
;; ���ݤʤ�ǥե�����ͤΤޤޤǤ⹽���ޤ���
;; ���M-x fetchmail��¹Ԥ����fetchmail����ư���ޤ���
;; �����Ф��ѿ�fetchmail-server-omit-passwd-list����Ͽ����Ƥ��ʤ����
;; �ǽ�˰��٤����ѥ���ɤ��䤤��碌�Ƶ�����������ܤμ¹Ԥ����
;; ���������ѥ���ɤ���Ѥ��ޤ���
;; ���ΤȤ��ѥ���ɤ�fetchmail-server-passwd-alist�ѿ��˵������졢
;; Emacs Lisp�˴��줿�ͤʤ��ñ�˼��Ф��Ƥ��ޤ��Τ�
;; ü��������Υ���Ȥ�����դ��Ƥ���������
;;

(defvar fetchmail-default-server nil
  "�ǥե���ȤΥ����С�")

(defvar fetchmail-server-omit-passwd-list ()
  "�ѥ���ɤ����Ϥ��ά���륵����̾�Υꥹ�ȡ�
��: '(\"hepsun2.phys.sci.kobe-u.ac.jp\"
      \"phys03.phys.sci.kobe-u.ac.jp\")")

(defvar fetchmail-server-option-alist ()
  "��������Υ��ץ��������ꤹ��Ϣ�ۥꥹ�ȡ�
��: '((default \"--protocol\" \"--pop3\" \"--fetchall\") ; �ǥե����
      (\"hepsun2.phys.sci.kobe-u.ac.jp\"
       \"--protocol\" \"apop\" \"--user\" \"foo\" \"--fetchall\")
      (\"phys03.phys.sci.kobe-u.ac.jp\"
       \"--protocol\" \"apop\" \"--user\" \"bar\" \"--keep\" \"--no-flush\" \"--uidl\"))")

(defvar fetchmail-server-alias-alist ()
  "�����Ф���̾�����ꤹ��Ϣ�ۥꥹ�ȡ�
��: '((\"KOBEHEP\" . \"hepsun2.phys.sci.kobe-u.ac.jp\")
      (\"KOBEPHYS\" . \"phys03.phys.sci.kobe-u.ac.jp\"))")

(defvar fetchmail-server-passwd-alist ()
  "�����ФΥѥ���ɤ���¸����Ϣ�ۥꥹ�ȡ�")

(defvar fetchmail-preprocess-hook ()
  "Fetchmail������������Ͽ����եå���")

(defvar fetchmail-postprocess-hook ()
  "Fetchmail�θ��������Ͽ����եå���")

(defvar fetchmail-notify-beep t
  "�����ѿ������ΤȤ�fetchmail����λ�������Ȥ�beep�����Τ餻�롣")

(defvar fetchmail-window t
  "�����ѿ������ΤȤ�fetchmail�Хåե��򥦥���ɥ��ǳ�����")

(defvar fetchmail-window-time-format " [%a %b %e %T %Y]"
  "Fetchmail��ư���������ɽ������񼰡�
�����ѿ���nil�����ꤹ��Ȼ����ɽ�����ʤ���")

(defvar fetchmail-window-height-ratio 0.15
  "Fetchmail������ɥ��ι⤵����Ψ��")

(defvar fetchmail-window-height-lower-limit 5
  "Fetchmail������ɥ��ι⤵�β��¡�")

(defvar fetchmail-window-height-upper-limit 10
  "Fetchmail������ɥ��ι⤵�ξ�¡�")

(defvar fetchmail-last-server nil
  "�Ǹ�˻Ȥ�줿�����Ф�̾�������äƤ��롣
fetchmail-start�ؿ�����ưŪ�����ꤹ��Τǡ��桼�������ꤷ�ƤϤ����ʤ���")

(defvar fetchmail-process-name "fetchmail"
  "Fetchmail�ץ�����̾����")

(defvar fetchmail-buffer-name "*fetchmail*"
  "Fetchmail�Хåե���̾����")

(defvar fetchmail-running nil
  "Fetchmail��ư����Ǥ��뤳�Ȥ�ɽ�魯�ޥ��ʡ��⡼���ѿ�")
(unless (assq 'fetchmail-running minor-mode-alist)
  (setq minor-mode-alist
	(cons '(fetchmail-running " Fetching mail...")
	      minor-mode-alist)))

(defvar fetchmail-mode-map nil
  "Fetchmail�᥸�㡼�⡼�ɤΥ����ޥåס�")
(unless fetchmail-mode-map
  (setq fetchmail-mode-map (make-sparse-keymap))
  (define-key fetchmail-mode-map "\C-cx" 'fetchmail)
  (define-key fetchmail-mode-map "\C-cq" 'fetchmail-close-window))

(defvar fetchmail-mode-hook nil
  "Fetchmail�᥸�㡼�⡼�ɤΥեå���")

(defun fetchmail-get-server-name (fetchmail-server-name-or-alias)
  "Fetchmail�Υ����Ф���̾���褹�롣"
  (or (cdr (assoc fetchmail-server-name-or-alias fetchmail-server-alias-alist))
      fetchmail-server-name-or-alias))

(defun fetchmail-get-option-list (fetchmail-server)
  "fetchmail�Υ��ץ����Υꥹ�Ȥ��֤���"
  (or (cdr (assoc (fetchmail-get-server-name fetchmail-server)
		  fetchmail-server-option-alist))
      (cdr (assq 'default fetchmail-server-option-alist))))

(defun fetchmail-make-server-alist ()
  "Fetchmail�����Ф�Ϣ�ۥꥹ�Ȥ��롣"
  (let ((fetchmail-server-list ()))
    (if fetchmail-default-server
	(setq fetchmail-server-list
	      (cons fetchmail-default-server fetchmail-server-list)))
    (if fetchmail-server-omit-passwd-list
	(setq fetchmail-server-list
	      (append fetchmail-server-list
		      fetchmail-server-omit-passwd-list)))
    (let (fetchmail-server-alist
	  (fetchmail-server-alist-list (list fetchmail-server-option-alist
					     fetchmail-server-alias-alist)))
      (while fetchmail-server-alist-list
	(setq fetchmail-server-alist
	      (car fetchmail-server-alist-list))
	(while fetchmail-server-alist
	  (if (stringp (caar fetchmail-server-alist))
	      (let ((fetchmail-server-couple-list (list (caar fetchmail-server-alist)
							(fetchmail-get-server-name
							 (caar fetchmail-server-alist)))))
		(while fetchmail-server-couple-list
		  (if (not (member (car fetchmail-server-couple-list)
				   fetchmail-server-list))
		      (setq fetchmail-server-list
			    (cons (car fetchmail-server-couple-list)
				  fetchmail-server-list)))
		  (setq fetchmail-server-couple-list
			(cdr fetchmail-server-couple-list)))))
	  (setq fetchmail-server-alist
		(cdr fetchmail-server-alist)))
	(setq fetchmail-server-alist-list
	      (cdr fetchmail-server-alist-list))))
    (let ((count 0))
      (mapcar
       (lambda (fetchmail-server)
	 (cons fetchmail-server
	       (setq count (1+ count))))
       fetchmail-server-list))))

(defun fetchmail-query-server ()
  "Fetchmail�Υ����Ф�ߥ˥Хåե������򤹤롣"
  (let ((fetchmail-server
	 (completing-read "Fetchmail server: "
			  (fetchmail-make-server-alist)
			  nil nil nil nil fetchmail-default-server)))
    (if (and fetchmail-server
	     (> (length fetchmail-server) 0))
	fetchmail-server
      nil)))

(defun fetchmail-set-passwd (fetchmail-server fetchmail-passwd)
  "fetchmail-server-passwd-alist�˥ѥ���ɤ����ꤹ�롣"
  (let ((fetchmail-server-passwd-pair
	 (assoc (fetchmail-get-server-name fetchmail-server)
		fetchmail-server-passwd-alist)))
    (let ((fetchmail-old-passwd (cdr fetchmail-server-passwd-pair)))
      (if fetchmail-server-passwd-pair
	  (setcdr fetchmail-server-passwd-pair
		  fetchmail-passwd)
	(setq fetchmail-server-passwd-alist
	      (cons (cons (fetchmail-get-server-name fetchmail-server)
			  fetchmail-passwd)
		    fetchmail-server-passwd-alist)))
      fetchmail-old-passwd)))

(defun fetchmail-get-passwd (fetchmail-server)
  "fetchmail-server-passwd-alist����ѥ���ɤ���Ф���"
  (cdr (assoc (fetchmail-get-server-name fetchmail-server)
	      fetchmail-server-passwd-alist)))

(defun fetchmail-clear-passwd (fetchmail-server)
  "fetchmail-server-passwd-alist����ѥ���ɤ������롣"
  (fetchmail-set-passwd (fetchmail-get-server-name fetchmail-server) nil))

(defun fetchmail-query-passwd-p (fetchmail-server)
  (not (member (fetchmail-get-server-name fetchmail-server)
	       fetchmail-server-omit-passwd-list)))

(defun fetchmail-query-passwd (fetchmail-server)
  "�����ФΥѥ���ɤ����ꤹ�롣"
  (unless (fetchmail-get-passwd fetchmail-server)
    (fetchmail-set-passwd (fetchmail-get-server-name fetchmail-server)
			  (base64-encode-string
			   (read-passwd (format "Password for %s: "
						fetchmail-server)) nil))))

(defun fetchmail-buffer-p ()
  "Fetchmail�Хåե��������Ƥ��뤫�ɤ������ǧ���롣"
  (if (get-buffer fetchmail-buffer-name)
      t
    nil))

(defun fetchmail-make-buffer ()
  "Fetchmail�Хåե����롣"
  (let ((default-major-mode 'fetchmail-mode))
    (set-buffer-major-mode
     (get-buffer-create fetchmail-buffer-name))))

(defun fetchmail-insert-buffer (msg)
  "Fetchmail�Хåե��κǸ�˥�å��������������롣"
  (save-selected-window
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert-before-markers msg)))

(defun fetchmail-window-p (&optional all-frames)
  "Fetchmail������ɥ��������Ƥ��뤫�ɤ������ǧ���롣"
  (if (get-buffer-window fetchmail-buffer-name all-frames)
      t
    nil))

(defun fetchmail-open-window ()
  "Fetchmail�Хåե��Υ�����ɥ��򳫤���"
  (interactive)
  (unless (fetchmail-window-p t)
    (set-window-buffer
     (split-window (selected-window) 
		   (- (window-height)
		      (max fetchmail-window-height-lower-limit
			   (min fetchmail-window-height-upper-limit
				(round
				 (* (window-height)
				    fetchmail-window-height-ratio))))
		      1))
     fetchmail-buffer-name)))

(defun fetchmail-close-window ()
  "Fetchmail�Хåե��Υ�����ɥ����Ĥ��롣"
  (interactive)
  (if (fetchmail-window-p t)
      (progn
	(delete-windows-on (get-buffer fetchmail-buffer-name))
	(bury-buffer fetchmail-buffer-name))))

(defun fetchmail-start-process (fetchmail-server)
  "Fetchmail��ư���Ƥ��Υץ������֤���"
  (fetchmail-insert-buffer
   (concat "<<< fetchmail"
	   (if fetchmail-window-time-format
	       (format-time-string fetchmail-window-time-format
				   (current-time)))
	   " >>>\n"))
  (let ((process-connection-type t)
	(fetchmail-process-list (append (list "fetchmail")
					(fetchmail-get-option-list fetchmail-server)
					(list (fetchmail-get-server-name fetchmail-server)))))
    (fetchmail-insert-buffer (concat (mapconcat
				      (lambda (param) param)
				      fetchmail-process-list " ") "\n"))
    (apply (function start-process)
	   fetchmail-process-name
	   fetchmail-buffer-name
	   fetchmail-process-list)))

(defun fetchmail-enter-passwd (fetchmail-process fetchmail-passwd)
  "Fetchmail�ץ����˥ѥ���ɤ����Ϥ��롣"
  (catch 'passwd-entered
    (while t
      (sleep-for 0.1)
      (save-selected-window
	(set-buffer fetchmail-buffer-name)
	(goto-char (process-mark fetchmail-process))
	(beginning-of-line)
	(if (string-match "Enter password"
			  (buffer-substring (point)
					    (process-mark fetchmail-process)))
	    (throw 'passwd-entered nil)))
      (if (or (not (process-status fetchmail-process))
	      (not (eq (process-status fetchmail-process) 'run)))
	(error "Fetchmail aborted in entering password."))))
  (process-send-string fetchmail-process fetchmail-passwd)
  (process-send-eof fetchmail-process))

(defun fetchmail-start (fetchmail-server)
  "Fetchmail���ĤΥ����Ф��Ф��Ƶ�ư���롣"
  (if (get-process fetchmail-process-name)
      (error "Fetchmail is already running."))
  (run-hooks 'fetchmail-preprocess-hook)
  (let ((fetchmail-process (fetchmail-start-process fetchmail-server)))
    (setq fetchmail-running t)
    (force-mode-line-update)
    (setq fetchmail-last-server fetchmail-server)
    (set-process-sentinel fetchmail-process
			  (function fetchmail-finish))
    (if (fetchmail-query-passwd-p fetchmail-server)
	(fetchmail-enter-passwd fetchmail-process
				(base64-decode-string
				 (fetchmail-get-passwd fetchmail-server))))))

(defun fetchmail-finish (fetchmail-process event)
  "Fetchmail�ץ�����λ���θ�����򤹤롣"
  (let ((fetchmail-exit-status
	 (cond
	  ((string-match "finished" event) 'mail)
	  ((string-match "exited" event)
	   (if (= 1 (process-exit-status fetchmail-process))
	       'nomail
	     'failure))
	  (t 'failure))))
    (if (eq fetchmail-exit-status 'failure)
	(fetchmail-clear-passwd fetchmail-last-server))
    (setq fetchmail-running nil)
    (force-mode-line-update)
    (let ((fetchmail-message
	   (cond
	    ((eq 'mail fetchmail-exit-status)
	     "You have mail.")
	    ((eq 'nomail fetchmail-exit-status)
	     "You have no mail.")
	    ((eq 'failure fetchmail-exit-status)
	     "Failed on fetchmail.")
	    (t
	     (error "Invalid fetchmail-exit-status")))))
      (if (eq 'mail fetchmail-exit-status)
	  (run-hooks 'fetchmail-postprocess-hook))
      (fetchmail-insert-buffer (concat fetchmail-message "\n"))
      (unless (fetchmail-window-p)
	(message fetchmail-message))
      (if fetchmail-notify-beep (beep)))))

(defun fetchmail (fetchmail-server)
  "Fetchmail��ư���롣������Ϳ���뤫fetchmail-default-server��
���ꤵ��Ƥ��ʤ��Ȥ��ϡ��ߥ˥Хåե���ʣ���Υ����Ф������򤹤롣"
  (interactive "P")
  (if (and (boundp 'fetchmail-server-param-alist)
	   fetchmail-server-param-alist)
      (error "obsolete variable: fetchmail-server-param-alist"))
  (if (and (boundp 'fetchmail-param-func-alist)
	   fetchmail-param-func-alist)
      (error "obsolete variable: fetchmail-param-func-alist"))
  (unless (stringp fetchmail-server)
    (setq fetchmail-server
	  (cond
	   (fetchmail-server (fetchmail-query-server))
	   (fetchmail-default-server fetchmail-default-server)
	   ((= 1 (length fetchmail-server-param-alist))
	    (car (car fetchmail-server-param-alist)))
	   (t (fetchmail-query-server)))))
  (unless fetchmail-server
    (error "Not selected fetchmail server."))
  (if (fetchmail-query-passwd-p fetchmail-server)
    (fetchmail-query-passwd fetchmail-server))
  (unless (fetchmail-buffer-p)
    (fetchmail-make-buffer))
  (if fetchmail-window
      (fetchmail-open-window))
  (fetchmail-start fetchmail-server))

(defun fetchmail-mode ()
  "Fetchmail�Хåե��ѤΥ᥸�㡼�⡼�ɡ�"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fetchmail-mode)
  (setq mode-name "Fetchmail")
  (use-local-map fetchmail-mode-map)
  (run-hooks 'fetchmail-mode-hook))

(provide 'fetchmail)
