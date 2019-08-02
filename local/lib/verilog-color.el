;;----------------------------------------------------------------------
;; $B%+%i%U%k(B Verilog $B%b!<%I(B
;;----------------------------------------------------------------------
;; $B@bL@(B:
;;   Verilog-HDL $B$r(B emacs(mule)$B>e$G5-=R$9$k;~$K(B hilit19 $B$rMxMQ$7$F(B
;;   $B%-!<%o!<%I$K?'$r$D$1$k%W%m%0%i%`$G$9(B
;;
;; $BJdB-(B:
;;  $B!&K\%W%m%0%i%`$r;HMQ$9$k$K$O!"(Bcolor-def.el $B$bI,MW$H$J$j$^$9!#(B
;;     ($B"*(B http://www.pa.airnte.ne.jp/tsuruta/programs/color-def.el)
;;  $B!&(Bverilog-mode.el $B$H$NJ;MQ$b2DG=$G$9!#(B
;;     ($B"*(B ftp://ftp.siliconlogic.com/pub/comp.lang.verilog/verilog-mode.Z)
;;
;; $B!V(B.emacs$B!W$N@_Dj(B:
;;    (setq load-path (cons ("$B%W%m%0%i%`$NCV>l=j(B") load-path))
;;    (load "color-def")
;;    (load "verilog-color")
;;    $B"((B verilog-mode.el $B$r;HMQ$9$k>l9g$O(B (load "verilog") $B$NA0$K(B
;;       (setq use-verilog-mode t) $B$r2C$($F2<$5$$(B
;;
;; $BA`:n(B:
;;   [ESC]-[z]        : auto-indent $B$N@Z$jBX$((B(verilog-mode)
;;----------------------------------------------------------------------
;;                        Copyright (C) 1995 - 1997 TSURUTA Mitsutoshi
;;----------------------------------------------------------------------
(defvar use-verilog-mode nil "usr verilog mode")

(cond ((null use-verilog-mode)
       (defun verilog-mode ()
         "Verilog-mode $B$N%@%_!<!"(BVerilog $B$NM=Ls8l$K?'$rIU$1$?$$;~$K;H$&!#(B"
         (interactive)
         (kill-all-local-variables)
         (setq major-mode 'verilog-mode)
         (setq mode-name "Verilog")))
      (t
       (autoload 'verilog-mode "verilog-mode" nil t)))
       
(setq auto-mode-alist 
  (append (list (cons "\\.v$" 'verilog-mode)
                (cons "\\.bhv$" 'verilog-mode)
                (cons "\\.rtl.*$" 'verilog-mode)
                (cons "testfixture.*" 'verilog-mode)
                (cons "\\.verilog$" 'verilog-mode))
          auto-mode-alist))

;(setq verilog-mode-hook 
;  '(lambda ()
;     (progn
;       (setq verilog-tab-always-indent t  ; [tab]$B%-!<$r%$%s%G%s%HMQ$K$9$k$+(B
;             verilog-auto-newline t       ; $B<+F0E*$K2~9T$r$9$k$+(B
;             verilog-auto-endcomments nil ; endXXX $B$N8e$m$K<+F0E*$K%3%a%s%H$r(B
;                                          ; $BF~$l$k$+!J"-$3$s$J46$8!K(B
;                                          ; function [a:b] XXX;
;                                          ; endfunction /* XXX */
;             verilog-indent-level 2       ; $B%$%s%G%s%H?<$5(B
;             verilog-continued-expr 2     ; $B#29T$K$o$?$k;~$K<!$N9T$N%$%s%G%s%H(B
;             verilog-label-offset 0       ; $B%i%Y%k$N%$%s%G%s%H%l%Y%k(B
;             verilog-case-offset 2        ; case $BJ8$N%$%s%G%s%H%l%Y%k(B
;             )
;       (define-key verilog-mode-map "\M-z" 'change-verilog-auto-newline))))

(defun change-verilog-auto-newline ()
  "$B<+F0E*$K2~9T$r$9$k$+$7$J$$$+$N%9%$%C%A(B
   [ESC][z] $B$G!"@Z$jBX$o$k!#(B"
  (interactive)
  (if (null verilog-auto-newline)
    (message "verilog-auto-newline mode : ON")
    (message "verilog-auto-newline mode : OFF"))
  (setq verilog-auto-newline (not verilog-auto-newline)))

(cond
  (window-system
    (hilit-set-mode-patterns
      'verilog-mode
      '(("//.*$" nil comment)
        ("/\\*.*" "\\*/" comment)
        ("`define" ".$" define)
        ("`include" ".$" define)
        ("`ifdef" ".$" define) ("`else" ".$" define) ("`endif" ".$" define)
        ("\\<\\(module\\|endmodule\\|function\\|endfunction\\)\\>" nil defun)
	("\\<\\(task\\|endtask\\|fork\\|join\\)\\>" nil defun)
        ("\\<\\(always\\|initial\\|assign\\|deassign\\|parameter\\)\\>" 1 keyword)
        ("\\<\\(posedge\\|negedge\\|or\\)\\>" 1 navy)
	("\\<\\(if\\|else\\|case\\|casez\\|casex\\|endcase\\)\\>" 1 keyword)
        ("\\<\\(begin\\|end\\|for\\|while\\|repeat\\|wait\\)\\>" 1 keyword)
        ("\\(\@\\|\#\\)" 1 keyword)
        ("\\<\\(input\\|output\\|inout\\|reg\\|wire\\)\\>" nil include)
        ("\\<\\(integer\\|real\\|time\\)\\>" nil include)
        ("\\(\$finish\\|\$stop\\)" nil glob-struct)
        ("\\(\$display\\|\$write\\|$monitor\\)" nil glob-struct)
        ("\\(\$random\\|\$readmemb\\|\$readmemh\\)" nil glob-struct)
        ("\\(\+\\|\-\\|\*\\|\/\\|\%\\)" 1 msg-quote)
        ("\\(\~\\|\&\\|\|\\|\\^\\|\!\\|\=\\|\<\\|\>\\)" nil msg-quote)
        ("[0-9]\*\'b[01zxZX_?]\*" nil DarkOrange)
        ("[0-9]\*\'h[0-9A-Fa-fzxZX_]\*" nil DarkOrange)
        ("[0-9]\*\'o[0-7zxZX_]\*" nil DarkOrange)
        ("[0-9]\*\'d[0-9zxZX_]\*" nil DarkOrange)
        ("\\<[0-9]\+\\>" nil OrangeRed)
        ("`[a-zA-Z0-9]\*" nil darkgreen)
        ))))
