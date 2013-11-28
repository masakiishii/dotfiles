;============== INIT ==============
;(set-language-environment 'Japanese)
;(set-default-coding-systems 'utf-8-mac)
;(set-terminal-coding-system 'utf-8-mac)
;(set-keyboard-coding-system 'utf-8-mac)
;(set-clipboard-coding-system 'utf-8-mac)
;(setq-default buffer-file-coding-system 'utf-8-mac)
;(prefer-coding-system 'utf-8-mac)

(if (boundp 'window-system)
    (setq initial-frame-alist
          (append (list
		   '(width . 150) ;; ウィンドウ幅
		   '(height . 38) ;; ウィンドウの高さ
		   '(top . 0)	  ;;表示位置
		   '(left . 5)	  ;;表示位置
		   '(alpha  . 80)
		   )
		  initial-frame-alist)))
(if window-system (set-scroll-bar-mode 'right))
(file-name-shadow-mode t)
(setq default-frame-alist initial-frame-alist)
(setq truncate-lines nil)
(setq truncate-partial-width-windows t)


;;パス
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))
(autoload 'gtags-mode "gtags" "" t)

;;============== Global Set Key ==============
(global-set-key "\M-t" 'gtags-find-tag)        ;definition
(global-set-key "\M-r" 'gtags-find-rtag)       ;reference
(global-set-key "\M-s" 'gtags-find-symbol)
(global-set-key "\M-p" 'gtags-find-pattern)
;;(global-set-key "\M-f" 'gtags-find-file)
(global-set-key "\C-t" 'gtags-pop-stack)
(global-set-key "\M-c" 'compile)
(global-set-key "\C-h" 'delete-backward-char)
(global-unset-key "\C-o")
(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-cc" 'comment-region) ; C-c c を範囲指定コメントに
(global-set-key "\C-cu" 'uncomment-region) ; C-c u を範囲指定コメント解除に
(global-set-key "\C-x\C-j" 'goto-line) ;指定行ジャンプ
(global-set-key "\C-q" 'set-mark-command)
					;(global-set-key "\C-c\C-q" 'mark-whole-buffer 'indent-region) ; indent-region
(global-set-key [f6] 'setnu-mode)
(global-set-key [f5] 'replace-string)
(windmove-default-keybindings)

;;============== Indent ==============
;;(setq default-tab-width 4)
(setq tab-width 4)
(setq indent-line-function 'indent-relative-maybe) ; 前と同じ行の幅にインデント

;;============== Font ==============
(if (eq window-system 'mac) (progn
			      (require 'carbon-font)
			      (fixed-width-set-fontset "osaka" 14)))

;;============== Key Event ==============
;;(setq mac-allow-anti-aliasing t)       ; mac 固有の設定
;;(setq mac-option-modifier 'meta)	   ; mac 用の command キーバインド
;;(if window-system (mac-key-mode 1))	   ; MacKeyModeを使う

;;============== Util Function ==============
(show-paren-mode t)                     ; 対応する括弧を光らせる。
(transient-mark-mode t)	                ; 選択部分のハイライト
(setq require-final-newline t)          ; always terminate last line in file
(setq major-mode 'text-mode)    ; default mode is text mode
(setq completion-ignore-case t)         ; file名の補完で大文字小文字を区別しない
(setq partial-completion-mode 1)        ; 補完機能を使う
(setq inhibit-startup-message t)        ;hide start-up message
(setq make-backup-files nil)
(blink-cursor-mode 0)
(column-number-mode t)
(setq scroll-step 1)
(which-function-mode 1)
(if window-system (menu-bar-mode 1) (menu-bar-mode nil))
;;(menu-bar-mode nil)

(setq elscreen-prefix-key "\C-o")
(if window-system (load "elscreen" "ElScreen" t)) ;elscreen

(add-to-list 'load-path "~/.emacs.d/auto-complete")        ;auto-complete設定
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict") ;auto-complete設定
(require 'auto-complete-config)                            ;auto-complete設定
(ac-config-default)                                        ;auto-complete設定
;;(require 'auto-complete) ;auto-complete
;;(global-auto-complete-mode t)
(if window-system (progn(tool-bar-mode nil))) ;hide tool-bar


;;====================================
;;jaspace.el を使った全角空白、タブ、改行表示モード
;;切り替えは M-x jaspace-mode-on or -off
;;====================================
(require 'jaspace)
;; 全角空白を表示させる。
(setq jaspace-alternate-jaspace-string "□")
;; 改行記号を表示させる。
(setq jaspace-alternate-eol-string "$\n")
;; タブ記号を表示。
(setq jaspace-highlight-tabs t)  ; highlight tabs

;; EXPERIMENTAL: On Emacs 21.3.50.1 (as of June 2004) or 22.0.5.1, a tab
;; character may also be shown as the alternate character if
;; font-lock-mode is enabled.
;; タブ記号を表示。
					;(setq jaspace-highlight-tabs ?&gt;) ; use ^ as a tab marker

					;============== Color ==============
(if window-system (progn
		    (add-to-list 'default-frame-alist '(foreground-color . "snow"))
		    (add-to-list 'default-frame-alist '(background-color . "black"))
		    (add-to-list 'default-frame-alist '(cursor-color . "snow"))
		    (set-face-foreground 'modeline "white")
					;(set-face-background 'modeline "dodgerblue")
		    (set-face-background 'modeline "firebrick")
		    (set-face-background 'region "RoyalBlue4")
		    (set-frame-parameter nil 'alpha 80)
		    ))
;
;;============== Opacity ==============
(add-to-list 'default-frame-alist '(alpha . 85))

;;============== change default color ==============
(add-hook 'font-lock-mode-hook '(lambda ()
				  (set-face-foreground 'font-lock-builtin-face "#ff9999")
				  (set-face-foreground 'font-lock-comment-face "slate gray")
				  (set-face-foreground 'font-lock-string-face  "spring green")
				  (set-face-foreground 'font-lock-keyword-face "#40E0D0")
				  (set-face-foreground 'font-lock-constant-face "violet")
				  (set-face-foreground 'font-lock-function-name-face "#ff8c00")
				  (set-face-foreground 'font-lock-variable-name-face "#00ffff")
				  (set-face-foreground 'font-lock-type-face "#ffaadd")
				  (set-face-foreground 'font-lock-warning-face "magenta")
				  (set-face-bold-p 'font-lock-function-name-face "#ff9999")
				  (set-face-bold-p 'font-lock-warning-face "#ff9999")
				  ))

;;============== Konoha-mode ==============
(autoload 'konoha-mode "konoha-mode" "Major mode for editing Konoha code." t)
(setq auto-mode-alist (append '(("\\.k$" . konoha-mode)) auto-mode-alist))
(put 'set-goal-column 'disabled nil)

;;============== Scala-mode ==============
(autoload 'scala-mode "Scala-mode" "Major mode for editing Scala code." t)
(setq auto-mode-alist (append '(("\\.scala$" . scala-mode)) auto-mode-alist))
(put 'set-goal-column 'disabled nil)

;;============== Mode Hook ===============
(add-hook 'c-mode-hook
	  '(lambda ()
	     (gtags-mode t)
	     (c-set-style "linux")
	     (setq tab-width 4)
	     (setq indent-tabs-mode t)
	     (setq c-basic-offset tab-width))
	  )

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (gtags-mode t)
	     (c-set-style "linux")
	     (setq tab-width 4)
	     (setq indent-tabs-mode t)
	     (setq c-basic-offset tab-width))
	  )

(add-hook 'javascript-mode-hook
	  '(lambda ()
	     (gtags-mode t)
	     (c-set-style "linux")
	     (setq tab-width 4)
	     (setq indent-tabs-mode t)
	     (setq c-basic-offset tab-width))
	  )

;; cmake mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))


;;set linenumber like linux mint

(setq load-path
      (append
       (list
	(expand-file-name "~/.emacs.d")
	(expand-file-name "~/.emacs.d/auto-install")
	)
       load-path))

(require 'linum)
(global-linum-mode t)
(setq linum-fomat "%4d ")
(set-face-attribute 'linum nil
                    :foreground "springgreen1")
(put 'upcase-region 'disabled nil)

;;=======line-number==========
;; (require 'wb-line-number)
;; (wb-line-number-toggle)
;; (custom-set-faces
;;  '(wb-line-number-face ((t (:foreground "springgreen1"))))
;;  '(wb-line-number-scroll-bar-face
;;    ((t (:foreground "white" :background "LightBlue1")))))
;; (setq truncate-partial-width-windows nil) ; use continuous line

;;=====tmux=====
(defun terminal-init-screen () 
  "Terminal initialization function for screen." 
  ;; Use the xterm color initialization code. 
  (load "term/xterm") 
  (xterm-register-default-colors))

;;=========sr.speedbar.el=========
;;(require 'sr-speedbar)    
;;(setq sr-speedbar-right-side nil) 

;;=========backcolor is transparent.========
(setq default-frame-alist
      (append (list
               '(alpha . (90 85))
;;'(alpha . (91 nil nil nil))
               ) default-frame-alist))


;;====================================
;;(set-frame-parameter nil 'alpha 80)
;;(set-alpha '(90 75))

;;========backcolor is transparent by plugin.========
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-gnome2)))

;;=======auto-install.el===============
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(require 'auto-install)

(auto-install-update-emacswiki-package-name t)

(auto-install-compatibility-setup)

(setq ediff-window-setup-funtion 'ediff-setup-windows-plain)

;;=======sequential-command.el==========
(require 'sequential-command-config)
(sequential-command-setup-keys)

;;======auto-async-byte-compile.el=====
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; ;=======haskel-mode==================                                                                                 
(autoload 'ghc-init "ghc" nil t)

(defun haskell-individual-setup ()
  (let ((mapping '(([f5] . "\C-c\C-l\C-x\omain\C-m\C-xo")
                   ("\C-c\C-i" . ghc-complete)
                   ([backtab] . haskell-indent-cycle))))    
    (loop for (key . f) in mapping
          do (define-key haskell-mode-map key f))

    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indent)
    (imenu-add-menubar-index)
    (ghc-init)
    (flymake-mode)))

(add-hook 'haskell-mode-hook 'haskell-individual-setup)
(put 'downcase-region 'disabled nil)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

;;==========coq mode==============================
;;(load-file "/usr/local/ProofGeneral/generic/proof-site.el")

;;==========<<<yasnippet.el>>>====================
;;(require 'yasnippet-config)
;;(yas/setup "~/.emacs.d/yasnippet-0.6.1c")

;;==========<<<Tuareg-mode(Ocaml)>>>===============
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode)
		("\\.topml$" . tuareg-mode))
	      auto-mode-alist))
