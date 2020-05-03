;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Author: Abdul Bahajaj <abdulbahajaj@gmail.com>


(require 'general)
(general-evil-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq display-line-numbers-type 'relative)

(setq explicit-shell-file-name "/bin/zsh")

(setq org-use-property-inheritance t)

(display-time-mode t)

(setq show-paren-style 'parenthesis)

(set-face-attribute 'show-paren-match nil :background "#FFFF00")

(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil :background "#333333"))

(setq org-log-done 'time)

(setq shell-command-switch "-ic")

(cd "~/repos")

(setq-default tab-width 4)

(setq indent-line-function 'insert-tab)

(display-battery-mode t)

(global-set-key (kbd "<f1>") nil)
(global-set-key (kbd "<f12>") nil)
(global-set-key (kbd "<f13>") nil)
(global-set-key (kbd "<f14>") nil)
(global-set-key (kbd "<f15>") nil)

(defun goto-raspberry-pi () (interactive) (cd "/ssh:rsp:"))

(defun my-filename-to-dir-path (p)
  (if p
      (reduce
       (lambda (acc c)
         (concat acc "/" c))
       (nbutlast (split-string p "/") 1))))

(defun my-open-neotree-at-root () (interactive)
  (neo-global--open-dir
    (or (doom-project-root)
        (my-filename-to-dir-path buffer-file-name)
        (dired-current-directory))))
       ;; (neo-global--open-dir (or buffer-file-name (dired-current-directory))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genearal binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-nmap
    "e" 'evil-embrace-evil-surround-region
    "z g" 'evil-scroll-line-to-bottom
    "r"  'undo-tree-redo
    "-"  'counsel-M-x
 
    ;; In Buffer movement
    "J"  (lambda () (interactive) (evil-next-line 10))
    "K"  (lambda () (interactive) (evil-previous-line 10))
    "L"  (lambda () (interactive) (right-char 10))
    "H"  (lambda () (interactive) (left-char 10))
    "l"  'right-char
    "h"  'left-char

    ;; tabs
    ;; "<SPC> 1" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 1))
    ;; "<SPC> 2" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 2))
    ;; "<SPC> 3" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 3))
    ;; "<SPC> 4" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 4))
    ;; "<SPC> 5" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 5))
    ;; "<SPC> 6" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 6))
    ;; "<SPC> 7" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 7))
    ;; "<SPC> 8" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 8))
    ;; "<SPC> 9" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 9))
    ;; "<f14> g" 'centaur-tabs-toggle-groups
    ;; "<f13> t" 'centaur-tabs-mode

    ;; other
    "M-b" 'evil-buffer-new
    "<f14> t" '+vterm/here
    "<f14> T" 'evil-collection-vterm-toggle-send-escape
    "<f14> s" 'save-buffer
    "<f14> d" '+doom-dashboard/open
    "<SPC> z" (lambda () (interactive) (evil-edit "."))
    "<f14> h" 'hs-hide-level
    "<f14> <f14>" '+default/search-project
    "<f14> P" 'proced
    "<f14> p" 'helm-top
    "<f14> 3" 'swiper-isearch-thing-at-point
    "<f14> f" 'helm-google-suggest
    "<f14> F" '+lookup/online-select
    "<f14> c" 'org-goto-calendar
    "<f14> C" '=calendar
    "<f14> M-c" 'org-date-from-calendar
    "<f14> l" 'magit-log-all
    "<f14> b" 'ibuffer
    "<f14> e" '+eshell/here
    "<f14> m" 'my/make-run
    "<f14> r" 'rename-buffer
    ;; "<f14> z" 'my-open-neotree-at-root
    "<f14> z" '+neotree/open
    "<f14> <f1> c" 'org-schedule
    "S-<SPC> <SPC>" 'counsel-locate
    ;; "<f1>" 'org-agenda
    "<f12>" 'switch-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Splits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; split creation and navigation
(defun my-split-window (pos)
  (cond
    ((string= pos "right")
      (progn
        (split-window-horizontally)
        (evil-window-right 1)))
    ((string= pos "left")
      (split-window-horizontally))
    ((string= pos "up")
      (split-window-vertically))
    ((string= pos "down")
      (progn
        (split-window-vertically)
        (evil-window-down 1)))))


(general-nmap
    ;; Resizing
    "+" 'evil-window-increase-width
    "_" 'evil-window-decrease-width
    "M-=" 'evil-window-increase-height
    "M--" 'evil-window-decrease-height

    ;; split navigation
    "<down>" 'evil-window-down
    "<left>" 'evil-window-left
    "<up>" 'evil-window-up
    "<right>" 'evil-window-right

    ;; moving windows
    "<f13> <right>" #'+evil/window-move-right
    "<f13> <up>" #'+evil/window-move-up
    "<f13> <left>" #'+evil/window-move-left
    "<f13> <down>" #'+evil/window-move-down
 
    ;; split creation
    "<SPC> <right>" (lambda () (interactive) (my-split-window "right"))
    "<SPC> <up>" (lambda () (interactive) (my-split-window "up"))
    "<SPC> <left>" (lambda () (interactive) (my-split-window "left"))
    "<SPC> <down>" (lambda () (interactive) (my-split-window "down")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dirs/files navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro create-folder-nmap (shortcut file-name)
  `(progn
     (defalias (intern (concat "cd-to-" (symbol-name (quote ,file-name))))
       (lambda ()
         (interactive)
         (cd
          (symbol-name
           (quote ,file-name)))))
     (general-nmap ,shortcut
       (intern
        (concat "cd-to-"
                (symbol-name (quote ,file-name)))))))

(create-folder-nmap "<f15> H" ~/                 )
(create-folder-nmap "<f15> P" ~/projects/        )
(create-folder-nmap "<f15> S" ~/scrap/           )
(create-folder-nmap "<f15> R" ~/repos/           )
(create-folder-nmap "<f15> S" ~/repos/scenario/  )
(create-folder-nmap "<f15> T" ~/repos/test-ngp/  )
(create-folder-nmap "<f15> N" ~/repos/norby/     )
(create-folder-nmap "<f15> E" ~/.emacs.d         )
(create-folder-nmap "<f15> D" ~/.doom.d          )
(create-folder-nmap "<f15> O" ~/org              )

(general-nmap
    ;;files/directory management
    ;; "<f15> P" (lambda () (interactive) (cd "~/projects/"))
    ;; "<f15> S" (lambda () (interactive) (cd "~/scrap/"))
    ;; "<f15> R" (lambda () (interactive) (cd "~/repos/"))
    "<f15> l" (lambda () (interactive) (evil-edit "~/org/timeline.org"))
    "<f15> i" (lambda () (interactive) (evil-edit "~/org/triage.org"))
    "<f15> t" (lambda () (interactive) (evil-edit "~/org/todo.org"))
    "<f15> g" (lambda () (interactive) (evil-edit "~/org/gist.org"))
    "<f15> n" (lambda () (interactive) (evil-edit "~/org/notes.org"))
    "<f15> s" (lambda () (interactive) (evil-edit "~/org/scrap.org"))
    "<f15> r" (lambda () (interactive) (evil-edit "~/.zshrc"))
    "<f15> p" (lambda () (interactive) (evil-edit "~/org/projects.org"))
    "<f15> b" (lambda () (interactive) (evil-edit "~/org/books.org"))
    ;; "<f15> P" 'goto-raspberry-pi

    "<f15> c" (lambda () (interactive) (evil-edit "~/.doom.d/config.el"))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Org mode config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org
  (progn
    (setq org-todo-keywords
          '((sequence
             "TODO(t)"
             "ON-GOING(p)"
             "STARTED(s)"
             "BLOCKED(w)"
             "|"
             "DONE(d)"
             "KILL(k)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STARTED" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("BLOCKED" . +org-todo-onhold)
          ("ON-GOING" . +org-todo-active)))
    (set-face-attribute 'org-level-1 nil :weight 'ultra-light  :height 1.2 :foreground "#ebe8e8" :background "#1f1f1f" )
    ;; (set-face-attribute 'org-level-2 nil :box '(:line-width 1 :color "#0d352c") :weight 'ultra-light :height 1.1 :foreground "#34ace0" :background "#0d352c");"#181835");;"#1f1f3f");;"#0d3028")
    ;; (set-face-attribute 'org-level-3 nil  :weight 'ultra-light :height 1.05 :foreground "#aaa69d" )
    ))

;; (use-package! org-fancy-priorities
;;   :hook (org-mode . org-fancy-priorities-mode)
;;   :config (setq
;;            org-bullets-bullet-list '("⁂")
           ;; org-todo-keyword-faces '(("TODO" :foreground "#f7d794" underline t))
           ;; org-fancy-priorities-list '("|||" "|| " "|  ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; vterm specific bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vterm-send-escape()
  (interactive)
  (vterm-send-key "<escape>"))

(with-eval-after-load 'vterm
  (defun vterm (&optional buffer-name)
    "Create a new vterm."
    (interactive)
    (let ((buffer (generate-new-buffer (or buffer-name "vterm"))))
      (with-current-buffer buffer
        (vterm-mode))
      (switch-to-buffer buffer))))

(defun vterm-run-and-go-up ()
    (interactive)
    (vterm-send-return)
    (vterm-send-up))

(defun my-vterm-normal-mode ()
  (interactive)
  (evil-force-normal-state)
  (vterm-copy-mode))

(defun my-vterm-insert ()
  (interactive)
  (vterm-copy-mode -1)
  (evil-insert 1))

(defun my-vterm-append ()
  (interactive)
  (vterm-copy-mode -1)
  (evil-append 1))

(defun my-vterm-clear ()
  (interactive)
  (vterm-clear-scrollback)
  (vterm-clear))

(general-define-key
  :states '(normal)
  :keymaps 'vterm-copy-mode-map
  "a" 'my-vterm-append
  "i" 'my-vterm-insert)

(general-define-key
    :states 'normal
    :keymaps 'vterm-mode-map
    "r" 'vt-source-zshrc)


(general-define-key
  :states '(insert)
  :keymaps 'vterm-mode-map
  "<f15>" 'my-vterm-normal-mode
  "S-<return>" 'vterm-run-and-go-up
  "<f1>" 'my-vterm-clear
  "M-<f13>" 'vt-rc
  "<f13>" 'vterm-send-C-c
  "<f14>" 'vterm-send-escape)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Vterm auto configurations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vt-exec (str)
    (vterm-send-string str)
    (vterm-send-return))

(defun vt-eq (key val)
  (vt-exec
   (format "%s=\"%s\"" key val)))

(defun vt-alias (key val)
  (vt-exec
   (format "alias %s=\"%s\"" key val)))

(defun vt-export (key val)
  (vt-exec
   (format "export %s=%s" key val)))

(defun vt-append-path (path)
  (vt-export
   "PATH"
   (format "%s:$PATH" path)))

(defun vt-source-zshrc ()
  (interactive)
  (vt-exec "source ~/.zshrc"))

(defun vt-rc ()
  (interactive)
  (vt-append-path "~/bin/")
  (vt-exec "bindkey -v")
  (vt-eq "PROMPT" "%n %5~# ")
  (vt-alias "l" "ls")
  (vt-alias "c" "clear")
  (vt-alias "ktl" "kubectl")
  ;; (vt-alias "la" "ls -lAh")
  (vt-alias "la" "ls -lAh")
  (vt-alias "ll" "ls -lh")
  (vt-alias "pod" "popd")
  (vt-alias "pd" "pushd")
  (vt-alias "...." "cd ../../..")
  (vt-alias "..." "cd ../..")
  (vt-alias ".." "cd .."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Random functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'mcs/lisp-insert-title
   [?4 ?5 ?a ?\; ?\; escape ?y ?y ?p ?p ?k ?l ?l ?l ?l ?l ?l ?h ?h ?d ?$ ?a ?  ?q backspace escape])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Removign yanking when deleting stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; Remove yank from delete
(evil-define-operator evil-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  ;; (let ((evil-was-yanked-without-register nil))
  ;;   (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (called-interactively-p 'any)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator evil-org-delete-char (count beg end type register)
  "Combine evil-delete-char with org-delete-char"
  :motion evil-forward-char
  (interactive "p<R><x>")
  (if (evil-visual-state-p)             ; No special support for visual state
      (evil-delete-char beg end type register)
    (evil-set-register ?- (filter-buffer-substring beg end))
    ;; (evil-yank beg end type register)
    (org-delete-char count)))

(evil-define-operator evil-org-delete-backward-char (count beg end type register)
  "Combine evil-delete-backward-char with org-delete-backward-char"
  :motion evil-backward-char
  (interactive "p<R><x>")
  (if (evil-visual-state-p)             ; No special support for visual state
      (evil-delete-backward-char beg end type register)
    (evil-set-register ?- (filter-buffer-substring beg end))
    ;; (evil-yank beg end type register)
    (org-delete-char count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Old code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   '(default ((t (:stipple nil
;;                 :background "Black"
;;                 :foreground "white"
;;                 :inverse-video nil
;;                 :box nil
;;                 :strike-through nil
;;                 :overline nil
;;                 :underline nil
;;                 :slant normal
;;                 :weight medium
;;                 l
;;                 :family "Roboto Mono Medium for Powerline")))))
