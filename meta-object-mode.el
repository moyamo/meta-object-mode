;; -*- lexical-binding: t -*-

(defvar meta-object-emulation-keymap (make-sparse-keymap))
(defvar meta-object-emulation-alist `((meta-object-mode . ,meta-object-emulation-keymap)))

;;;###autoload
(define-minor-mode meta-object-mode
  nil                                   ; TODO write a doc
  :init-value nil
  :lighter " M-O"
  :keymap nil
  :global t
  (when meta-object-mode
    (add-to-list 'emulation-mode-map-alists 'meta-object-emulation-alist)
    ;; Company keys because company uses emulation mode too
    ;; TODO undo this when we disable the mode and remove the M-p and M-n bindings
    (defun meta-object-bind-company ()
      (bind-key "M-q" #'company-select-previous company-active-map)
      (bind-key "M-a" #'company-select-next company-active-map))
    (when (featurep 'company)
      (meta-object-bind-company))
    (add-hook 'company-mode-hook #'meta-object-bind-company)))

(defun my/begin-prev-line ()
  "If at the beginning of line move to previous line else move to beginning of line."
  (interactive)
  (let ((p (point)))
    (beginning-of-line)
    (when (= p (point))
      (beginning-of-line 0))))

(defun my/end-next-line ()
  "If at the beginning of line move to previous line else move to beginning of line."
  (interactive)
  (let ((p (point)))
    (end-of-line)
    (when (= p (point))
      (end-of-line 2))))

(defun meta-object-kill ()
  "Kill the current meta object."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-word 1)))

(defun meta-object-bad-key ()
  (interactive)
  (message "Bad key don't use it"))

(defun meta-object--get-usual-key (key)
  (let ((meta-object-mode nil))
    (key-binding (kbd key))))

;; n and p are after C-n and C-p because n and p usually do more complicated things
;; when n and p are not self-insert but C-n and C-p pretty much always down and up line
(defvar down-keys '("C-n" "n" "M-n" "C-c n" "C-c C-n" "C-c M-n"))
(defvar up-keys '("C-p" "p" "M-p" "C-c p" "C-c C-p" "C-c M-p"))
;; We don't have f and b here because f and b usually do weird things unrelated
;; to forward and backward when they are not self-insert
(defvar forward-keys '("C-f" "M-f" "C-c f" "C-c C-f" "C-c M-f"))
(defvar backward-keys '("C-b" "M-b" "C-c b" "C-c C-b" "C-c M-b"))

(defun meta-object--is-real-command (symb)
  (if (not symb)
      nil
    (let ((name (if (symbolp symb) (symbol-name symb) "Sometimes the command is not a symbol")))
      (if (string-match-p ".*self-insert.*" name)
          nil
        t))))

(defun meta-object--get-real-commands (keylist)
  (-distinct
   (-filter #'commandp
            (-filter #'meta-object--is-real-command
                     (-map #'meta-object--get-usual-key keylist)))))

(defmacro meta-object-make-default-command (command-name key-list default-condition default-command getter)
  (let ((options (gensym "options"))
        (command (gensym "command")))
      `(defun ,command-name ()
         (interactive)
         (let ((,options (meta-object--get-real-commands ,key-list))
               ,command)
           (message "Keys are %s" ,options)
           (setq ,command (if (,default-condition ,options)
                             ,default-command
                            (,getter ,options)))
           ;; The follow line fixes a weird bug where moving down and up would
           ;; cause the command to forget the column number
           (setq this-command ,command)
           (call-interactively ,command)))))

(meta-object-make-default-command meta-object-down down-keys null #'next-line car)
(meta-object-make-default-command meta-object-up up-keys null #'previous-line car)
(meta-object-make-default-command meta-object-forward forward-keys null #'forward-char car)
(meta-object-make-default-command meta-object-backward backward-keys null #'backward-char car)


(meta-object-make-default-command meta-object-secondary-down down-keys (lambda (x) (> 2 (length x))) #'my/end-next-line cadr)
(meta-object-make-default-command meta-object-secondary-up up-keys (lambda (x) (> 2 (length x))) #'my/begin-prev-line cadr)
(meta-object-make-default-command meta-object-primary-forward forward-keys (lambda (x) (> 2 (length x))) #'forward-word cadr)
(meta-object-make-default-command meta-object-primary-backward backward-keys (lambda (x) (> 2 (length x))) #'backward-word cadr)


(defun meta-object-unbind-standard-keys ()
  "Unbind the standard Emacs keys that have been replaced by meta-object keys.
This is to help you learn the new keys and to prevent you from
relying on old habits."
  (interactive)
  (-map
   (lambda (k) (define-key meta-object-emulation-keymap (kbd k) #'meta-object-bad-key))
   '(
    ;; arrow keys
    "<up>"
    "<left>"
    "<right>"
    "<down>"
    ;; Char-Line navigation
    "C-p"
    "C-n"
    "C-f"
    "C-b"
    ;; Word navigation
    "M-b"
    ;; "M-f"
    ;; Begin-ENd line
    "C-a"
    "C-e"
    ;; Cut-Copy-Paste
    "C-y"
    "M-y"
    "C-w"
    "M-w"
    ;; Set-mark
    "C-@"
    ;; Secondary up and down
    "M-n"
    "M-p"
    )))

(defun meta-object-bind-keys ()
  (bind-key "M-e" #'meta-object-up meta-object-emulation-keymap)
  (bind-key "M-d" #'meta-object-down meta-object-emulation-keymap)
  (bind-key "M-f" #'meta-object-forward meta-object-emulation-keymap)
  (bind-key "M-s" #'meta-object-backward meta-object-emulation-keymap)
  (bind-key "M-r" #'meta-object-primary-forward meta-object-emulation-keymap)
  (bind-key "M-w" #'meta-object-primary-backward meta-object-emulation-keymap)
  (bind-key "M-q" #'meta-object-secondary-up meta-object-emulation-keymap)
  (bind-key "M-a" #'meta-object-secondary-down meta-object-emulation-keymap)
  (bind-key "M-c" #'kill-ring-save meta-object-emulation-keymap)
  (bind-key "M-'" #'helm-M-x meta-object-emulation-keymap)
  (bind-key "M-x" #'meta-object-kill meta-object-emulation-keymap)
  (bind-key "M-v" #'helm-show-kill-ring meta-object-emulation-keymap)
  (bind-key "C-v" #'yank meta-object-emulation-keymap)
  (bind-key "M-z" #'set-mark-command meta-object-emulation-keymap)
  (bind-key "M-B" #'fill-paragraph meta-object-emulation-keymap))

(meta-object-unbind-standard-keys)
(meta-object-bind-keys)

(provide 'meta-object-mode)
;;; meta-object-mode.el ends here
