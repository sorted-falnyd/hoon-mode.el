(defgroup hoon-herb nil
  "Hoon Herb."
  :prefix "hoon-herb-"
  :group 'hoon)

(defcustom hoon-herb-executable "herb"
  "Herb executable name."
  :group 'hoon
  :type 'string)

(defcustom hoon-herb-executable-path nil
  "Herb executable path."
  :group 'hoon
  :type 'string)

(defcustom hoon-herb-args "-d"
  "Args for herb."
  :group 'hoon
  :type 'string)

(defun hoon-herb-path ()
  "Find Hoon herb executable."
  (if hoon-herb-executable-path
      hoon-herb-executable-path
    (progn
      (setq hoon-herb-executable-path (executable-find hoon-herb-executable))
      hoon-herb-executable-path)))

(defun hoon-herb-eval-region ()
  "Eval current region in herb."
  (interactive)
  (shell-command
   (concat (hoon-herb-path) " " hoon-herb-args " "
           (shell-quote-argument (buffer-substring (region-beginning) (region-end)))
           " &")))

(defun hoon-herb-eval-buffer ()
  "Eval current buffer in herb."
  (interactive)
  (shell-command
   (concat (hoon-herb-path) " " hoon-herb-args " "
           (shell-quote-argument (buffer-substring-no-properties (point-min) (point-max)))
           " &")))

(defun hoon-herb-eval (expression)
  "Eval EXPRESSION in herb."
  (interactive
   (list (read-from-minibuffer "Hoon: "
                               (when (region-active-p)
                                 (buffer-substring (region-beginning) (region-end))))))
  (shell-command
   (concat (hoon-herb-path) " " hoon-herb-args " "
           (shell-quote-argument expression)
           " &")))

(provide 'hoon-herb)
