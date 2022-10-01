
(defmacro hoon-get-or-set (sym &rest body)
  `(if ,sym ,sym (progn (setq ,sym (progn ,@body)) ,sym)))

(provide 'hoon-utils)
