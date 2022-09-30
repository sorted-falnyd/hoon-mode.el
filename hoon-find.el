(defgroup hoon-find nil
  "Hoon find src/doc utilities."
  :prefix "hoon-find-"
  :group 'hoon)

(defcustom hoon-find-docs-dir "content/docs/hoon"
  ""
  :type 'string
  :group 'hoon-find)

(defcustom hoon-find-source-dir "."
  ""
  :type 'string
  :group 'hoon-find)

(defcustom hoon-find-forge-path "https://github.com/urbit"
  "" :type 'string :group 'hoon-find)
(defcustom hoon-find-forge-docs-project "urbit.org" "" :type 'string :group 'hoon-find)
(defcustom hoon-find-forge-source-project "urbit" "" :type 'string :group 'hoon-find)

(defcustom hoon-find-cache-directory ".emacs.d/.cache/hoon-find" "" :type 'string :group 'hoon-find)

(defun hoon-find--cache-directory ()
  (concat user-home-directory hoon-find-cache-directory))

(defun hoon-find--docs-directory ()
  (concat (hoon-find--cache-directory) "/" hoon-find-forge-docs-project))

(defun hoon-find--source-directory ()
  (concat (hoon-find--cache-directory) "/" hoon-find-forge-source-project))

(defun hoon-find-ensure-cache-dir ()
  (make-directory (hoon-find--cache-directory) :parents))

(defun hoon-find-ensure-docs-project ()
  (shell-command
   (concat "git clone --depth 1" hoon-find-forge-path "/" hoon-find-forge-docs-project " " (hoon-find--docs-directory))
   "*Hoon Find*"
   "*Hoon Find Errors*"))

(defun hoon-find-ensure-source-project ()
  (shell-command
   (concat "git clone " hoon-find-forge-path "/" hoon-find-forge-source-project " " (hoon-find--source-directory))
   "*Hoon Find*"
   "*Hoon Find Errors*"))

(defvar hoon-docs-dir "/home/jake/vault/projects/urbit/git/urbit.org/"
  "Location of hoon docs.")
(defvar hoon-source-dir "/home/jake/vault/projects/urbit/git/urbit"
  "Location of hoon docs.")

(defcustom hoon-docs-rune-dir
  (concat hoon-docs-dir "/" "reference/rune/")
  "Location of hoon rune docs."
  :type 'string)

(defun hoon-goto-symbol ()
  "Go to symbol definition."
  (interactive)
  (cond
   ((setq-local current-fnsym (hoon--current-fnsym))
    (hoon-grep-docs (format " %s " current-fnsym) '("-F")
                    hoon-docs-dir nil))
   ((setq-local current-aura (hoon--current-aura))
    (hoon-grep-docs (format "%s" current-aura) '("-F")
                    hoon-docs-dir nil))
   ((setq-local current-rune (hoon--current-rune))
    (hoon-grep-docs (format "`%s`" current-rune) '("-F") hoon-docs-rune-dir nil))))

(defun hoon-grep-source (input)
  (interactive "P")
  (+helm-file-search :query input :in hoon-source-dir))

(defun hoon-grep-docs (input &optional opts dir use-helm)
  (interactive "P")
  (if use-helm
      (helm-do-grep-ag hoon-docs-dir)
    (let* ((default-directory (if dir dir hoon-docs-dir))
           (keyword (substring-no-properties input) )
           (shell-keyword (shell-quote-argument keyword))
           (command-template "noglob rg --vimgrep --no-heading --smart-case %s %s .")
           (command-string (format command-template
                                   (if opts (-reduce 'string-join opts) "")
                                   shell-keyword))
           (command-count-string
            (format command-template
                    (concat
                     (if opts (-reduce 'string-join opts) "")
                     " --count-matches --no-filename ")
                    shell-keyword))
           (maybe-count
            (-some-->
                (shell-command-to-string
                 command-count-string)
              (->> (s-lines it) (-filter (-not 's-blank?)))
              (-map 'string-to-number it)
              (-reduce '+ it)))
           (count (if maybe-count maybe-count 0))
           (val (unless (= count 0)
                  (progn
                    (message "Running: %s" command-string)
                    (-some-->
                        (shell-command-to-string
                         command-string)
                      (split-string it "\n" t)))))
           (lst (if val (split-string (car val) ":")))
           (linenum (if val (string-to-number (cadr lst)))))
      (progn
        (if (= count 0) (+helm-file-search :query keyword :in hoon-source-dir :args opts) (deactivate-mark))
        (if (> count 1)
            (progn
              (+helm-file-search :query keyword :in hoon-docs-dir :args opts)
              (deactivate-mark))
          (unless (length< lst 1)
            (print lst)
            (print linenum)
            (deactivate-mark)
            ;; open file
            (find-file (car lst))
            ;; goto line if line number exists
            (when (and linenum (> linenum 0))
              (goto-char (point-min))
              (forward-line (1- linenum)))))))))
