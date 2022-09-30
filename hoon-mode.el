;;; hoon-mode.el --- Major mode for editing hoon files for urbit

;; Copyright (C) 2014–2016 Urbit

;; Author:
;;    * Adam Bliss        https://github.com/abliss         <abliss@gmail.com>
;; Contributors:
;;    * N Gvrnd           https://github.com/ngvrnd
;;    * TJamesCorcoran    https://github.com/TJamesCorcoran <jamescorcoran@gmail.com>
;;    * Rastus Vernon     https://github.com/rastus-vernon  <rastus.vernon@protonmail.ch>
;;    * Elliot Glaysher   https://github.com/eglaysher      <erg@google.com>
;;    * David Kerschner   https://github.com/baudtack       <dkerschner@hcoop.net>
;;    * Johnathan Maudlin https://github.com/jcmdln         <jcmdln@gmail.com>
;;
;; URL: https://github.com/urbit/hoon-mode.el
;; Version: 0.1
;; Keywords: extensions, hoon, nock, urbit, Mars
;; Package-Requires: ((emacs "28.1") (dash "2.19.1") (s "1.13.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is my first Major Mode, so don't expect much. It's heavily based on
;; SampleMode from the Emacs wiki.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

(defvar hoon-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Basic quoting support
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; Hoon comments. Also mark ':' as a normal punctuation character.
    (modify-syntax-entry ?: ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Add dash to the symbol class since it can be part of identifier.
    (modify-syntax-entry ?- "_" st)

    ;; Put all other characters which can be part of runes in the punctuation
    ;; class so that forward and backward work properly.
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry '(?\# . ?\&) "." st)
    (modify-syntax-entry '(?* . ?\,) "." st)
    (modify-syntax-entry '(?. . ?/) "." st)
    (modify-syntax-entry '(?\; . ?@) "." st)
    (modify-syntax-entry '(?^ . ?_) "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?~ "." st)
    st)
  "Syntax table for `hoon-mode'.")

(defvar hoon-mode-map
  (let ((map (make-sparse-keymap)))
    map))
(rx-define gap (and space (one-or-more space)))
(rx-define identifier (one-or-more (or "." lower digit "-" "+" "<" ">")))
(rx-define mold (and lower (zero-or-more (or lower digit "-"))))
(rx-define wing (or "*"
               "?"
               "^"
               (and "@" (zero-or-more word))
               (and (opt "$-")
                    "("
                    (one-or-more
                     (or (or alphanumeric "(" ")" "*" "?" "@" "-" ":"
                             "^" "_")
                         ;; Spaces must be single.
                         (and space (or alphanumeric "(" ")" "*" "?"
                                        "@" "-" ":" "^" "_"))))
                    ")")
               (and lower (one-or-more (or lower digit "-" ":" "^")))
               "$-"
               ))
(rx-define arm (and (group "+" (or "+" "-" "$" "*")) gap
                    (group (or "$" identifier))))
(rx-define arm-comment (and (zero-or-more gap) "::" (zero-or-more gap) "+"))

(eval-and-compile
  (defconst hoon-rx-constituents
    `((gap . ,(rx (and space (one-or-more space))))
      (identifier . ,(rx (and lower (zero-or-more (or lower digit "-")))))
      (mold . ,(rx (or "*"
                       "?"
                       "^"
                       (and "@" (zero-or-more word))
                       (and (opt "$-")
                            "("
                            (one-or-more
                             (or (or alphanumeric "(" ")" "*" "?" "@" "-" ":"
                                     "^" "_")
                                 ;; Spaces must be single.
                                 (and space (or alphanumeric "(" ")" "*" "?"
                                                "@" "-" ":" "^" "_"))))
                            ")")
                       (and lower (one-or-more (or lower digit "-" ":" "^")))
                       "$-"
                       )))
      (wing . ,(rx (one-or-more (or "." lower digit "-" "+" "<" ">"))))
      )
    "Common patterns used in font locking hoon code.")

  (defmacro hoon-rx (&rest regexps)
    "Hoon mode specialized rx macro."
    (let ((rx-constituents (append hoon-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t)))))

  (defmacro hoon-rx-imenu (&rest regexps)
    "Hoon mode specialized rx macro."
    (let ((rx-constituents (append hoon-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             `(and ,@regexps))
            (t
             (car regexps))))))

(defconst hoon-arm-rx
  (hoon-rx-imenu (rx (and (group "+" (or "+" "-" "$" "*")) gap
                          (group (or "$" identifier))))))

(defconst hoon-font-lock-arm-declarations-rx
  (hoon-rx (and (group "+" (or "+" "-" "$" "*")) gap
                (group (or "$" identifier))))
  "Regexp of declarations")


(defconst hoon-font-lock-face-mold-old-rx
  (hoon-rx
   (and (group word (zero-or-more (or word "-")))
        "/"
        (group mold)))
  "Regexp to old style name/mold in declarations.")

(defconst hoon-font-lock-tisfas-rx
  (hoon-rx (and "=/" gap (group wing) (opt "=") (opt (group mold))))
  "Regexp to match =/.")

(defconst hoon-font-lock-bar-mold-rx
  (hoon-rx (group (or "|=" "=|")))
  "Regexp to match |= or =|. Used for syntax highlighting the molds on
lines like |=  [a=@t b=wire].")

(defconst hoon-font-lock-face-mold-rx
  (hoon-rx
   (and (group word (zero-or-more (or word "-")))
        "="
        (group mold)))
  "Regexp to match name=mold in declarations")

(defconst hoon-font-lock-kethep-rx
  (hoon-rx (and "^-  "
                (opt "{")
                (group (or mold) (zero-or-more space (or mold)))
                (opt "}")))
  "Regexp to match ^- in long form. Note the `or' around
  `mold'. We need to wrap the imported stuff in that context.")

(defconst hoon-font-lock-kethep-irregular-rx
  (hoon-rx (and "`" (group mold) "`")))

(defconst hoon-font-lock-kettis-rx
  (hoon-rx (and "^=" gap (group identifier))))

(defconst hoon-font-lock-kettis-irregular-rx
  (hoon-rx (and (group identifier) "="))
  "Regexp of faces.")

(defconst hoon-font-lock-mold-shorthand-rx
  (hoon-rx (and (or "[" "(" line-start space)
                (group (and (and "=" identifier)
                            (zero-or-more (or "." ":" identifier))))))
  "Regexp to match =same-name-as-mold in declarations")

(defconst hoon-font-lock-tis-wing-rx
  (hoon-rx (and (or "=." "=?" "=*") gap (group wing)))
  "Several runes start with <rune> <gap> term/wing. Combine these into one
regexp. Because of =/, this rule must run after the normal mold rule.")

(defconst hoon-font-lock-tisket-rx
  (hoon-rx (and "=^" gap (group-n 1 wing) (opt "=") (opt (group-n 3 mold)) gap (group-n 2 wing))))

(defconst hoon-font-lock-symbols-rx
  (rx (and "%" (or (and word (zero-or-more (any word "-")))
                   "|" "&" "$" ".n" ".y")))
  "Regexp of symbols. This must be run before runes, or %.n and %.y will
 partially be highlighted as runes.")

(defconst hoon-font-lock-runes-rx
  ;; This could be `regexp-opt' and added statically for more speed
  (rx (or
       "$@" "$_" "$:" "$%" "$-" "$^" "$?" "$=" "$|" "$," "$&" "$+"
       "|_" "|:" "|%" "|." "|^" "|-" "|~" "|*" "|=" "|?" "|$"
       ":_" ":^" ":-" ":+" ":~" ":*"
       "%_" "%." "%-" "%*" "%^" "%+" "%~" "%="
       ".^" ".+" ".*" ".=" ".?"
       "^|" "^." "^+" "^-" "^&" "^~" "^=" "^?"
       "~|" "~_" "~%" "~/" "~<" "~>" "~$" "~+" "~&" "~=" "~?" "~!"
       ";:" ";/" ";~" ";;"
       "=|" "=:" "=/" "=;" "=." "=?" "=<" "=-" "=>" "=^" "=+" "=~" "=*" "=,"
       "?|" "?-" "?:" "?." "?^" "?<" "?>" "?+" "?&" "?@" "?~" "?=" "?!"
       "!," "!>" "!;" "!=" "!?" "!^" "!:" "!<"
       "+|"
       ;; Not technically runes, but we highlight them like that.
       "=="
       "--"
       ))
  "Regexp of runes.")

(defconst hoon-font-lock-preprocessor-rx
  (rx (or "/?" "/-" "/+" "//" "/="))
  "Ford preprocessor 'runes'.")

(defconst hoon-font-lock-zapzap-rx
  (rx "!!")
  "Highlight the crash rune in red.")

(defconst hoon-font-lock-numbers-rx
  ;; Numbers are in decimal, binary, hex, base32, or base64, and they must
  ;; contain dots (optionally followed by whitespace), as in the German manner.
  (rx (or
       (and "0w"
            (repeat 1 5 (in "-~0-9a-zA-Z"))
            (zero-or-more "." (repeat 5 (in "-~0-9a-zA-Z"))))
       (and "0v"
            (repeat 1 5 (in "0-9a-v"))
            (zero-or-more "." (repeat 5 (in "0-9a-v"))))
       (and "0b"
            (repeat 1 4 (in "0-1"))
            (zero-or-more "." (repeat 4 (in "0-1"))))
       (and "0x"
            (repeat 1 4 hex)
            (zero-or-more "." (repeat 4 hex)))
       (and (repeat 1 3 digit)
            (zero-or-more "." (repeat 3 digit)))
       ))
  "Regexp of numbers")

(defconst hoon-font-lock-todos-rx
  (rx (or "XX" "XXX" "TODO" "FIXME"))
  "Regexp of todo notes.")

;; This is a start, but we still occasionally miss some complex mold declarations.
(defvar hoon-font-lock-keywords
  `(
    (,hoon-font-lock-arm-declarations-rx ;; "++  arm"
     (1 font-lock-constant-face)
     (2 font-lock-function-name-face))
    (,hoon-font-lock-face-mold-old-rx    ;; name/mold
     (1 font-lock-variable-name-face)
     (2 font-lock-type-face))
    (,hoon-font-lock-bar-mold-rx         ;; (=| |=)  name=mold
     (1 font-lock-constant-face)
     (,hoon-font-lock-face-mold-rx
      nil
      nil
      (1 font-lock-variable-name-face)
      (2 font-lock-type-face)))
    (,hoon-font-lock-mold-shorthand-rx   ;; =same-name-as-mold
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-kethep-rx           ;; ^-  mold
     (1 font-lock-type-face))
    (,hoon-font-lock-kethep-irregular-rx ;; `mold`
     (1 font-lock-type-face))
    (,hoon-font-lock-kettis-rx           ;; ^=  face
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-kettis-irregular-rx ;; face=
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-tisfas-rx           ;; =/  wing=@t
     (1 font-lock-variable-name-face
     (2 font-lock-type-face nil t)))
    (,hoon-font-lock-tis-wing-rx         ;; (=. =?)  wing
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-tisket-rx           ;; =^  wing=@t  wing
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-type-face nil t))

    (,hoon-font-lock-symbols-rx . font-lock-keyword-face)

    ;; Highlights all other runes in other contexts.
    (,hoon-font-lock-runes-rx . font-lock-constant-face)
    (,hoon-font-lock-preprocessor-rx . font-lock-preprocessor-face)
    (,hoon-font-lock-zapzap-rx . font-lock-warning-face)

    ;; Highlight any auras in any other contexts. This must happen after all
    ;; the above because it would otherwise stop the previous rules' execution.
    ;; TODO: This rule causes false positives, highlighting ^ in contexts where
    ;; it's used to reach up one namespace instead of being a mold.
    ("\\(@\\w*\\)\\|\\^" . font-lock-type-face)

    ;; These highlights don't have any issues.
    (,hoon-font-lock-numbers-rx . font-lock-constant-face)
    (,hoon-font-lock-todos-rx . font-lock-warning-face))
  "Keyword highlighting specification for `hoon-mode'.")
(setq hoon-imenu-generic-expression `(("Arms" ,(rx-to-string 'arm) 2)
                                     ("Section" "^::    *[+]+\\([^
]+\\)" 1)))
(defvar hoon-outline-regexp ,(rx-to-string 'arm-comment))

;;;###autoload
(define-derived-mode hoon-mode prog-mode "Hoon"
  "A major mode for editing Hoon files."
  :syntax-table hoon-mode-syntax-table
  (set (make-local-variable 'comment-start) "::")
  (set (make-local-variable 'comment-padding) 2)
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 56)   ;; zero based columns
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(::+\\)\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(hoon-font-lock-keywords))
  (set (make-local-variable 'indent-tabs-mode) nil) ;; tabs zutiefst verboten
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  (set (make-local-variable 'imenu-generic-expression)
       hoon-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) hoon-outline-regexp)

  ;; Hoon files shouldn't have empty lines, but emacs expects them for
  ;; navigation. Treat lines which are just `comment-start' at any margin as
  ;; blank lines for paragraph navigation purposes.
  (set (make-local-variable 'paragraph-start) "\\([ \t]*\:\:\\)*[ \t\f]*$")

  ;; Hoon files often have the same file name in different
  ;; directories. Previously, this was manually handled by hoon-mode instead of
  ;; just setting the right variables and letting Emacs handle it.
  (set (make-local-variable 'uniquify-buffer-name-style) 'forward)
  (set (make-local-variable 'uniquify-strip-common-suffix) nil))

(defun hoon-fill-paragraph (&optional justify)
  "Only fill inside comments. (It might be neat to auto-convert short to long
form syntax, but that would take parsing.)"
  (interactive "P")
  (or (fill-comment-paragraph justify)
      ;; Never return nil; `fill-paragraph' will perform its default behavior
      ;; if we do.
      t))

;;; Indentation

(defun hoon-indent-line ()
  "Indent current line of Hoon code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (hoon-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun hoon-calculate-indentation ()
  "Return the column to which the current line should be indented."
  0) ;;TODO

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hoon$" . hoon-mode))

(defgroup hoon nil
  "hoon mode for emacs"
  :prefix "hoon-"
  :group 'tools)

(defcustom hoon-lsp-enable nil
  "Enable hoon-language-server support. NOTE: requires lsp-mode and hoon-language-server to be installed"
  :group 'hoon
  :type 'boolean)

(defcustom hoon-lsp-port "8080"
  "Port for language server"
  :group 'hoon
  :type 'string)

(defcustom hoon-lsp-delay "0"
  "Delay for language server"
  :group 'hoon
  :type 'string)

(eval-after-load "lsp-mode"
  (if hoon-lsp-enable
    '(progn
      (add-to-list 'lsp-language-id-configuration '(hoon-mode . "hoon"))
      (lsp-register-client
        (make-lsp-client :new-connection
                        (lsp-stdio-connection `("hoon-language-server"
                                                ,(concat "-p " hoon-lsp-port)
                                                ,(concat "-d " hoon-lsp-delay)))
                         :major-modes '(hoon-mode)
                         :server-id 'hoon-ls))
      (add-hook 'hoon-mode-hook #'lsp))
  '()))

(defcustom hoon-herb-path "herb"
  "Path to herb"
  :group 'hoon
  :type 'string)

(defcustom hoon-herb-args "-d"
  "args for herb"
  :group 'hoon
  :type 'string)

(defun hoon-eval-region-in-herb ()
  (interactive)
  (shell-command
   (concat hoon-herb-path " " hoon-herb-args " "
	   (shell-quote-argument (buffer-substring (region-beginning) (region-end)))
	   " &")))

(defun hoon-eval-buffer-in-herb ()
  (interactive)
  (shell-command
   (concat hoon-herb-path " " hoon-herb-args " "
	   (shell-quote-argument (buffer-substring-no-properties (point-min) (point-max)))
	   " &")))

(define-key hoon-mode-map (kbd "C-c r") 'hoon-eval-region-in-herb)
(define-key hoon-mode-map (kbd "C-c e") 'hoon-eval-in-herb)
(define-key hoon-mode-map (kbd "C-c b") 'hoon-eval-buffer-in-herb)


(defvar hoon-docs-dir "/home/jake/vault/projects/urbit/git/urbit.org/content/docs/hoon"
  "Location of hoon docs.")
(defvar hoon-source-dir "/home/jake/vault/projects/urbit/git/urbit"
  "Location of hoon docs.")

(defcustom hoon-docs-rune-dir (concat hoon-docs-dir "/"
                                      "reference/rune/")
  "Location of hoon rune docs.")


(defun hoon-goto-symbol ()
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


(defun hoon--current-rune ()
  (let ((start (point))
         (end (+ (point) 2)))
    (set-mark start)
    (goto-char end)
    (buffer-substring start end)))

(defun hoon--current-aura ()
  (when (looking-at "@")
    (let ((start (point))
          (end (progn  (forward-word) (point))))
      (set-mark start)
      (goto-char end)
      (buffer-substring start end))))

(defun hoon--current-fnsym ()
  (let ((sym (thing-at-point 'symbol)))
    (when sym
        (progn
          (set-mark (point))
          (forward-thing 'symbol)
          sym)
      )
    ))

(define-key hoon-mode-map (kbd "M-.") 'hoon-goto-symbol)
(define-key hoon-mode-map (kbd "M-n") 'outline-next-visible-heading)
(define-key hoon-mode-map (kbd "M-p") 'outline-previous-visible-heading)

(defun hoon-eval-in-herb (expression)
  (interactive
   (list (read-from-minibuffer "Hoon: "
                               (when (region-active-p)
                                 (buffer-substring (region-beginning) (region-end))))))
  (shell-command
   (concat hoon-herb-path " " hoon-herb-args " "
	   (shell-quote-argument expression)
	   " &")))

(provide 'hoon-mode)
;;; hoon-mode.el ends here
