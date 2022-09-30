(require 'lsp-mode)

(defgroup hoon-lsp nil
  "Hoon LSP mode for Emacs."
  :prefix "hoon-lsp-"
  :group 'hoon)

(defcustom hoon-lsp-enable nil
  "Enable hoon-language-server support.
NOTE: requires lsp-mode and hoon-language-server to be installed"
  :group 'hoon
  :type 'boolean)

(defcustom hoon-lsp-port "8080"
  "Port for language server."
  :group 'hoon
  :type 'string)

(defcustom hoon-lsp-delay "0"
  "Delay for language server."
  :group 'hoon
  :type 'string)

(defun hoon-lsp-bootstrap ()
  (add-to-list 'lsp-language-id-configuration '(hoon-mode . "hoon")))

(defun hoon-lsp-make-client ()
  (make-lsp-client :new-connection
                   (lsp-stdio-connection `("hoon-language-server"
                                           ,(concat "-p " hoon-lsp-port)
                                           ,(concat "-d " hoon-lsp-delay)))
                   :major-modes '(hoon-mode)
                   :server-id 'hoon-ls))

(defun hoon-lsp-register-client ()
  (lsp-register-client (hoon-lsp-make-client)))

(with-eval-after-load 'hoon-mode
  (progn
    (hoon-lsp-bootstrap)
    (hoon-lsp-register-client)
    (add-hook 'hoon-mode-hook #'lsp)))

(provide 'hoon-lsp)
