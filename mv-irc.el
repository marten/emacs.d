(require 'erc-auto)

;;; Finally, connect to the networks.
(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.mhil.net"     :port 6667
         :nick "marten_work"        :full-name "Marten Veldthuis")
    (erc :server "irc.rgoc.rug.nl"  :port 6667
         :nick "marten"        :full-name "Marten Veldthuis")
    ;(erc :server "irc.freenode.net" :port 6667
    ;     :nick "marten"             :full-name "Marten Veldthuis")
    ))

(defun my-erc-refill (column)
  "Fill the ERC messages in the current buffer to COLUMN.
    Using a very high number will undo any previous filling.
    See also `erc-fill-prefix'."
  (interactive "nFill at which column? ")
  (let ((erc-fill-column column))
    ;; `erc-fill' fills the whole buffer, no need to set region
    (erc-fill)))

(provide 'mv-irc)
