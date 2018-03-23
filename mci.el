;;; package --- summary

;;; Commentary:
;;
;; Emacs MCI module
;;
;; Author: Rodrigo Valin
;;

;;; code:


(defun mci-get-hosts ()
  "Gets mci list of hosts from mci."
  (mapcar 'json-read-from-string (butlast (split-string (shell-command-to-string "mci plain") "\n"))))


(defun host-name-from-host (host)
  "Return a string repr of HOST."
  (concat (alist-get 'host host) ":" (alist-get 'id host) ":" (alist-get 'status host)))


(defun format-candidate-list (host)
  "Builds a line for HOST to be displayed in results."
  (require 'helm-grep)
  (let* ((split (helm-grep-split-line (host-name-from-host host)))
         (h (car split))
         (id (nth 1 split))
         (status (nth 2 split)))
    (cons (concat (propertize host 'face 'italics)
                  (propertize ":" 'invisible t)
                  (propertize id 'face 'italics)
                  (propertize ":" 'invisible t)
                  (propertize status 'face 'bold))
          host)))

(defun build-helm-candidates (hosts)
  "Builds an alist of mci HOSTS."
  (let (ahosts)
    (dolist (host hosts)
      (add-to-list 'ahosts (cons (host-name-from-host host) host)))
    ahosts))


(defun mci-host-from-id (hosts id)
  "Return a host from HOSTS with a given ID."
  (let (host)
    (dolist (source hosts)
      (if (string= id (alist-get 'id source)) (setq host source)))
    host))


(defun mci-browse (host)
  "Open browser at HOST."
  (browse-url (concat "http://" (alist-get 'host host) ":8080")))


(defun mci-kill (host)
  "Kill the HOST."
  (message (shell-command-to-string (concat "mci kill " (alist-get 'id host)))))


(defun mci-distro (host)
  "Show distro for HOST."
  (message (alist-get 'distro host)))


(defun mci-ssh (host)
  "Ssh connect to HOST."
  (my-ssh (alist-get 'user host) (alist-get 'host host) "22"))


(defun my-ssh (user host port)
  "Connect to a remote host by ssh at USER@HOST:PORT."
  (interactive "sUser: \nsHost: \nsPort (default 22): ")
  (let* ((port (if (equal port "") "22" port))
         (switches (list host "-l" user "-p" port)))
    (set-buffer (apply 'make-term "ssh" "ssh" nil switches))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer "*ssh*")))


(defun mci-distro-spawn (distro)
  "Create a new host for DISTRO."
  (message (shell-command-to-string (concat "mci spawn " distro))))


(defun mci-spawn ()
  "Spawn a new host in mci."
  (interactive)
  (let* ((distros (butlast (split-string (shell-command-to-string "mci distros --plain") "\n"))))
    (helm :sources (helm-build-sync-source "MCI Distros"
                     :candidates distros
                     :fuzzy-match t
                     :action '(("Spawn" . mci-distro-spawn)))
          :buffer "*helm mci:distros*")))


(defun mci-list ()
  "List of mci hosts."
  (interactive)
  (let* ((hosts (build-helm-candidates (mci-get-hosts))))
    (helm :sources (helm-build-sync-source "MCI List"
                     :candidates hosts
                     :fuzzy-match t
                     :persistent-action 'mci-browse
                     :persistent-help "Open MMS instance in default browser."
                     :action
                     '(("Browse" . mci-browse)
                       ("Show distro" . mci-distro)
                       ("Ssh into" . mci-ssh)
                       ("Kill instance" . mci-kill)))
          :buffer "*helm MCI*")))

(provide 'mci)
;;; mci.el ends here
