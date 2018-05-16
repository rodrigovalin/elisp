;;; package --- summary

;;; Commentary:
;;
;; Emacs MCI module
;;
;; Author: Rodrigo Valin
;;
;;
;; List of displayed items (rows):
;;
;; status : host (host-type) : instance-id : distro : creation : expiration

;;; code:


(defun mci-get-hosts ()
  "Gets mci list of hosts from mci."
  (reverse (mapcar 'json-read-from-string (butlast (split-string (shell-command-to-string "mci list --plain") "\n")))))


(defun mci--propertize-status (status)
  "Return a propertized STATUS depending on contents."
  (cond ((string-prefix-p "running" status) (propertize status 'face 'bold))
        ((string-prefix-p "starting" status) (propertize status 'face 'bold))
        (t status)))


(defun /d (date part)
  "Return for a DATE object, a given PART."
  (cond ((equal part 'year) (nth 0 date))
        ((equal part 'month) (nth 1 date))
        ((equal part 'day) (nth 2 date))
        ((equal part 'hour) (nth 3 date))
        ((equal part 'minute) (nth 4 date))
        ((equal part 'second) (nth 5 date))
        (t 0)))


(defun mci--propertize-date (date)
  "Return a propertized DATE."
  (let ((pd (mci--parse-iso8601-datetime date)))
    (format "%d/%d/%d_%d:%d" (/d pd 'year) (/d pd 'month) (/d pd 'day) (/d pd 'hour) (/d pd 'minute))))

(defun mci--parse-iso8601-datetime (dtime)
  "Parse an iso8601 DTIME string.  Return a list of YYYY MM DD HH mm SS."
  (let* ((parted (split-string dtime "T"))
         (date-part (split-string (car parted) "-"))
         (time-part (split-string (nth 1 parted) ":")))
    (mapcar #'string-to-number
            (list (car date-part) (nth 1 date-part) (nth 2 date-part)
                  (car time-part) (nth 1 time-part) (car (split-string (nth 2 time-part) "[^0-9]"))))))


(defun current-date-iso8601 ()
  "Return current date in iso8601 format."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))


(defun current-date ()
  "Return current date in YYYY MM DD HH mm SS list."
  (mci--parse-iso8601-datetime (current-date-iso8601)))


(defun mci--helm-item (host)
  "Return a string repr of HOST."
  (concat
   (mci--propertize-status (format "%-9s" (alist-get 'status host)))
   (if (eq (alist-get 'host host) "")
       "-starting-"
     (propertize (format "%-50s" (alist-get 'host host)) 'face 'italic))
   (format " %-5s " (alist-get 'host_type host))
   (format "%-20s "(alist-get 'id host))
   (format "%-20s "(alist-get 'distro host))
   (mci--propertize-date (alist-get 'creation_time host))
   " -- "
   (mci--propertize-date (alist-get 'expiration_time host))))


(defun build-helm-candidates (hosts)
  "Builds an alist of mci HOSTS."
  (let (ahosts)
    (dolist (host hosts)
      (add-to-list 'ahosts (cons (mci--helm-item host) host)))
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
  (let* ((distros (butlast (split-string (shell-command-to-string "mci distros | awk '{ print $3 }'") "\n"))))
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
