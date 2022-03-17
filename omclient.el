;;; -*- lexical-binding: t; -*-

;; omclient.el -- Summary:
;;  Gets automation config data from Ops Manager API and displays it in
;;  a new buffer.
;;
;;; Commentary:
;;
;; For use with MongoDB Ops Manager.
;;


(require 'with-editor)
(require 'ht)
(require 'url)

;;; Code:

(defvar endpoint-automation-config "/api/public/v1.0/groups/")

(defun mdb/get-mdb-resources ()
  "Return a list of MongoDB resources in the current namespace."
  (remove "" (split-string (shell-command-to-string (concat "kubectl --request-timeout 3s get mdb --no-headers -o name")) "\n")))

(defun mdb/get-om-resources ()
  "Return a list of MongoDB resources in the current namespace."
  (remove "" (split-string (shell-command-to-string (concat "kubectl --request-timeout 3s get om --no-headers -o name")) "\n")))

(defun kube/secret-read-entry (secret-name entry)
  "Read from SECRET-NAME the value of ENTRY."
  (let ((secret-obj (json-parse-string (shell-command-to-string (concat "kubectl get secret " secret-name " -o json")))))
    (base64-decode-string (ht-get* secret-obj "data" entry))))


(defun mdb/get-mdb-credentials (mdb-resource)
  "Return the name of the Secret with credentials for MDB-RESOURCE."
    (ht-get* mdb-resource "spec" "credentials"))


(defun mdb/get-mdb-api-key-from-credentials-secret (credentials-secret)
  "Return private and public key for this resource from CREDENTIALS-SECRET Secret."
  (let ((credentials (ht-create)))
    (ht-set! credentials "privateKey" (kube/secret-read-entry credentials-secret "privateKey"))
    (ht-set! credentials "publicKey" (kube/secret-read-entry credentials-secret "publicKey"))
    credentials))


(defun mdb/-parse-link-into-url-group (link)
  "Return `url' and `group' from LINK."
  (let ((result (ht-create))
        (parsed-url (url-generic-parse-url link)))
    (ht-set! result "group" (car (last (split-string (url-filename parsed-url) "/"))))
    (ht-set! result "url" (concat (url-type parsed-url) "://" (url-host parsed-url) ":" (number-to-string (url-port parsed-url))))
    result))
 

(defun mdb/get-resource-by-name (name)
  "Return a resource by NAME."
  (json-parse-string (shell-command-to-string (concat "kubectl get " name " -o json"))))


(defun mdb/curlo-start-do ()
  "Try to start curlo no matter what."
  (call-process-shell-command "kubectl run --image curlimages/curl curlo --restart Never -- sleep infinity"))


(defun mdb/curlo-start ()
  "Start curlo if it has not been started already."
  (let ((kubectl-get-output (shell-command-to-string "kubectl get pods curlo")))
    (if (string-match-p "Error from server" kubectl-get-output)
        (mdb/curlo-start-do)
      (message "curlo has started already"))))


(defun mdb/run-curl-on-container (curl-command)
  "Run CURL-COMMAND on container and return response."
  (let ((cmd (concat "kubectl exec curlo -- " curl-command)))
    (message (concat "executing: " cmd))
    (let ((curl-output (shell-command-to-string cmd)))
      (json-parse-string curl-output))))

(defvar mdb/test nil)
(defun mdb/-build-automation-config-url (url group basic-auth-username basic-auth-password)
  (if mdb/test
      "curl --silent https://jsonplaceholder.typicode.com/albums"
    (concat "curl --silent -q -u " basic-auth-username ":" basic-auth-password " --digest " url endpoint-automation-config group "/automationConfig")))

(defun kube/config-map-read-entry (configmap name)
  "From CONFIGMAP Read NAME."
  (shell-command-to-string (concat "kubectl get cm " configmap " -o jsonpath='{.data." name "}'")))

(defun om (name)
  "Initialize an Ops Manager object from NAME."
  (mdb/get-resource-by-name (concat "om/" name)))

;;(om "om-basic")


;; (kube/config-map-read-entry "replica-set-with-prom-config" "baseUrl")

;; TODO: maybe remove "versions" from the object before princ on the
;; *automation-config* buffer.

;;;###autoload
(defun mdb/get-automation-config ()
    "Open the automation config for a resource in a new buffer."
    (interactive)
    (let* ((resource (mdb/get-mdb-resources))
           (selection (completing-read-default "Choose MongoDB resource" resource))
           (resource (mdb/get-resource-by-name selection))
           (credentials-secret (mdb/get-mdb-credentials resource))
           (credentials (mdb/get-mdb-api-key-from-credentials-secret credentials-secret))
           (url-group (mdb/-parse-link-into-url-group (kube/config-map-read-entry (ht-get* resource "spec" "opsManager" "configMapRef" "name") "baseUrl")))
           (curl-cmd (mdb/-build-automation-config-url (ht-get url-group "url") (ht-get url-group "group") (ht-get credentials "publicKey") (ht-get credentials "privateKey"))))
      (mdb/curlo-start)
      (message (concat "executing: " curl-cmd))
      (with-output-to-temp-buffer (get-buffer-create "*automation-config*")
        (princ (mdb/run-curl-on-container curl-cmd)))))

(defun mdb/edit ()
  "Choose a MongoDB object and allow to modify it."
  (interactive)
  (let* ((resource (mdb/get-mdb-resources))
         (selection (completing-read-default "Choose MongoDB resource" resource)))
    (with-editor-async-shell-command (concat "kubectl edit " selection))))

(defun om/edit ()
  "Choose a MongoDBOpsManager object and allow to modify it."
  (interactive)
  (let* ((resource (mdb/get-om-resources))
         (selection (completing-read-default "Choose MongoDBOpsManager resource" resource)))
    (with-editor-async-shell-command (concat "kubectl edit " selection))))

(defun operator/rebuild ()
  "Rebuild the operator."
  (interactive)
  (shell-command-to-string (concat "./pipeline.py --include operator-quick"))
  (shell-command-to-string (concat "kubectl scale deploy mongodb-enterprise-operator --replicas 0 && kubectl scale deploy/mongodb-enterprise-operator --replicas 1")))


(provide 'omclient)
;;; omclient.el ends here
