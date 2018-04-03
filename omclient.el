;; omclient.el -- Summary:
;;  Gets automation config data from Ops Manager API and displays it in
;;  a new buffer.
;;
;;; Commentary:
;;
;; For use with MongoDB Ops Manager.
;;

;;; Code:

(defvar endpoint-automation-config "/api/public/v1.0/groups/")


(defun kubectl-read-json-path (resource jsonpath)
  "Return for a RESOURCE data from JSONPATH."
  (shell-command-to-string (concat "kubectl get " resource " -o=jsonpath='{" jsonpath "}'")))


(defun read-om-config-from-map ()
  "Set om configuration from config-map."
  (let ((config-map ()))
    (setf (alist-get 'base-url config-map)
          (kubectl-read-json-path "configmap ops-manager-config" ".data.BASE_URL"))
    (setf (alist-get 'group-id config-map)
          (kubectl-read-json-path "configmap ops-manager-config" ".data.GROUP_ID"))
    (setf (alist-get 'public-api-key config-map)
          (kubectl-read-json-path "configmap ops-manager-config" ".data.PUBLIC_API_KEY"))
    (setf (alist-get 'user-login config-map)
          (kubectl-read-json-path "configmap ops-manager-config" ".data.USER_LOGIN"))
    config-map))

(defun build-curl-command (url username password)
  "Builds curl command get/digest URL USERNAME PASSWORD."
  (concat "curl -s " url " --user '" username ":" password "' --digest --header 'Accept: application/json'"))


(defun om-automation-config ()
  "Call curl command to get automation config GROUP-ID USERNAME PUBLIC-API."
  (interactive)
  (let* ((config-map (read-om-config-from-map))
         (url (concat (alist-get 'base-url config-map)
                      endpoint-automation-config
                      (alist-get 'group-id config-map)
                      "/automationConfig"))
         (curl-command (build-curl-command url
                                           (alist-get 'user-login config-map)
                                           (alist-get 'public-api-key config-map)))
         (doc (json-reformat-from-string (shell-command-to-string curl-command))))
    (with-output-to-temp-buffer "*OM Response*"
      (princ doc))))


(provide 'omclient)
;;; omclient.el ends here
