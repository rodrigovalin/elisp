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


(defun kubectl-read-json-path (resource jsonpath &optional namespace )
  "Return for a RESOURCE data from JSONPATH.  A NAMESPACE can be provided."
  (shell-command-to-string (concat "kubectl get -n mongodb " resource " -o=jsonpath='{" jsonpath "}'")))

(defvar config-map-name "global-om-config")
(defvar namespace "mongodb")

(defun read-om-config-from-map ()
  "Set om configuration from config-map."
  (let ((config-map ())
        (config-map-str (concat "configmap " config-map-name)))
    (setf (alist-get 'base-url config-map)
          (kubectl-read-json-path config-map-str ".data.BASE_URL" namespace))
    (setf (alist-get 'group-id config-map)
          (kubectl-read-json-path config-map-str ".data.GROUP_ID" namespace))
    (setf (alist-get 'public-api-key config-map)
          (kubectl-read-json-path config-map-str ".data.PUBLIC_API_KEY" namespace))
    (setf (alist-get 'user-login config-map)
          (kubectl-read-json-path config-map-str ".data.USER_LOGIN" namespace))
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
         )
    (with-output-to-temp-buffer "*OM Response*" (princ curl-command))))

    ;;      (doc (shell-command-to-string curl-command)))
    ;; (with-output-to-temp-buffer "*OM Response*"
    ;;   (princ doc))))

; (om-automation-config)

(defun om-delete-monitoring (host)
  "Stop monitoring of HOST."
  (let* ((config-map (read-om-config-from-map))
         (url (concat (alist-get 'base-url config-map)
                      endpoint-automation-config
                      (alist-get 'group-id config-map)
                      "/hosts/"
                      host))
         (curl-command (build-curl-command url
                                           (alist-get 'user-login config-map)
                                           (alist-get 'public-api-key config-map))))
    curl-command))

(om-delete-monitoring "host1")

(provide 'omclient)
;;; omclient.el ends here
