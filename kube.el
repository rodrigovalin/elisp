;;; kube.el --- Summary:
;;
;; Some helper functions for kubectl and yaml files.
;;
;;; Commentary:
;;
;; This is a simple module I created when working with Kubernetes.
;; It allows you to apply or delete a buffer or region quickly without
;; having to visit the command line (which is probably on another
;; screen). I found it useful for quickly changing your Kubernetes
;; resources while editing them.
;;
;; Author: Rodrigo Valin <licorna@gmail.com>
;;
;;; Code:


(defun kube--command-on-region (command)
  "Run COMMAND on region."
  (shell-command-on-region (region-beginning) (region-end) command))

(defun kube-delete ()
  "Run kubectl delete -f <this-buffer-filename>."
  (interactive)
  (let ((filename (buffer-file-name)))
    (save-buffer)
    (message (shell-command-to-string (concat "kubectl delete -f " filename)))))

(defun kube-delete-region ()
  "Run kubectl delete -f on region."
  (interactive)
  (kube--command-on-region "kubectl delete -f -"))

(defun kube-replace ()
  "Run kubectl replace -f <this-buffer-filename>."
  (interactive)
  (let ((filename (buffer-file-name)))
    (save-buffer)
    (message (shell-command-to-string (concat "kubectl replace -f " filename)))))

(defun kube-replace-region ()
  "Run kubectl replace -f on region."
  (interactive)
  (kube--command-on-region "kubectl replace -f -"))


(defun kube-apply ()
  "Apply this file, creating or updating a given Kubernetes Resource."
  (interactive)
  (let ((filename (buffer-file-name)))
    (save-buffer)
    (message (shell-command-to-string (concat "kubectl apply -f " filename)))))

(defun kube-apply-region ()
  "Run kubectl apply -f on region."
  (interactive)
  (kube--command-on-region "kubectl apply -f -"))

(defun kube-dash ()
  "Open the minikube dashboard."
  (interactive)
  (shell-command-to-string "minikube dashboard"))

(defun yaml-next-field ()
  "Jump to next yaml field."
  (interactive)
  (search-forward-regexp ": +"))

(defun yaml-prev-field()
  "Jump to previous yaml field."
  (interactive)
  (search-backward-regexp ": +"))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)
             (define-key yaml-mode-map "\M-\r" 'insert-ts)
             (define-key yaml-mode-map (kbd "C-<tab>") 'yaml-next-field)
             (define-key yaml-mode-map (kbd "C-S-<tab>") 'yaml-prev-field)
             ))

(provide 'kube)
;;; kube.el ends here
