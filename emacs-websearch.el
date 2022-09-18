(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'browse-url)
(require 'request)

(defgroup emacs-websearch nil
  "emacs-websearch"
  :group 'convenience)

(defcustom emacs-websearch-engine 'google
  "search engine for emacs-websearch"
  :type '(choice
          (const google))
  :group 'emacs-websearch)

(defvar emacs-websearch-suggest (pcase emacs-websearch-engine
                                  ('google "http://suggestqueries.google.com/complete/search")))

(defvar emacs-websearch-link (pcase emacs-websearch-engine
                               ('google "https://www.google.com/search?q=%s")))

(defun emacs-websearch-parse-suggests (suggests)
  (pcase 'emacs-websearch-engine
    (google (mapcar #'identity (aref data 1)))))

;; (defun emacs-websearch (input)
;;   (interactive
;;    (let ((input-default (thing-at-point 'symbol)))
;;      (list (read-string (format-prompt (format "Search on %s" emacs-websearch-engine)
;;                                        input-default)
;;                         nil nil input-default))))
;;   (if (not (string-empty-p input))
;;       (request
;;         emacs-websearch-suggest
;;         :type "GET"
;;         :params (list
;;                  (cons "client" "firefox")
;;                  (cons "q" input))
;;         :parser 'json-read
;;         :success (cl-function
;;                   (lambda (&key data &allow-other-keys)
;;                     (let ((result (completing-read (format "Search on %s " emacs-websearch-engine)
;;                                                    (emacs-websearch-parse-suggests data))))
;;                       (browse-url (format emacs-websearch-link result))))))))

(defun emacs-websearch-builder (input &rest pred type)
  (if (not (string-empty-p input))
      (let ((result))
        (request
          emacs-websearch-suggest
          :type "GET"
          :params (list
                   (cons "client" "firefox")
                   (cons "q" input))
          :parser 'json-read
          :sync t
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (setq result (emacs-websearch-parse-suggests data)))))
        result)))

(defun emacs-websearch (result)
  (interactive
   (list (completing-read (format "Search on %s: " emacs-websearch-engine)
                          (completion-table-dynamic #'emacs-websearch-builder))))
  (browse-url (format emacs-websearch-link result)))
