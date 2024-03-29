;;; emacs-websearch.el --- emacs websearch  -*- lexical-binding: t; -*-

;; Package-Requires: (request)

(require 'cl-lib)
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
    (google (mapcar #'identity (aref suggests 1)))))

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

(defun emacs-websearch-default-term ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun emacs-websearch (result)
  (interactive
   (list (completing-read (format-prompt (format "Search on %s" emacs-websearch-engine)
                                         (emacs-websearch-default-term))
                          (completion-table-dynamic #'emacs-websearch-builder)
                          nil nil nil nil (emacs-websearch-default-term))))
  (browse-url (format emacs-websearch-link result)))

(provide 'emacs-websearch)

;;; emacs-websearch.el ends here
