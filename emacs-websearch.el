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

(defcustom emacs-websearch-async t
  "Non-nil means searching asynchronously. Currently, this option is only for consult/vertico."
  :group 'emacs-websearch)

(defvar emacs-websearch-suggest (pcase emacs-websearch-engine
                                  ('google "http://suggestqueries.google.com/complete/search")))

(defvar emacs-websearch-link (pcase emacs-websearch-engine
                               ('google "https://www.google.com/search?q=%s")))

(defvar emacs-websearch--result nil)

(defun emacs-websearch-async-available-p ()
  (and emacs-websearch-async
       (functionp 'vertico--exhibit)))

(defun emacs-websearch-parse-suggests (suggests)
  (pcase 'emacs-websearch-engine
    (google (mapcar #'identity (aref suggests 1)))))

(defun emacs-websearch-builder (input)
  (when (not (string-empty-p input))
    (request
      emacs-websearch-suggest
      :type "GET"
      :params (list
               (cons "client" "firefox")
               (cons "q" input))
      :sync (if (emacs-websearch-async-available-p) nil t)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq emacs-websearch--result
                        (emacs-websearch-parse-suggests data)))))
    emacs-websearch--result))

(defun emacs-websearch-default-term ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun emacs-websearch ()
  (interactive)
  (setq emacs-websearch--result nil)
  (let* ((update-timer (when (emacs-websearch-async-available-p)
                         (run-with-timer
                          0.3 0.3
                          (lambda ()
                            (emacs-websearch-builder (minibuffer-contents))
                            (when vertico--input
                              (setq vertico--input t)
                              (vertico--exhibit))))))
         (result (unwind-protect
                     (completing-read
                      (format-prompt (format "Search on %s" emacs-websearch-engine)
                                     (emacs-websearch-default-term))
                      (completion-table-dynamic #'emacs-websearch-builder)
                      nil nil nil nil (emacs-websearch-default-term))
                   (when update-timer (cancel-timer update-timer)))))
    (browse-url (format emacs-websearch-link result))))

(provide 'emacs-websearch)

;;; emacs-websearch.el ends here
