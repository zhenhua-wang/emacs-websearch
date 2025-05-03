;;; emacs-websearch.el --- emacs websearch  -*- lexical-binding: t; -*-

;; Package-Requires: (request)

(require 'cl-lib)
(require 'json)
(require 'browse-url)
(require 'request)

(defgroup emacs-websearch nil
  "emacs-websearch"
  :group 'convenience)

(defcustom emacs-websearch-engine 'duckduckgo
  "search engine for emacs-websearch"
  :type '(choice
          (const google)
          (const duckduckgo))
  :group 'emacs-websearch)

(defcustom emacs-websearch-async t
  "Non-nil means searching asynchronously. Currently, this option is only for consult/vertico."
  :group 'emacs-websearch)

(defun emacs-websearch-suggest ()
  (pcase emacs-websearch-engine
    ('google "http://suggestqueries.google.com/complete/search")
    ('duckduckgo "https://duckduckgo.com/ac/")))

(defun emacs-websearch-link ()
  (pcase emacs-websearch-engine
    ('google "https://www.google.com/search?q=%s")
    ('duckduckgo "https://duckduckgo.com/?t=h_&q=%s&ia=web")))

(defvar emacs-websearch--async-stop-p nil)

(defvar emacs-websearch--minibuffer-content nil)

(defvar emacs-websearch--result nil)

(defun emacs-websearch-async-available-p ()
  (and emacs-websearch-async
       (boundp 'vertico--input)
       (functionp 'vertico--exhibit)))

(defun emacs-websearch-async-update-timer ()
  (when (emacs-websearch-async-available-p)
    (run-with-timer
     0.3 0.3
     (lambda ()
       (let* ((current-minibuffer-contents (substring-no-properties (minibuffer-contents)))
              (same-contents-p (string= current-minibuffer-contents
                                        emacs-websearch--minibuffer-content)))
         (when (and (length> current-minibuffer-contents 0)
                    (not (and same-contents-p emacs-websearch--async-stop-p)))
           ;; update display
           (when vertico--input
             (setq vertico--input t)
             (vertico--exhibit))
           (setq emacs-websearch--async-stop-p (if same-contents-p t nil)
                 emacs-websearch--minibuffer-content current-minibuffer-contents)))))))

(defun emacs-websearch-parse-suggests (suggests)
  (pcase emacs-websearch-engine
    ('google (mapcar #'identity (aref suggests 1)))
    ('duckduckgo (mapcar #'cdar suggests))))

(defun emacs-websearch-builder (input)
  (unless (string-empty-p input)
    (request
      (emacs-websearch-suggest)
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
  "Search things on web search engine."
  (interactive)
  (setq emacs-websearch--result nil)
  (let* ((completion-ignore-case t)
         (search-timer (emacs-websearch-async-update-timer))
         (result (unwind-protect
                     (completing-read
                      (format-prompt (format "Search on %s" emacs-websearch-engine)
                                     (emacs-websearch-default-term))
                      (completion-table-dynamic #'emacs-websearch-builder)
                      nil nil nil nil (emacs-websearch-default-term))
                   ;; reset
                   (when search-timer (cancel-timer search-timer))
                   (setq emacs-websearch--minibuffer-content nil))))
    (browse-url (format (emacs-websearch-link) result))))

(provide 'emacs-websearch)

;;; emacs-websearch.el ends here
