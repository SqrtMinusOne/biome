;;; biome-api.el --- API for Open Meteo  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Korytov Pavel

;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Homepage: https://github.com/SqrtMinusOne/biome

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Make requests to Open Meteo API.

;;; Code:
(require 'json)
(require 'request)

(defconst biome-api--default-urls
  '(("Weather Forecast" . "https://api.open-meteo.com/v1/forecast")
    ("DWD ICON (Germany)" . "https://api.open-meteo.com/v1/dwd-icon")
    ("NOAA GFS & HRRR (U.S.)" . "https://api.open-meteo.com/v1/gfs")
    ("MeteoFrance" . "https://api.open-meteo.com/v1/meteofrance")
    ("ECMWF" . "https://api.open-meteo.com/v1/ecmwf")
    ("JMA (Japan)" . "https://api.open-meteo.com/v1/jma")
    ("MET (Norway)" . "https://api.open-meteo.com/v1/metno")
    ("GEM (Canada)" . "https://api.open-meteo.com/v1/gem")
    ("BOM (Australia)" . "https://api.open-meteo.com/v1/bom")
    ("CMA (China)" . "https://api.open-meteo.com/v1/cma")
    ("Historical Weather" . "https://archive-api.open-meteo.com/v1/archive")
    ("Historical Weather (on this day)"
     . "https://archive-api.open-meteo.com/v1/archive")
    ("Ensemble Models" . "https://ensemble-api.open-meteo.com/v1/ensemble")
    ("Previous Runs API" . "https://previous-runs-api.open-meteo.com/v1/forecast")
    ("Climate Change" . "https://climate-api.open-meteo.com/v1/climate")
    ("Marine Forecast" . "https://marine-api.open-meteo.com/v1/marine")
    ("Air Quality" . "https://air-quality-api.open-meteo.com/v1/air-quality")
    ("Flood" . "https://flood-api.open-meteo.com/v1/flood"))
  "Default URLs for Open Meteo API.")

(defcustom biome-api-try-parse-error-as-response nil
  "Try to parse error responses as normal ones.

This is a workaround for my termux setup, somehow request' gives
the parsed response to the error callback there."
  :group 'biome
  :type 'boolean)

(defcustom biome-api-urls biome-api--default-urls
  "URLs of the Open Meteo API."
  :type `(alist
          :key-type string
          :value-type string
          :options ,(mapcar #'car biome-api--default-urls))
  :group 'biome)

(defun biome-api--get-params (query)
  "Convert QUERY to a list of parameters for `request'.

QUERY is a form as defined by `biome-query-current'."
  (mapcar (lambda (item)
            (cond
             ((stringp item) (cons item "true"))
             ((member (car item) '("end_date" "start_date"))
              (cons (car item)
                    (format-time-string "%F" (cdr item))))
             ((listp item)
              (cons
               (car item)
               (cond
                ((listp (cdr item)) (mapconcat #'identity (reverse (cdr item)) ","))
                (t (cdr item)))))))
          (alist-get :params query)))

(defun biome-error-quit ()
  "Close a window and kill its buffer."
  (interactive)
  (quit-window t))

(defvar biome-api-error-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'biome-error-quit)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'biome-error-quit))
    keymap)
  "Keymap for `biome-api-error-mode'.")

(define-derived-mode biome-api-error-mode text-mode "Biome Error"
  "Major mode for viewing open meteo API errors.

\\{biome-api-error-mode-map}"
  (read-only-mode 1))

(defun biome-api--show-error (error-thrown response &optional err)
  "Show ERROR-THROWN and RESPONSE in a buffer."
  (let ((buffer (generate-new-buffer "*biome-api-error*")))
    (with-current-buffer buffer
      (insert
       "Open Meteo has returned an error.\n"
       (propertize "Error: " 'face 'font-lock-warning-face)
       (prin1-to-string error-thrown)
       "\n")
      (when err
        (insert
         (propertize "Pasing error: " 'face 'font-lock-warning-face)
         (error-message-string err)
         "\n"))
      (condition-case nil
          (insert (propertize "Reason: " 'face 'font-lock-warning-face)
                  (alist-get 'reason (request-response-data response))
                  "\n")
        (error
         (insert "Can't parse reason. Raw response: \n"
                 (prin1-to-string response))))
      (biome-api-error-mode))
    (switch-to-buffer buffer)))

(defun biome-api-get (query callback)
  "Get data from Open Meteo API.

QUERY is a form as defined by `biome-query-current'.  CALLBACK is
called with QUERY and the data returned by the API as arguments."
  (let ((url (alist-get (alist-get :name query)
                        biome-api-urls nil nil #'equal)))
    (request url
      :type "GET"
      :params (biome-api--get-params query)
      :parser #'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback (copy-tree query) data)))
      :error
      (cl-function
       (lambda (&key error-thrown response &allow-other-keys)
         (if biome-api-try-parse-error-as-response
             (condition-case err
                 (funcall callback (copy-tree query) (request-response-data response))
               (error (biome-api--show-error error-thrown response err)))))))))

(defun biome-api-get-multiple (queries callback)
  "Get data from Open Meteo API.

QUERIES is a list of forms as defined by `biome-query-current'.  CALLBACK is
called with QUERIES and the data returned by the API as arguments."
  (let (requests)
    (seq-map-indexed
     (lambda (query idx)
       (push
        (request (alist-get (alist-get :name query)
                            biome-api-urls nil nil #'equal)
          :type "GET"
          :params (biome-api--get-params query)
          :parser #'json-read
          :success (cl-function
                    (lambda (&rest)
                      ;; I'm not sure why, but `request-response-done-p' for
                      ;; the current request returns nil.  I don't
                      ;; know how stable this is, so...
                      (let ((completed-count
                             (cl-loop for i from 0
                                      for request in requests
                                      if (or (request-response-done-p request)
                                             (= i (- (length requests) 1 idx)))
                                      sum 1)))
                        (message "Completed %d/%d requests"
                                 completed-count (length queries))
                        (when (eq (length queries) completed-count)
                          (funcall callback (copy-tree queries)
                                   (mapcar #'request-response-data
                                           (reverse requests)))))))
          :error
          (cl-function (lambda (&key error-thrown response &allow-other-keys)
                         (biome-api--show-error error-thrown response))))
        requests))
     queries)))

(provide 'biome-api)
;;; biome-api.el ends here
