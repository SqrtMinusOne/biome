;;; biome-api.el --- API for Open Meteo  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

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

;; TODO

;;; Code:
(require 'json)
(require 'request)

(defconst biome-api--default-urls
  '(("Weather Forecast" . "https://api.open-meteo.com/v1/forecast")
    ("DWD ICON" . "https://api.open-meteo.com/v1/dwd-icon")
    ("NOAA GFS & HRRR" . "https://api.open-meteo.com/v1/gfs")
    ("MeteoFrance" . "https://api.open-meteo.com/v1/meteofrance")
    ("ECMWF" . "https://api.open-meteo.com/v1/ecmwf")
    ("JMA" . "https://api.open-meteo.com/v1/jma")
    ("MET Norway" . "https://api.open-meteo.com/v1/metno")
    ("GEM" . "https://api.open-meteo.com/v1/gem")
    ("Historical Weather" . "https://archive-api.open-meteo.com/v1/archive")
    ("Ensemble Models" . "https://ensemble-api.open-meteo.com/v1/ensemble")
    ("Climate Change" . "https://climate-api.open-meteo.com/v1/climate")
    ("Marine Forecast" . "https://marine-api.open-meteo.com/v1/marine")
    ("Air Quality" . "https://air-quality-api.open-meteo.com/v1/air-quality")
    ("Flood" . "https://flood-api.open-meteo.com/v1/flood"))
  "Default URLs for Open Meteo API.")

(defcustom biome-api-urls biome-api--default-urls
  "URL of the Open Meteo API."
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
                    (format-time-string "%Y-%m-%d" (cdr item))))
             ((listp item)
              (cons
               (car item)
               (cond
                ((listp (cdr item)) (mapconcat #'identity (cdr item) ","))
                (t (cdr item)))))))
          (alist-get :params query)))

(defun biome-error-quit ()
  "Close a window and kill its buffer."
  (interactive)
  (quit-window t))

(defvar open-meteo-error-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'biome-error-quit)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'biome-error-quit))
    keymap)
  "Keymap for `open-meteo-error-mode'.")

(define-derived-mode open-meteo-error-mode text-mode "Lyrics view"
  "Major mode for viewing open meteo errors.

\\{open-meteo-error-mode-map}"
  (read-only-mode 1))

(defun biome-api--show-error (error-thrown response)
  "Show ERROR-THROWN and RESPONSE in a buffer."
  (let ((buffer (generate-new-buffer "*biome-api-error*")))
    (with-current-buffer buffer
      (insert
       (concat "Open Meteo has returned an error.\n"
               (propertize "Error: " 'face 'font-lock-warning-face)
               (prin1-to-string error-thrown)
               "\n"))
      (condition-case nil
          (insert (concat (propertize "Reason: " 'face 'font-lock-warning-face)
                          (alist-get 'reason (request-response-data response)))
                  "\n")
        (insert "Can't parse reason. Raw response: ")
        (insert (prin1-to-string response)))
      (open-meteo-error-mode))
    (switch-to-buffer buffer)))

(defun biome-api-get (query callback)
  "Get data from Open Meteo API.

QUERY is a form as defined by `biome-query-current'.  CALLBACK is
called with the data as its only argument."
  (let ((url (alist-get (alist-get :name query)
                        biome-api-urls nil nil #'equal)))
    (request url
      :type "GET"
      :params (biome-api--get-params query)
      :parser #'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error
      (cl-function (lambda (&key error-thrown response &allow-other-keys)
                     (biome-api--show-error error-thrown response))))))

(provide 'biome-api)
;;; biome-api.el ends here
