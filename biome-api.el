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

;;  TODO

;;; Code:
(require 'request)
(require 'cl-lib)
(require 'seq)

;; Parsing

(defconst biome-api--parse-exclude-ids
  '("select_city" "current_weather" "localhost")
  "Exclude these IDs from parsing.")

(defconst biome-api--replace-variables
  `(("past_days" . ((:name . "Past days")
                    (:type . number)
                    (:min . 0)
                    (:max . 92)))
    ("forecast_days" . ((:name . "Forecast days")
                        (:type . number)
                        (:min . 0)
                        (:max . 16)))
    ("start_date" . ((:name . "Start date")
                     (:type . date)))
    ("end_date" . ((:name . "End date")
                   (:type . date)))
    ("timezone" . ((:name . "Timezone")
                   (:type . timezone))))
  "Replace these variable defintions with the given ones.")

(defun biome-api--fix-string (string)
  "Remove extra spaces and newlines from STRING."
  (string-trim
   (replace-regexp-in-string
    (rx (+ (or "\n" "\t" " " " " " ")))
    " "
    string)))

(defun biome-api--parse-page-variables (section)
  "Parse variables from SECTION.

SECTION is a DOM element."
  (let ((elements (dom-search section (lambda (el) (not (stringp el)))))
        fields field-names)
    (cl-loop for elem in elements
             if (member (dom-attr elem 'id) biome-api--parse-exclude-ids)
             do (null nil)              ; how to do nothing? :D
             else if (eq (dom-tag elem) 'input)
             do (push
                 (cons (dom-attr elem 'id)
                       `((:type . ,(dom-attr elem 'type))))
                 fields)
             else if (eq (dom-tag elem) 'select)
             do (push
                 (cons (dom-attr elem 'id)
                       `((:type . select)
                         (:options . ,(mapcar
                                       (lambda (opt) (cons (dom-attr opt 'value)
                                                           (biome-api--fix-string
                                                            (dom-text opt))))
                                       (dom-by-tag elem 'option)))))
                 fields)
             else if (eq (dom-tag elem) 'label)
             do (push (cons (dom-attr elem 'for)
                            (biome-api--fix-string (dom-text elem)))
                      field-names))
    (cl-loop for (id . name) in field-names
             do (when-let ((field (assoc id fields)))
                  (setf (cdr field) (cons `(:name . ,name) (cdr field)))))
    (cl-loop for (id . replace-field) in biome-api--replace-variables
             do (when-let ((field (assoc id fields)))
                  (setf (cdr field) (copy-tree (cdr replace-field)))))
    (nreverse fields)))

(defun biome-api--parse-page-pills (section)
  (when-let* ((pill-names
               (mapcar
                (lambda (button)
                  (cons (dom-attr button 'aria-controls)
                        (biome-api--fix-string (dom-text button))))
                (dom-search
                 section
                 (lambda (el)
                   (and (eq (dom-tag el) 'button)
                        (string-match-p "nav-link" (dom-attr el 'class))))))))
    (cl-loop for (id . name) in pill-names
             collect `((:name . ,name)
                       (:fields
                        . ,(biome-api--parse-page-variables
                            ;; XXX dom-by-id doesn't work here
                            (car (dom-search
                                  section
                                  (lambda (el)
                                    (string= (dom-attr el 'id) id))))))))))

(defun biome-api--parse-page-accordion (section)
  (when-let* ((accordion (car (dom-by-class section "accordion")))
              (items (dom-by-class accordion "accordion-item")))
    (let (res)
      (cl-loop
       for item in items
       for item-name = (biome-api--fix-string
                        (dom-text (car (dom-by-class item "accordion-button"))))
       if (dom-by-class item "nav-pills") do
       (push `((:name . ,item-name)
               (:children . ,(biome-api--parse-page-pills item)))
             res)
       else do (push `((:name . ,item-name)
                       (:fields . ,(biome-api--parse-page-variables item)))
                     res))
      (nreverse res))))

(defun biome-api--parse-section (section)
  (or (when-let ((accordion (biome-api--parse-page-accordion section)))
        (list (cons :children accordion)))
      (when-let ((variables (biome-api--parse-page-variables section)))
        (list (cons :fields variables)))))

(defun biome-api--parse-postprocess-extract-models (res)
  (let ((models
         (seq-find
          (lambda (item)
            (string-match-p
             "model" (downcase (cdr (assoc :name item)))))
          res)))
    (if models
        (setq res (delete models res))
      (setq res
            (mapcar (lambda (item)
                      (when (assoc :children item)
                        (let ((extracted (biome-api--parse-postprocess-extract-models
                                          (cadr (assoc :children item)))))
                          (setf (cadr (assoc :children item)) (car extracted))
                          (when (cdr extracted)
                            (setq models (cdr extracted)))))
                      item)
                    res)))
    (cons res models)))

(defun biome-api--parse-postprocess (res)
  (setq res (cl-loop for (name . data) in res
                     collect `((:name . ,name)
                               ,@data)))
  (when-let ((models-data (biome-api--parse-postprocess-extract-models res)))
    (setq res (append (car models-data)
                      (list (cdr models-data)))))
  res)

(defun biome-api--parse-page (html-string)
  (let* ((html (with-temp-buffer
                 (insert html-string)
                 (libxml-parse-html-region (point-min) (point-max))))
         (form (dom-by-id html "api_form"))
         (endpoint (dom-attr form 'action))
         (tree
          (cl-loop with section-name = nil
                   with res = nil
                   for section in (dom-non-text-children form)
                   do (let ((h2s (dom-by-tag section 'h2)))
                        (when (and h2s (not (string-empty-p (dom-text (car h2s)))))
                          (setq section-name
                                (biome-api--fix-string
                                 (dom-text (car h2s))))))
                   if (string= section-name "Usage License") return (nreverse res)
                   ;; Merge different sections with the same name
                   unless (assoc section-name res)
                   do (push `(,section-name . nil) res)
                   do (let ((parsed-section (biome-api--parse-section section)))
                        (cl-loop for (kind . data) in parsed-section
                                 unless (assoc kind (cdr (assoc section-name res)))
                                 do (push `(,kind . nil) (cdr (assoc section-name res)))
                                 do (setf (cdr (assoc kind (cdr (assoc section-name res))))
                                          (cons data (cdr (assoc kind (cdr (assoc section-name res))))))))
                   finally return (nreverse res))))
    (biome-api--parse-postprocess tree)))

;; (setq my/test2 (biome-api--parse-page my/test))

;; (request "https://open-meteo.com/en/docs"
;;   :parser 'buffer-string
;;   :success (cl-function
;;             (lambda (&key data &allow-other-keys)
;;               (setq my/test data))))


(provide 'biome-api)
;;; biome-api.el ends here
