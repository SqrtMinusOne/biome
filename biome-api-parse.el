;;; biome-api-parse.el --- Parsing API data for Open Meteo  -*- lexical-binding: t -*-

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

;;  Parsing logic for the Open Meteo API.  This is meant to be used
;;  just for the maintainence purposes.
;;
;; `biome-api-parse--generate' is main the entrypoint.

;;; Code:
(require 'browse-url)
(require 'dom)
(require 'cl-lib)
(require 'seq)

(defvar biome-api-parse--data nil
  "Data parsed from the API docs.")

(defconst biome-api-parse--urls
  '(((:name . "Weather Forecast")
     (:url . "https://open-meteo.com/en/docs")
     (:description . "Seamless integration of high-resolution weather models with up 16 days forecast")
     (:key . "ww"))
    ((:name . "DWD ICON")
     (:url . "https://open-meteo.com/en/docs/dwd-api")
     (:description . "German Weather Service ICON model. 15-minutely data for Central Europe")
     (:key . "wd"))
    ((:name . "NOAA GFS & HRRR")
     (:url . "https://open-meteo.com/en/docs/gfs-api")
     (:description . "Forecasts tailored for the US region")
     (:key . "wu"))
    ((:name . "MeteoFrance")
     (:url . "https://open-meteo.com/en/docs/meteofrance-api")
     (:description . "Forecasts tailored for Central Europe and France")
     (:key . "wf"))
    ((:name . "ECMWF")
     (:url . "https://open-meteo.com/en/docs/ecmwf-api")
     (:description . "Open-data forecasts by ECMWF")
     (:key . "we"))
    ((:name . "JMA")
     (:url . "https://open-meteo.com/en/docs/jma-api")
     (:description . "Forecasts tailored for Japan")
     (:key . "wj"))
    ((:name . "MET Norway")
     (:url . "https://open-meteo.com/en/docs/metno-api")
     (:description . "Forecasts exclusively for North Europe")
     (:key . "wn"))
    ((:name . "GEM")
     (:url . "https://open-meteo.com/en/docs/gem-api")
     (:description . "Forecasts tailored for North America")
     (:key . "wa"))
    ((:name . "Historical Weather")
     (:url . "https://open-meteo.com/en/docs/historical-weather-api")
     (:description . "Weather information since 1940")
     (:key . "h"))
    ((:name . "Ensemble Models")
     (:url . "https://open-meteo.com/en/docs/ensemble-api")
     (:description . "Weather information since 1940")
     (:key . "e"))
    ((:name . "Climate Change")
     (:url . "https://open-meteo.com/en/docs/climate-api")
     (:description . "Climate change projections")
     (:key . "c"))
    ((:name . "Marine Forecast")
     (:url . "https://open-meteo.com/en/docs/marine-weather-api")
     (:description . "Wave forecasts")
     (:key . "m"))
    ((:name . "Air Quality")
     (:url . "https://open-meteo.com/en/docs/air-quality-api")
     (:description . "Pollutants and pollen forcast")
     (:key . "a"))
    ((:name . "Flood")
     (:url . "https://open-meteo.com/en/docs/flood-api")
     (:description . "River discharge forecast")
     (:key . "f")))
  "URLs with open-meteo API docs.")

(defconst biome-api-parse--section-mapping
  '(("Daily Weather Variables" . "daily")
    ("Hourly Weather Variables" . "hourly")
    ("15-Minutely Weather Variables" . "minutely_15")
    ("3-Hourly Weather Variables" . "hourly")
    ("Hourly Marine Variables" . "hourly")
    ("Daily Marine Variables" . "daily")
    ("Hourly Air Quality Variables" . "hourly")
    ("Weather models" . "models")
    ("Flood Models" . "models")
    ("Reanalysis models" . "models")
    ("Ensemble Models" . "models")
    ("Climate models" . "models")
    ("Flood models" . "models"))
  "Mapping from section names to API query param names.")

(defconst biome-api-parse--exclude-ids
  '("select_city" "current_weather" "localhost" "_hourly")
  "Exclude these IDs from parsing.")

(defconst biome-api-parse--float-ids
  '("latitude" "longitude" "elevation")
  "Parse these IDs as floats.")

(defconst biome-api-parse--replace-variables
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

(defconst biome-api-parse--add-settings
  `(((:param . ("elevation" . ((:name . "Elevation")
                               (:type . float))))
     (:pages . ("Weather Forecast" "DWD ICON" "NOAA GFS & HRRR"
                "MeteoFrance" "ECMWF" "JMA" "MET Norway" "GEM" "Historical Weather"
                "Ensemble Models")))))

(defun biome-api-parse--fix-string (string)
  "Remove extra spaces and newlines from STRING."
  (string-trim
   (replace-regexp-in-string
    (rx (+ (or "\n" "\t" " " " " " ")))
    " "
    string)))

(defun biome-api-parse--page-variables (section)
  "Parse variables from SECTION.

SECTION is a DOM element.  Return a list of fields as defined by
`biome-api-parse--page'."
  (let ((elements (dom-search section (lambda (el) (not (stringp el)))))
        fields field-names field-id-mapping)
    (cl-loop for elem in elements
             for dom-id = (seq-some
                           (lambda (v) (unless (string-empty-p v) v))
                           (list (dom-attr elem 'id)
                                 (dom-attr elem 'name)))
             for id = (seq-some
                       (lambda (v) (unless (string-empty-p v) v))
                       (list (let ((val (dom-attr elem 'value)))
                               (unless (member val '("true" "false")) val))
                             dom-id))
             if (and (member (dom-tag elem) '(input select))
                     (not (equal id dom-id)))
             do (push (cons dom-id id) field-id-mapping)
             if (member id biome-api-parse--exclude-ids)
             do (null nil)              ; how to do nothing? :D
             else if (member id biome-api-parse--float-ids)
             do (push (cons id '((:type . float))) fields)
             else if (eq (dom-tag elem) 'input)
             do (push
                 (cons id `((:type . ,(intern (dom-attr elem 'type)))))
                 fields)
             else if (eq (dom-tag elem) 'select)
             do (push
                 (cons id
                       `((:type . select)
                         (:options . ,(mapcar
                                       (lambda (opt) (cons (dom-attr opt 'value)
                                                           (biome-api-parse--fix-string
                                                            (dom-texts opt))))
                                       (dom-by-tag elem 'option)))))
                 fields)
             else if (eq (dom-tag elem) 'label)
             do (push (cons (or (cdr (assoc (dom-attr elem 'for) field-id-mapping))
                                (dom-attr elem 'for))
                            (biome-api-parse--fix-string (dom-texts elem)))
                      field-names))
    (cl-loop for (id . name) in field-names
             do (when-let ((field (assoc id fields)))
                  (setf (cdr field) (cons `(:name . ,name) (cdr field)))))
    (cl-loop for (id . replace-field) in biome-api-parse--replace-variables
             do (when-let ((field (assoc id fields)))
                  (setf (cdr field) (copy-tree replace-field))))
    (nreverse fields)))

(defun biome-api-parse--page-pills (section)
  "Parse bootstrap pills from SECTION.

Currently open-meteo uses it for the \"Pressure Level Variables\"
section in the hourly accordion.

Return a list of sections as defined by `biome-api-parse--page'."
  (when-let* ((pill-names
               (mapcar
                (lambda (button)
                  (cons (dom-attr button 'aria-controls)
                        (biome-api-parse--fix-string (dom-text button))))
                (dom-search
                 section
                 (lambda (el)
                   (and (eq (dom-tag el) 'button)
                        (string-match-p "nav-link" (dom-attr el 'class))))))))
    (cl-loop for (id . name) in pill-names
             collect `((:name . ,name)
                       (:fields
                        . ,(biome-api-parse--page-variables
                            ;; XXX dom-by-id doesn't work here
                            (car (dom-search
                                  section
                                  (lambda (el)
                                    (string= (dom-attr el 'id) id))))))))))

(defun biome-api-parse--page-accordion (section)
  "Parse bootstrap accordion from SECTION.

Return a list of sections as defined by `biome-api-parse--page'."
  (when-let* ((accordion (car (dom-by-class section "accordion")))
              (items (dom-by-class accordion "accordion-item")))
    (let (res)
      (cl-loop
       for item in items
       for item-name = (biome-api-parse--fix-string
                        (dom-text (car (dom-by-class item "accordion-button"))))
       if (dom-by-class item "nav-pills") do
       (push `((:name . ,item-name)
               (:children . ,(biome-api-parse--page-pills item)))
             res)
       else do (push `((:name . ,item-name)
                       (:fields . ,(biome-api-parse--page-variables item)))
                     res))
      (nreverse res))))

(defun biome-api-parse--section (section)
  "Parse one open-meteo SECTION.

The return value is as defined by `biome-api-parse--page'."
  (or (when-let ((accordion (biome-api-parse--page-accordion section)))
        `((:children . ,accordion)))
      (when-let ((variables (biome-api-parse--page-variables section)))
        `((:fields . ,variables)))))

(defun biome-api-parse--postprocess-extract-section (sections section-name &optional remove)
  "Extract section with SECTION-NAME from SECTIONS.

SECTIONS is a list of sections as defined by `biome-api-parse--page'.
SECTION-NAME is a regexp.

The return value is a cons cell with the sections list as car and the
extracted section as cdr.  If REMOVE is non-nil, the extracted section
is removed from the sections list."
  (let ((models
         (seq-find
          (lambda (item)
            (string-match-p
             section-name
             (downcase (cdr (assoc :name item)))))
          sections)))
    (if models
        (when remove
          (setq sections (delete models sections)))
      (setq sections
            (mapcar (lambda (item)
                      (when (assoc :children item)
                        (let ((extracted (biome-api-parse--postprocess-extract-section
                                          (cdr (assoc :children item))
                                          section-name remove)))
                          (when remove
                            (setf (cdr (assoc :children item)) (car extracted)) )
                          (when (cdr extracted)
                            (setq models (cdr extracted)))))
                      item)
                    sections)))
    (cons sections models)))

(defun biome-api-parse--postprocess (sections name)
  "Postprocess the result of `biome-api-parse--page'.

SECTIONS is a list of sections as defined by `biome-api-parse--page',
with the exception that the root section list is a alist of the form
\(<name> . <section-def>).  This function changes that.

NAME is the page name as given in `biome-api-parse--urls'."
  ;; Replace (<name> . <section-def>) with ((:name . <name>)
  ;; ...<section-def>)
  (setq sections (cl-loop for (name . data) in sections
                          collect `((:name . ,name)
                                    ,@data)))
  ;; Extract the model section from the hourly accordion and add it to
  ;; the root section.
  (when-let ((models-data (biome-api-parse--postprocess-extract-section
                           sections "models" t)))
    (setq sections (append (car models-data)
                           (list (cdr models-data)))))
  ;; Add settings
  (when-let ((settings-data (biome-api-parse--postprocess-extract-section
                             sections "settings")))
    (cl-loop for var in biome-api-parse--add-settings
             if (member name (alist-get :pages var))
             do (push (copy-tree (alist-get :param var))
                      (alist-get :fields (cdr settings-data))))
    ;; Fix forecast_days for Flood API
    (when (equal name "Flood")
      (let ((forecast-days (alist-get "forecast_days"
                                      (alist-get :fields (cdr settings-data))
                                      nil nil #'equal)))
        (setf (alist-get :max forecast-days) 210))))
  ;; Add section-specific URL params
  ;; XXX I do not know why this doesn't work without returning
  ;; sections from the loop
  (setq sections
        (cl-loop for section in sections
                 for name = (alist-get :name section)
                 do (when-let ((val (cdr (assoc name biome-api-parse--section-mapping))))
                      (setf (alist-get :param section) val))
                 collect section))
  (setq sections (seq-filter #'identity sections))
  ;; Sort sections
  (setq sections
        (seq-sort-by
         (lambda (e)
           (let ((name (downcase (alist-get :name e))))
             (cond
              ((string-match-p "coordinates" name) 1)
              ((string-match-p "variables" name) 2)
              ((string-match-p "settings" name) 3)
              ((string-match-p "models" name) 4)
              (t 2.5))))
         #'<
         sections))
  sections)

(defun biome-api-parse--page (html-string name)
  "Parse HTML-STRING from open-meteo API docs.

NAME is the page name as given in `biome-api-parse--urls'.

Return the list of sections, where the section is an alist with the
following attributes:
- `:name' - section name
- `:fields' - list of fields
- `:children' - list of child sections
- `:param' - if non-nil, the section is a variable section.

A field is a cons cell with the field ID as car and an alist with the
fields attributes as cdr:
- `:name' - display name of the field
- `:type' - field type.  The following types are available:
- number - integer field
- checkbox - boolean field
- select - select field
- float - float field
- date - date field
- `:min', `:max' - minimum and maximum values for number fields
- `options' - alist of options for select fields"
  (let* ((html (with-temp-buffer
                 (insert html-string)
                 (libxml-parse-html-region (point-min) (point-max))))
         (form (car (dom-by-tag html 'form)))
         (sections
          (cl-loop with section-name = nil
                   with res = nil
                   for section in (dom-non-text-children form)
                   do (let ((h2s (dom-by-tag section 'h2)))
                        (when (and h2s (not (string-empty-p (dom-text (car h2s)))))
                          (setq section-name
                                (biome-api-parse--fix-string
                                 (dom-text (car h2s))))))
                   if (string= section-name "Usage License") return (nreverse res)
                   ;; if (string= section-name "Hourly Weather Variables") return (nreverse res)
                   ;; Merge different sections with the same name
                   unless (assoc section-name res)
                   do (push `(,section-name . nil) res)
                   do (let ((parsed-section (biome-api-parse--section section)))
                        (cl-loop
                         for (kind . data) in parsed-section
                         unless (assoc kind (cdr (assoc section-name res)))
                         do (push `(,kind . nil) (cdr (assoc section-name res)))
                         do (setf (cdr (assoc kind (cdr (assoc section-name res))))
                                  (append data (cdr (assoc kind (cdr (assoc section-name res))))))))
                   finally return (nreverse res))))
    (biome-api-parse--postprocess sections name)))

(defun biome-api-parse--datum (datum)
  "Parse one page from open-meteo API docs.

DATUM is an element of `biome-api-parse--urls'."
  (browse-url (alist-get :url datum))
  (let* ((html (read-string "Enter HTML string: "))
         (parsed (biome-api-parse--page html (alist-get :name datum))))
    (setf (alist-get (alist-get :name datum)
                     biome-api-parse--data nil nil #'equal)
          (append (copy-tree datum)
                  (list (cons :sections parsed))))))

(defun biome-api-parse--timezones ()
  "Return a list of timezones from the tzdata package."
  (thread-last
    "find $TZDIR -type f | cut -c $(($(echo -n \"$TZDIR\" | wc -c) + 2))- | grep -Eo '[A-Z].*'"
    shell-command-to-string
    split-string
    (seq-filter (lambda (x) (not (string-empty-p x))))
    seq-uniq
    (seq-sort #'string-lessp)))

(defun biome-api-parse--generate ()
  "Generate `biome-api-data' and `biome-api-timezones' constants.

This function does two things:
- Parses the HTML pages from `biome-api-parse--urls'
- Generates the list of timezones from the tzdata package

Unfortunately, the HTML pages have accordions that are dynamically
loaded, so we need to manually load them in the browser, expand the
accordions and copy the HTML source.

The function prints the generated constants to a new buffer.  Save
them to biome-api-data.el."
  (interactive)
  (setq biome-api-parse--data nil)
  (let ((timezones (biome-api-parse--timezones)))
    (cl-loop for datum in biome-api-parse--urls
             do (biome-api-parse--datum datum))
    (let ((buffer (generate-new-buffer "*biome-generated*")))
      (with-current-buffer buffer
        (emacs-lisp-mode)
        (insert (pp-to-string
                 `(defconst biome-api-data
                    ',(reverse biome-api-parse--data)
                    ;; ',biome-api-data
                    "Open-meteo API docs data."))
                "\n\n"
                (pp-to-string
                 `(defconst biome-api-timezones
                    ',timezones
                    "List of timezones from the tzdata package.")))
        (goto-char (point-min))
        (switch-to-buffer buffer)))))

(provide 'biome-api-parse)
;;; biome-api-parse.el ends here
