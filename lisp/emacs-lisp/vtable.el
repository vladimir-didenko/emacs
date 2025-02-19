;;; vtable.el --- Displaying data in tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'text-property-search)
(require 'mule-util)

(cl-defstruct vtable-column
  "A vtable column."
  name
  width
  min-width
  max-width
  primary
  align
  getter
  formatter
  displayer
  -numerical)

(defclass vtable ()
  ((columns :initarg :columns :accessor vtable-columns)
   (objects :initarg :objects :accessor vtable-objects)
   (objects-function :initarg :objects-function
                     :accessor vtable-objects-function)
   (getter :initarg :getter :accessor vtable-getter)
   (formatter :initarg :formatter :accessor vtable-formatter)
   (displayer :initarg :displayer :accessor vtable-displayer)
   (use-header-line :initarg :use-header-line
                    :accessor vtable-use-header-line)
   (face :initarg :face :accessor vtable-face)
   (actions :initarg :actions :accessor vtable-actions)
   (keymap :initarg :keymap :accessor vtable-keymap)
   (separator-width :initarg :separator-width :accessor vtable-separator-width)
   (sort-by :initarg :sort-by :accessor vtable-sort-by)
   (ellipsis :initarg :ellipsis :accessor vtable-ellipsis)
   (-cache :initform (make-hash-table :test #'equal)))
  "A object to hold the data for a table.")

(defvar-keymap vtable-map
  "S" #'vtable-sort-by-current-column
  "{" #'vtable-narrow-current-column
  "}" #'vtable-widen-current-column
  "g" #'vtable-revert-command
  "M-<left>" #'vtable-previous-column
  "M-<right>" #'vtable-next-column)

(defvar-keymap vtable-header-line-map
  :parent vtable-map
  "<follow-link>" 'mouse-face
  "<mouse-2>" #'vtable-header-line-sort)

(cl-defun make-vtable (&key columns objects objects-function
                            getter
                            formatter
                            displayer
                            (use-header-line t)
                            (face 'variable-pitch)
                            actions keymap
                            (separator-width 1)
                            sort-by
                            (ellipsis t)
                            (insert t))
  "Create and insert a vtable at point.
The vtable object is returned.  If INSERT is nil, the table won't
be inserted."
  (when objects-function
    (setq objects (funcall objects-function)))
  ;; Auto-generate the columns.
  (unless columns
    (unless objects
      (error "Can't auto-generate columns; no objects"))
    (setf columns (make-list (length (car objects)) "")))
  (setq columns (mapcar (lambda (column)
                          (cond
                           ;; We just have the name (as a string).
                           ((stringp column)
                            (make-vtable-column :name column))
                           ;; A plist of keywords/values.
                           ((listp column)
                            (apply #'make-vtable-column column))
                           ;; A full `vtable-column' object.
                           (t
                            column)))
                        columns))
  ;; We'll be altering the list, so create a copy.
  (setq objects (copy-sequence objects))
  (let ((table
         (make-instance 'vtable
                        :columns columns
                        :objects objects
                        :objects-function objects-function
                        :getter getter
                        :formatter formatter
                        :displayer displayer
                        :use-header-line use-header-line
                        :face face
                        :actions actions
                        :keymap keymap
                        :separator-width separator-width
                        :sort-by sort-by
                        :ellipsis ellipsis)))
    ;; Compute missing column data.
    (setf (vtable-columns table) (vtable--compute-columns table))
    (unless sort-by
      (seq-do-indexed (lambda (column index)
                        (when (vtable-column-primary column)
                          (push (cons index (vtable-column-primary column))
                                (vtable-sort-by table))))
                      (vtable-columns table)))
    (when insert
      (vtable-insert table))
    table))

;;; Interface utility functions.

(defun vtable-current-table ()
  "Return the table under point."
  (get-text-property (point) 'vtable))

(defun vtable-current-object ()
  "Return the object under point."
  (get-text-property (point) 'vtable-object))

(defun vtable-current-column ()
  "Return the index of the column under point."
  (get-text-property (point) 'vtable-column))

(defun vtable-beginning-of-table ()
  "Go to the start of the current table."
  (if (text-property-search-backward 'vtable (vtable-current-table))
      (point)
    (goto-char (point-min))))

(defun vtable-end-of-table ()
  "Go to the end of the current table."
  (if (text-property-search-forward 'vtable (vtable-current-table))
      (point)
    (goto-char (point-max))))

(defun vtable-goto-object (object)
  "Go to OBJECT in the current table.
Return the position of the object if found, and nil if not."
  (let ((start (point)))
    (vtable-beginning-of-table)
    (save-restriction
      (narrow-to-region (point) (vtable-end-of-table))
      (if (text-property-search-forward 'vtable-object object #'eq)
          (progn
            (forward-line -1)
            (point))
        (goto-char start)
        nil))))

(defun vtable-goto-table (table)
  "Go to TABLE in the current buffer.
If TABLE is found, return the position of the start of the table.
If it can't be found, return nil and don't move point."
  (let ((start (point)))
    (goto-char (point-min))
    (if-let ((match (text-property-search-forward 'vtable table t)))
        (goto-char (prop-match-beginning match))
      (goto-char start)
      nil)))

(defun vtable-goto-column (column)
  "Go to COLUMN on the current line."
  (beginning-of-line)
  (if-let ((match (text-property-search-forward 'vtable-column column t)))
      (goto-char (prop-match-beginning match))
    (end-of-line)))

(defun vtable-update-object (table object old-object)
  "Replace OLD-OBJECT in TABLE with OBJECT."
  (let* ((objects (vtable-objects table))
         (inhibit-read-only t))
    ;; First replace the object in the object storage.
    (if (eq old-object (car objects))
        ;; It's at the head, so replace it there.
        (setf (vtable-objects table)
              (cons object (cdr objects)))
      ;; Otherwise splice into the list.
      (while (and (cdr objects)
                  (not (eq (cadr objects) old-object)))
        (setq objects (cdr objects)))
      (unless objects
        (error "Can't find the old object"))
      (setcar (cdr objects) object))
    ;; Then update the cache...
    (let ((line (assq old-object (car (vtable--cache table)))))
      (unless line
        (error "Can't find cached object"))
      (setcar line object)
      (setcdr line (vtable--compute-cached-line table object))
      ;; ... and redisplay the line in question.
      (save-excursion
        (vtable-goto-object old-object)
        (let ((keymap (get-text-property (point) 'keymap))
              (start (point)))
          (delete-line)
          (vtable--insert-line table line (nth 1 (vtable--cache table))
                               (vtable--spacer table))
          (add-text-properties start (point) (list 'keymap keymap
                                                   'vtable table))))
      ;; We may have inserted a non-numerical value into a previously
      ;; all-numerical table, so recompute.
      (vtable--recompute-numerical table (cdr line)))))

(defun vtable-remove-object (table object)
  "Remove OBJECT from TABLE.
This will also remove the displayed line."
  ;; First remove from the objects.
  (setf (vtable-objects table) (delq object (vtable-objects table)))
  ;; Then adjust the cache and display.
  (let ((cache (vtable--cache table))
        (inhibit-read-only t))
    (setcar cache (delq (assq object (car cache)) (car cache)))
    (save-excursion
      (vtable-goto-table table)
      (when (vtable-goto-object object)
        (delete-line)))))

(defun vtable-insert-object (table object &optional after-object)
  "Insert OBJECT into TABLE after AFTER-OBJECT.
If AFTER-OBJECT is nil (or doesn't exist in the table), insert
OBJECT at the end.
This also updates the displayed table."
  ;; First insert into the objects.
  (let (pos)
    (if (and after-object
             (setq pos (memq after-object (vtable-objects table))))
        ;; Splice into list.
        (setcdr pos (cons object (cdr pos)))
      ;; Append.
      (nconc (vtable-objects table) (list object))))
  ;; Then adjust the cache and display.
  (save-excursion
    (vtable-goto-table table)
    (let* ((cache (vtable--cache table))
           (inhibit-read-only t)
           (keymap (get-text-property (point) 'keymap))
           (elem (and after-object
                      (assq after-object (car cache))))
           (line (cons object (vtable--compute-cached-line table object))))
      (if (not elem)
          ;; Append.
          (progn
            (setcar cache (nconc (car cache) (list line)))
            (vtable-end-of-table))
        ;; Splice into list.
        (let ((pos (memq elem (car cache))))
          (setcdr pos (cons line (cdr pos)))
          (unless (vtable-goto-object after-object)
            (vtable-end-of-table))))
      (let ((start (point)))
        (vtable--insert-line table line (nth 1 cache) (vtable--spacer table))
        (add-text-properties start (point) (list 'keymap keymap
                                                 'vtable table)))
      ;; We may have inserted a non-numerical value into a previously
      ;; all-numerical table, so recompute.
      (vtable--recompute-numerical table (cdr line)))))

(defun vtable-column (table index)
  "Return the name of the INDEXth column in TABLE."
  (vtable-column-name (elt (vtable-columns table) index)))

;;; Generating the table.

(defun vtable--get-value (object index column table)
  "Compute a cell value."
  (cond
   ((vtable-column-getter column)
    (funcall (vtable-column-getter column)
             object table))
   ((vtable-getter table)
    (funcall (vtable-getter table)
             object index table))
   ;; No getter functions; standard getters.
   ((stringp object)
    object)
   (t
    (elt object index))))

(defun vtable--compute-columns (table)
  (let ((numerical (make-vector (length (vtable-columns table)) t))
        (columns (vtable-columns table)))
    ;; First determine whether there are any all-numerical columns.
    (dolist (object (vtable-objects table))
      (seq-do-indexed
       (lambda (_elem index)
         (unless (numberp (vtable--get-value object index (elt columns index)
                                             table))
           (setf (elt numerical index) nil)))
       (vtable-columns table)))
    ;; Then fill in defaults.
    (seq-map-indexed
     (lambda (column index)
       ;; This is used when displaying.
       (unless (vtable-column-align column)
         (setf (vtable-column-align column)
               (if (elt numerical index)
                   'right
                 'left)))
       ;; This is used for sorting.
       (setf (vtable-column--numerical column)
             (elt numerical index))
       column)
     (vtable-columns table))))

(defun vtable--spacer (table)
  (vtable--compute-width table (vtable-separator-width table)))

(defun vtable-insert (table)
  (let* ((spacer (vtable--spacer table))
         (start (point))
         (ellipsis (if (vtable-ellipsis table)
                       (propertize (truncate-string-ellipsis)
                                   'face (vtable-face table))
                     ""))
         (ellipsis-width (string-pixel-width ellipsis))
         data widths)
    ;; We maintain a cache per screen/window width, so that we render
    ;; correctly if Emacs is open on two different screens (or the
    ;; user resizes the frame).
    (if-let ((cache (vtable--cache table)))
        (setq data (nth 0 cache)
              widths (nth 1 cache))
      (setq data (vtable--compute-cache table)
            widths (vtable--compute-widths table data))
      (setf (gethash (vtable--cache-key) (slot-value table '-cache))
            (list data widths)))
    (if (vtable-use-header-line table)
        (vtable--set-header-line table widths spacer)
      ;; Insert the header line directly into the buffer, and put a
      ;; keymap to be able to sort the columns there (by clicking on
      ;; them).
      (vtable--insert-header-line table widths spacer)
      (add-text-properties start (point)
                           (list 'keymap vtable-header-line-map
                                 'rear-nonsticky t
                                 'vtable table))
      (setq start (point)))
    (vtable--sort table)
    ;; Insert the data.
    (dolist (line (car (vtable--cache table)))
      (vtable--insert-line table line widths spacer
                           ellipsis ellipsis-width))
    (add-text-properties start (point)
                         (list 'keymap (vtable--make-keymap table)
                               'rear-nonsticky t
                               'vtable table))
    (goto-char start)))

(defun vtable--insert-line (table line widths spacer
                                  &optional ellipsis ellipsis-width)
  (let ((start (point))
        (columns (vtable-columns table)))
    (seq-do-indexed
     (lambda (elem index)
       (let ((value (nth 0 elem))
             (column (elt columns index))
             (pre-computed (nth 2 elem)))
         ;; See if we have any formatters here.
         (cond
          ((vtable-column-formatter column)
           (setq value (funcall (vtable-column-formatter column) value)
                 pre-computed nil))
          ((vtable-formatter table)
           (setq value (funcall (vtable-formatter table)
                                value index table)
                 pre-computed nil)))
         (let ((displayed
                ;; Allow any displayers to have their say.
                (cond
                 ((vtable-column-displayer column)
                  (funcall (vtable-column-displayer column)
                           value (elt widths index) table))
                 ((vtable-displayer table)
                  (funcall (vtable-displayer table)
                           value index (elt widths index) table))
                 (pre-computed
                  ;; If we don't have a displayer, use the pre-made
                  ;; (cached) string value.
                  (if (> (nth 1 elem) (elt widths index))
                      (concat
                       (vtable--limit-string
                        pre-computed (- (elt widths index) ellipsis-width))
                       ellipsis)
                    pre-computed))
                 ;; Recompute widths.
                 (t
                  (if (> (string-pixel-width value) (elt widths index))
                      (concat
                       (vtable--limit-string
                        value (- (elt widths index) ellipsis-width))
                       ellipsis)
                    value))))
               (start (point)))
           (if (eq (vtable-column-align column) 'left)
               (insert displayed
                       (propertize
                        " " 'display
                        (list 'space
                              :width (list
                                      (+ (- (elt widths index)
                                            (string-pixel-width displayed))
                                         spacer)))))
             ;; Align to the right.
             (insert (propertize " " 'display
                                 (list 'space
                                       :width (list (- (elt widths index)
                                                       (string-pixel-width
                                                        displayed)))))
                     displayed
                     (propertize " " 'display
                                 (list 'space
                                       :width (list spacer)))))
           (put-text-property start (point) 'vtable-column index))))
     (cdr line))
    (insert "\n")
    (put-text-property start (point) 'vtable-object (car line))))

(defun vtable--cache-key ()
  (cons (frame-terminal) (window-width)))

(defun vtable--cache (table)
  (gethash (vtable--cache-key) (slot-value table '-cache)))

(defun vtable--clear-cache (table)
  (setf (gethash (vtable--cache-key) (slot-value table '-cache)) nil))

(defun vtable--sort (table)
  (pcase-dolist (`(,index . ,direction) (vtable-sort-by table))
    (let ((cache (vtable--cache table))
          (numerical (vtable-column--numerical
                      (elt (vtable-columns table) index))))
      (setcar cache
              (sort (car cache)
                    (lambda (e1 e2)
                      (let ((c1 (elt e1 (1+ index)))
                            (c2 (elt e2 (1+ index))))
                        (if numerical
                            (< (car c1) (car c2))
                          (string< (if (stringp (car c1))
                                       (car c1)
                                     (format "%s" (car c1)))
                                   (if (stringp (car c2))
                                       (car c2)
                                     (format "%s" (car c2)))))))))
      (when (eq direction 'descend)
        (setcar cache (nreverse (car cache)))))))

(defun vtable--indicator (table index)
  (let ((order (car (last (vtable-sort-by table)))))
    (if (eq index (car order))
        ;; We're sorting by this column last, so return an indicator.
        (catch 'found
          (dolist (candidate (nth (if (eq (cdr order) 'ascend)
                                      1
                                    0)
                                  '((?▼ ?v)
                                    (?▲ ?^))))
            (when (char-displayable-p candidate)
              (throw 'found (string candidate)))))
      "")))

(defun vtable--insert-header-line (table widths spacer)
  ;; Insert the header directly into the buffer.
  (let* ((start (point)))
    (seq-do-indexed
     (lambda (column index)
       (let* ((name (propertize
                     (vtable-column-name column)
                     'face (list 'header-line (vtable-face table))))
              (start (point))
              (indicator (vtable--indicator table index))
              (indicator-width (string-pixel-width indicator))
              displayed)
         (insert
          (setq displayed
                (concat
                 (if (> (string-pixel-width name)
                        (- (elt widths index) indicator-width))
                     (vtable--limit-string
                      name (- (elt widths index) indicator-width))
                   name)
                 indicator))
          (propertize " " 'display
                      (list 'space :width
                            (list (+ (- (elt widths index)
                                        (string-pixel-width displayed))
                                     spacer)))))
         (put-text-property start (point) 'vtable-column index)))
     (vtable-columns table))
    (insert "\n")
    (add-face-text-property start (point) 'header-line)))

(defun vtable--recompute-numerical (table line)
  "Recompute numericalness of columns if necessary."
  (let ((columns (vtable-columns table))
        (recompute nil))
    (seq-do-indexed
     (lambda (elem index)
       (when (and (vtable-column--numerical (elt columns index))
                  (not (numberp elem)))
         (setq recompute t)))
     line)
    (when recompute
      (vtable--compute-columns table))))

(defun vtable--set-header-line (table widths spacer)
  (setq header-line-format
        (string-replace
         "%" "%%"
         (with-temp-buffer
           (insert " ")
           (vtable--insert-header-line table widths spacer)
           ;; Align the header with the (possibly) fringed buffer text.
           (put-text-property
            (point-min) (1+ (point-min))
            'display '(space :align-to 0))
           (buffer-substring (point-min) (1- (point-max))))))
  (vtable-header-mode 1))

(defun vtable--limit-string (string pixels)
  (while (and (length> string 0)
              (> (string-pixel-width string) pixels))
    (setq string (substring string 0 (1- (length string)))))
  string)

(defun vtable--char-width (table)
  (string-pixel-width (propertize "x" 'face (vtable-face table))))

(defun vtable--compute-width (table spec)
  (cond
   ((numberp spec)
    (* spec (vtable--char-width table)))
   ((string-match "\\([0-9.]+\\)ex" spec)
    (* (string-to-number (match-string 1 spec)) (vtable--char-width table)))
   ((string-match "\\([0-9.]+\\)px" spec)
    (string-to-number (match-string 1 spec)))
   ((string-match "\\([0-9.]+\\)%" spec)
    (* (string-to-number (match-string 1 spec)) (window-width nil t)))
   (t
    (error "Invalid spec: %s" spec))))

(defun vtable--compute-widths (table cache)
  "Compute the display widths for TABLE."
  (seq-into
   (seq-map-indexed
    (lambda (column index)
      (let ((width
             (or
              ;; Explicit widths.
              (and (vtable-column-width column)
                   (vtable--compute-width table (vtable-column-width column)))
              ;; Compute based on the displayed widths of
              ;; the data.
              (seq-max (seq-map (lambda (elem)
                                  (nth 1 (elt (cdr elem) index)))
                                cache)))))
        ;; Let min-width/max-width specs have their say.
        (when-let ((min-width (and (vtable-column-min-width column)
                                   (vtable--compute-width
                                    table (vtable-column-min-width column)))))
          (setq width (max width min-width)))
        (when-let ((max-width (and (vtable-column-max-width column)
                                   (vtable--compute-width
                                    table (vtable-column-max-width column)))))
          (setq width (min width max-width)))
        width))
    (vtable-columns table))
   'vector))

(defun vtable--compute-cache (table)
  (seq-map
   (lambda (object)
     (cons object (vtable--compute-cached-line table object)))
   (vtable-objects table)))

(defun vtable--compute-cached-line (table object)
  (seq-map-indexed
   (lambda (column index)
     (let* ((value (vtable--get-value object index column table))
            (string (if (stringp value)
                        (copy-sequence value)
                      (format "%s" value))))
       (add-face-text-property 0 (length string)
                               (vtable-face table)
                               t string)
       ;; We stash the computed width and string here -- if there are
       ;; no formatters/displayers, we'll be using the string, and
       ;; then won't have to recreate it.
       (list value (string-pixel-width string) string)))
   (vtable-columns table)))

(defun vtable--make-keymap (table)
  (let ((map (if (or (vtable-actions table)
                     (vtable-keymap table))
                 (copy-keymap vtable-map)
               vtable-map)))
    (when-let ((actions (vtable-actions table)))
      (while actions
        (funcall (lambda (key binding)
                   (keymap-set map key
                               (lambda (object)
                                 (interactive (list (vtable-current-object)))
                                 (funcall binding object))))
                 (car actions) (cadr actions))
        (setq actions (cddr actions))))
    (if (vtable-keymap table)
        (progn
          (setf (vtable-keymap table)
                (copy-keymap (vtable-keymap table)))
          ;; Respect any previously set parent keymaps.
          (set-keymap-parent (vtable-keymap table)
                             (if (keymap-parent (vtable-keymap table))
                                 (append (ensure-list
                                          (vtable-keymap table))
                                         (list map))
                               map))
          (vtable-keymap table))
      map)))

(defun vtable-revert ()
  "Regenerate the table under point."
  (let ((table (vtable-current-table))
        (object (vtable-current-object))
        (column (vtable-current-column))
        (inhibit-read-only t))
    (unless table
      (user-error "No table under point"))
    (delete-region (vtable-beginning-of-table) (vtable-end-of-table))
    (vtable-insert table)
    (when object
      (vtable-goto-object object))
    (when column
      (vtable-goto-column column))))

(defun vtable--widths (table)
  (nth 1 (vtable--cache table)))

;;; Commands.

(defvar-keymap vtable-header-mode-map
  "<header-line> <mouse-1>" 'vtable-header-line-sort
  "<header-line> <mouse-2>" 'vtable-header-line-sort)

(define-minor-mode vtable-header-mode
  "Minor mode for buffers with vtables with headers."
  :keymap vtable-header-mode-map)

(defun vtable-narrow-current-column ()
  "Narrow the current column."
  (interactive)
  (let* ((table (vtable-current-table))
         (column (vtable-current-column))
         (widths (vtable--widths table)))
    (setf (aref widths column)
          (max (* (vtable--char-width table) 2)
               (- (aref widths column) (vtable--char-width table))))
    (vtable-revert)))

(defun vtable-widen-current-column ()
  "Widen the current column."
  (interactive)
  (let* ((table (vtable-current-table))
         (column (vtable-current-column))
         (widths (nth 1 (vtable--cache table))))
    (cl-incf (aref widths column) (vtable--char-width table))
    (vtable-revert)))

(defun vtable-previous-column ()
  "Go to the previous column."
  (interactive)
  (vtable-goto-column
   (max 0 (1- (or (vtable-current-column)
                  (length (vtable--widths (vtable-current-table))))))))

(defun vtable-next-column ()
  "Go to the next column."
  (interactive)
  (when (vtable-current-column)
    (vtable-goto-column
     (min (1- (length (vtable--widths (vtable-current-table))))
          (1+ (vtable-current-column))))))

(defun vtable-revert-command ()
  "Re-query data and regenerate the table under point."
  (interactive)
  (let ((table (vtable-current-table)))
    (when (vtable-objects-function table)
      (setf (vtable-objects table) (funcall (vtable-objects-function table))))
    (vtable--clear-cache table))
  (vtable-revert))

(defun vtable-sort-by-current-column ()
  "Sort the table under point by the column under point."
  (interactive)
  (unless (vtable-current-column)
    (user-error "No current column"))
  (let* ((table (vtable-current-table))
         (last (car (last (vtable-sort-by table))))
         (index (vtable-current-column)))
    ;; First prune any previous appearance of this column.
    (setf (vtable-sort-by table)
          (delq (assq index (vtable-sort-by table))
                (vtable-sort-by table)))
    ;; Then insert this as the last sort key.
    (setf (vtable-sort-by table)
          (append (vtable-sort-by table)
                  (list (cons index
                              (if (eq (car last) index)
                                  (if (eq (cdr last) 'ascend)
                                      'descend
                                    'ascend)
                                'ascend))))))
  (vtable-revert))

(defun vtable-header-line-sort (e)
  "Sort a vtable from the header line."
  (interactive "e")
  (let* ((pos (event-start e))
	 (obj (posn-object pos)))
    (with-current-buffer (window-buffer (posn-window pos))
      (goto-char (point-min))
      (vtable-goto-column
       (get-text-property (if obj (cdr obj) (posn-point pos))
			  'vtable-column
			  (car obj)))
      (vtable-sort-by-current-column))))

(provide 'vtable)

;;; vtable.el ends here
