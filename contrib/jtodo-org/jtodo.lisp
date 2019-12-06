;;;; modified from Jacob Reckhard's original jtodo
;;;; license: GPLv3
(defpackage :jtodo
  (:use :cl)
  (:export
    *todo-path
    *default-list
    list-add
    list-remove
    list-toggle-item
    list-toggle
    list-clear-done
    list-count
    file-write-list
    file-read-list
    display-todo-header
    display-todo
    display-all-lists
    display-help))
    
(in-package :jtodo)

(defvar *todo-path "~/.jtodolistfiles/")
(defvar *default-list "Todo")

(defun list-add (todo-list data)
  "Adds an item to the list and returns it"
  (append todo-list (list (list nil data))))

(defun list-remove (todo-list index)
  "Removes the item at the index and returns it"
  (labels ((inner-remove (l i)
                         (cond ((= 0 i) (cdr l))
                               ((not l) l)
                               (t (cons (car l) (inner-remove (cdr l) (1- i)))))))
    (inner-remove todo-list index)))

(defun list-toggle-item (item)
  "Returns the item but toggled"
  (cons (not (car item)) (cdr item)))

(defun list-toggle (todo-list index)
  "Toggles the item at the index and returns it"
  (if (< index 1) (return-from list-toggle todo-list))
  (labels ((inner-toggle (l i)
                         (cond ((= 0 i) (cons (list-toggle-item (car l)) (cdr l)))
                               ((not l) l)
                               (t (cons (car l) (inner-toggle (cdr l) (1- i)))))))
    (inner-toggle todo-list index)))

(defun list-clear-done (todo-list)
  "Removes all items that are done from the list"
  (cond ((stringp (car todo-list))
         (cons (car todo-list) (list-clear-done (cdr todo-list))))
        ((not todo-list) todo-list)
        ((car (car todo-list)) (list-clear-done (cdr todo-list)))
        (t (cons (car todo-list) (list-clear-done (cdr todo-list))))))

(defun list-count (todo-list)
  "Counts how many items are in a list"
  (format t "~a~%" (length todo-list))
  todo-list)


(defun file-write-list (todo-list)
  "Writes the todo list to a file"
  (if (not todo-list) (return-from file-write-list nil))
  (let ((name (concatenate 'string *todo-path (car todo-list))))
    (with-open-file (f name :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (print todo-list f))
    (if (not (cdr todo-list))
      (delete-file (probe-file name)))))

(defun file-read-list (todo-file)
  "Reads the todo list from a file"
  (let ((name (concatenate 'string *todo-path todo-file)))
    (if (probe-file name) (with-open-file (f name) (read f)) (list todo-file))))



(defun display-todo-header (title)
  "Displays the title with an underline"
  (format t " ~a[1;4m~A~a[0m~%" #\escape title #\escape))
(defun with-color-fg (txt color)
  (format nil "~a[3~am~A~a[0m" #\escape color txt #\escape))
(defun display-todo-item (item index)
  "Displays an item with formatting"
  (format t "   ~A. ~A ~A~%"
          index
          (if (car item) (with-color-fg "✓" 2) "☐")
          (car (cdr item))))

(defun display-todo (todo-list)
  "Pretty prints a todo list"
  (display-todo-header (car todo-list))
  (let ((i 0))
    (dolist (item (cdr todo-list))
      (setf i (1+ i))
      (display-todo-item item i))
    todo-list))

(defun display-all-lists ()
  "Prints all available lists"
  (format t "You have the following todo lists active:~%")
  (let ((files (directory (concatenate 'string *todo-path "**/*"))))
    (dolist (item files)
      (format t "  • ~a~%"
              (subseq (namestring item)
                      (length (namestring (car (directory *todo-path))))))))
  nil) ; important this returns nil, so it doesn't try and write later

(defun display-help ()
  "Prints all option"
  (format t "Usage: todo [-l LIST] [ACTION]~%")
  (format t "    Where Action is one of the following:~%")
  (format t "    -r NUM      Remove the item with the index NUM~%")
  (format t "    -t NUM      Toggles the item with the index NUM~%")
  (format t "    -c          Clears all items that are done~%")
  (format t "    -ls         Prints the name of all your lists~%")
  (format t "    -count      Counts the number of items in the list~%")
  (format t "    -clear      Clears all items in the list~%")
  nil) ; important this returns nil, so it doesn't try and write later
