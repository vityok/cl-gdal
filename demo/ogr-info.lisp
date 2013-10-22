;; open vector geographic files and display information about layers
;; and features that they contain
;;
;; This program is intended to serve as a demonstration and a
;; prototype to check how useful and convenient OGR library Lisp API
;; is in practice.
;;
;; This program depends on the cl-cffi-gtk system that is available on
;; GitHub: https://github.com/crategus/cl-cffi-gtk
;;
;; Load:
;;
;; sbcl --load ogr-info.lisp --eval '(ogr-info-demo:main)'

(ql:quickload :cl-ogr)
(ql:quickload :cl-cffi-gtk)
(ql:quickload :cl-cffi-gtk-gobject)

(defpackage :ogr-info-demo
  (:use :cl :gtk :gobject)
  (:export #:main))

(in-package :ogr-info-demo)

;; --------------------------------------------------------

(defun example-file-chooser-dialog ()
  (within-main-loop
   (let ((window (make-instance 'gtk-window
                                :title "Example File Chooser Dialog"
                                :type :toplevel
                                :border-width 12
                                :default-width 300
                                :default-height 100))
         (button (make-instance 'gtk-button
                                :label "Select a file for save ..."
                                :image
                                (gtk-image-new-from-stock "gtk-save"
                                                          :button))))
     ;; Handle the signal "destroy" for the window.
     (g-signal-connect window "destroy"
                       (lambda (widget)
                         (declare (ignore widget))
                         (leave-gtk-main)))
     ;; Handle the signal "clicked" for the button.
     (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         (let ((dialog (gtk-file-chooser-dialog-new "Speichern"
                                                                    nil
                                                                    :open
                                                                    "gtk-open" :accept
                                                                    "gtk-cancel" :cancel)))
                           (when (eq (gtk-dialog-run dialog) :accept)
                             (format t "Saved to file ~A~%"
                                     (gtk-file-chooser-get-filename dialog)))
                           (gtk-widget-destroy dialog))))
     (gtk-container-add window button)
     (gtk-widget-show-all window))))

;; --------------------------------------------------------

(defun main ()
  (example-file-chooser-dialog)
  )
