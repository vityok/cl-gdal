;; Opens vector geographic files and displays information about layers
;; and features that they contain.
;;
;; This program is intended to serve as a demonstration and a
;; prototype to check how useful and convenient OGR library Lisp API
;; is in practice.
;;
;; This program depends on the LTK system that is available in
;; quicklisp and avoids external dependencies put forward by the GTK.
;;
;; Load:
;;
;; sbcl --load ogr-info-tk.lisp --eval '(ogr-info-demo:main)'
;; lx86cl --load ogr-info-tk.lisp --eval '(ogr-info-demo:main)'


;; TODO
;;
;; open selected file
;; list layers
;; draw features in selected area
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :ltk)
  (ql:quickload :cl-ogr))

(defpackage :ogr-info-demo
  (:use :cl)
  (:export :main))

(in-package :ogr-info-demo)

;; --------------------------------------------------------

(defun inspect-ogr-file (shp)
  (format t "inspecting file: ~a~%" shp)
  (let ((hDS (ogr:ogr-open shp 0 (cffi:null-pointer))))
    (when (cffi:null-pointer-p hds)
      (error "Failed to open file"))
    (format t "it contains: ~a layers~%" (ogr:ogr-ds-get-layer-count hds))

    (loop for i from 0 below (ogr:ogr-ds-get-layer-count hds) do
         (let ((layer (ogr:ogr-ds-get-layer hds i)))
           (format t "layer[~a]: ~a~%" i (ogr:ogr-l-get-name layer))))))

;; --------------------------------------------------------

(defun gui-choose-file-dialog ()
  (let ((fname (ltk:get-open-file)))
    (format t "file: ~a~%" fname)
    (when fname
      (inspect-ogr-file fname))))

;; --------------------------------------------------------

(defun main ()
  (ogr:ogr-register-all)

  (ltk:with-ltk ()
    (gui-choose-file-dialog)
    )
  )

;; EOF