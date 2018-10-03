(ql:quickload '(:cl-cffi-gtk :cl-openal :cl-alc :cl-alut))


(defpackage :vpeer
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :vpeer)


(defstruct imgs
  (img nil)
  (defo (gdk-pixbuf-new-from-file "./img/neko.png"))
  (close-eye (gdk-pixbuf-new-from-file "./img/neko-close-eye.png")))

(defun re-draw (new-img imgs frame window)
  (gtk-widget-destroy (imgs-img imgs))
  ;;(gtk-container-remove frame (imgs-img imgs))
  (setf (imgs-img imgs) (gtk-image-new-from-pixbuf new-img))
  (gtk-container-add frame (imgs-img imgs))
  (gtk-widget-show (imgs-img imgs))
  ;;(gtk-widget-show-all window)
  )

(defun mabataki (imgs frame window)
  (re-draw (imgs-close-eye imgs) imgs frame window)
  (g-timeout-add
   200
   (lambda ()
     (re-draw (imgs-defo imgs) imgs frame window)
     nil)))

(defun main ()
  (setf *random-state* (make-random-state t))
  (within-main-loop
    (let* ((window (gtk-window-new :toplevel))
           (frame (make-instance 'gtk-frame :shadow-type :in))
           (imgs (make-imgs)))
      (setf (imgs-img imgs) (gtk-image-new-from-pixbuf (imgs-defo imgs)))
      ;; Signal handler for the window to handle the signal "destroy".
      (gtk-window-resize window 480 320)
      (setf (gtk-window-title window) "vpeer")
      (setf (gtk-widget-app-paintable window) t)
      ;;(setf (gtk-window-decorated window) nil)
      (let* ((screen (gtk-widget-get-screen frame))
             (visual (gdk-screen-get-rgba-visual screen)))
        (gtk-widget-set-visual window visual)
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            ;;(gtk-widget-destroy window)
                            (leave-gtk-main)))
        (g-signal-connect window "key-press-event"
                          (lambda (widget key)
                            (declare (ignore widget))
                            (when (= 113 (gdk-event-key-keyval key))
                              (if (gtk-window-decorated window)
                                  (setf (gtk-window-decorated window) nil)
                                  (setf (gtk-window-decorated window) t)))))

        (g-timeout-add
         3000
         (lambda ()
           (if (> 3 (random 4))
               (mabataki imgs frame window))
           t))
        (gtk-container-add frame (imgs-img imgs))
        (gtk-container-add window frame)
        ;; Show the window.
        (gtk-widget-show-all window))))
  (join-gtk-main))

(main)
