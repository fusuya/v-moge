(ql:quickload '(:cl-cffi-gtk :cl-openal :cl-alc :cl-alut))


(defpackage :vpeer
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :vpeer)

(defparameter *end* nil)
(defparameter *freq* 22050)
(defparameter *cap_size* 2048)

;;音声再生部分いらない
(defun altest2 (imgs frame window)
  ;;(print 7)
  (let* ((bufq nil)
         (error-code 0)
         (hellobuf (cffi:foreign-alloc :uint :count 16))
         (hellosource (cffi:foreign-alloc :uint))
         (audiodev nil) (audiocontext nil)
         (inputdev nil)
         (buffer nil) (samplesin nil) (availbuffers nil)
         (mybuff nil) (bufferholder nil))
    ;;(print 0)
    (setf audiodev (alc:open-device nil)
          error-code (alc:get-error audiodev)
          audiocontext (alc:create-context audiodev))
    ;;(print 1.1)
    (alc:make-context-current audiocontext)
    (setf error-code (alc:get-error audiodev))
    (setf inputdev (alc:capture-open-device (null-pointer) *freq* #x1101 (floor *freq* 2))
          error-code (alc:get-error inputdev))
    (alc:capture-start inputdev)
    (setf error-code (alc:get-error inputdev))
    ;;(print 1)
    (%al:gen-buffers 16 hellobuf)
    (setf error-code (al:get-error))

    (loop for ii from 0 below 16
          do (push (mem-aref hellobuf :uint ii) bufq))
    (setf bufq (reverse bufq))
    (%al:gen-sources 1 hellosource)
    (setf error-code (al:get-error))
    ;;(print 2)
    (setf buffer (cffi:foreign-alloc :short :count (* *freq* 2))
          samplesin (cffi:foreign-alloc :int :initial-element 0)
          availbuffers (cffi:foreign-alloc :int :initial-element 0)
          mybuff (cffi:foreign-alloc :uint)
          bufferholder (cffi:foreign-alloc :uint :count 16))
    ;;(print 3)
    (loop  while (null *end*)
           do (%al:get-source-i (mem-aref hellosource :uint 0) :buffers-processed availbuffers)
              (when (> (mem-ref availbuffers :int) 0)
                (%al:source-unqueue-buffers (mem-aref hellosource :uint 0) (mem-ref availbuffers :int) bufferholder)
                (loop for ii from 0 below (mem-ref availbuffers :int)
                      do (setf bufq (append bufq (list (mem-aref bufferholder :uint ii))))))
              (%alc:get-integer-v inputdev :capture-samples 1 samplesin)
              (when (> (mem-ref samplesin :int) *cap_size*)
                (%alc:capture-samples inputdev buffer *cap_size*)
                (when (find-if #'(lambda (x) (>= (abs x) 200))
                               (loop for i from 0 below *cap_size*
                                     collect (mem-aref buffer :short i)))
                  (mabataki imgs frame window))
                (when bufq
                  (let ((state (cffi:foreign-alloc :int :initial-element 0)))
                    (setf (mem-ref mybuff :uint) (car bufq)
                          bufq (cdr bufq))
                    (al:buffer-data (mem-ref mybuff :uint) #x1101 buffer (* *cap_size* (cffi:foreign-type-size :short)) *freq*)
                    ;;(when (= count 0)
                    ;;  (format t "~s~%" (loop for i from 0 below *cap_size*
                    ;;                         collect (mem-aref buffer :short i))))
                    (%al:source-queue-buffers (mem-aref hellosource :uint 0) 1 mybuff)
                    (%al:get-source-i (mem-aref hellosource :uint 0) :source-state state)
                    (when (/= (mem-ref state :int) #x1012)
                      (al:source-play (mem-aref hellosource :uint 0)))
                    ;;(incf count)
                    (cffi:foreign-free state))))
              (gtk-main-iteration-do nil))

    (cffi:foreign-free buffer)
    (cffi:foreign-free samplesin)
    (cffi:foreign-free availbuffers)
    (cffi:foreign-free mybuff)
    (cffi:foreign-free bufferholder)

    (alc:capture-stop inputdev)
    (alc:capture-close-device inputdev)
    (%al:source-stop-v 1 hellosource)
    (loop for ii from 0 below 1
          do (%al:source-i (mem-aref hellosource :uint 0) :buffer 0))
    (%al:delete-sources 1 hellosource)
    (%al:delete-buffers 16 hellobuf)
    (cffi:foreign-free hellosource)
    (cffi:foreign-free hellobuf)

    (setf error-code (al:get-error))
    (alc:make-context-current (null-pointer))
    (setf error-code (al:get-error))
    (alc:destroy-context audiocontext)
    (alc:close-device audiodev)))



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
                            (let ((hoge (gdk-event-key-keyval key)))
                              (cond
                                ((= (char-code #\q) hoge)
                                 (setf *end* t) ;;test
                                 (mabataki imgs frame window)
                                 (if (gtk-window-decorated window)
                                     (setf (gtk-window-decorated window) nil)
                                     (setf (gtk-window-decorated window) t)))
                                ((= (char-code #\s) hoge)
                                 (setf *end* nil)
                                 (mabataki imgs frame window)
                                 (altest2 imgs frame window))))))
        ;; (g-timeout-add
        ;;  3000
        ;;  (lambda ()
        ;;    (if (> 3 (random 4))
        ;;        (mabataki imgs frame window))
        ;;    t))

        (gtk-container-add frame (imgs-img imgs))
        (gtk-container-add window frame)
        ;; Show the window.
        (gtk-widget-show-all window))
      ;;(altest2 imgs frame window)
      ))
  (join-gtk-main))

(main)
