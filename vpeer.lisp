(ql:quickload '(:cl-cffi-gtk :cl-openal :cl-alc :cl-alut))


(defpackage :vpeer
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :vpeer)

(defparameter *end* nil)
(defparameter *freq* 22050)
(defparameter *cap_size* 2048)




(defun load-img (img)
  (gtk-image-new-from-pixbuf (gdk-pixbuf-new-from-file img)))

(defstruct imgs
  (img nil)
  (body (load-img "./img/body.png"))
  (open-mouth (load-img "./img/open-mouth.png"))
  (close-mouth (load-img "./img/close-mouth.png"))
  (open-eye (load-img "./img/open-eye.png"))
  (close-eye (load-img "./img/close-eye.png")))

(defstruct draw
  (body nil)
  (eye nil)
  (mouth nil))

(defstruct al
  (buf (cffi:foreign-alloc :uint :count 16))
  (source (cffi:foreign-alloc :uint))
  (audiodev nil)
  (audiocontext nil)
  (inputdev nil)
  (bufq nil)
  (buffer (cffi:foreign-alloc :short :count (* *freq* 2)))
  (samplesin (cffi:foreign-alloc :int :initial-element 0))
  (availbuffers (cffi:foreign-alloc :int :initial-element 0))
  (mybuff (cffi:foreign-alloc :uint))
  (state (cffi:foreign-alloc :int :initial-element 0))
  (bufferholder (cffi:foreign-alloc :uint :count 16)))

(defun re-draw (new-img moge func ol)
  (gtk-widget-destroy (funcall func moge))
  (funcall (fdefinition `(setf ,func)) new-img moge)
  ;;(gtk-container-add frame (funcall func moge))
  (gtk-overlay-add-overlay ol (funcall func moge))
  (gtk-widget-show (funcall func moge))
  )

(defun mabataki (imgs moge ol)
  (re-draw (imgs-close-eye imgs) moge 'draw-eye ol)
  (g-timeout-add
   200
   (lambda ()
     (re-draw (imgs-open-eye imgs) moge 'draw-eye ol)
     nil)))

(defun kuchipaku (imgs moge ol)
  (re-draw (imgs-open-mouth imgs) moge 'draw-mouth ol)
  (g-timeout-add
   200
   (lambda ()
     (re-draw (imgs-close-mouth imgs) moge 'draw-mouth ol)
     nil)))



(defun al-init (al)
  (let ((error-code 0))
    (setf (al-audiodev al) (alc:open-device nil)
          error-code (alc:get-error (al-audiodev al))
          (al-audiocontext al) (alc:create-context (al-audiodev al)))
    (alc:make-context-current (al-audiocontext al))
    (setf error-code (alc:get-error (al-audiodev al)))
    (setf (al-inputdev al) (alc:capture-open-device (null-pointer) *freq* #x1101 (floor *freq* 2))
          error-code (alc:get-error (al-inputdev al)))
    (%al:gen-buffers 16 (al-buf al))
    (setf error-code (al:get-error))
    (loop for ii from 0 below 16
          do (push (mem-aref (al-buf al) :uint ii) (al-bufq al)))
    (setf (al-bufq al) (reverse (al-bufq al)))
    (%al:gen-sources 1 (al-source al))
    (setf error-code (al:get-error))))


(defun al-free (al)
  (let ((error-code 0))
    (cffi:foreign-free (al-buffer al))
    (cffi:foreign-free (al-samplesin al))
    (cffi:foreign-free (al-availbuffers al))
    (cffi:foreign-free (al-mybuff al))
    (cffi:foreign-free (al-state al))
    (cffi:foreign-free (al-bufferholder al))
    (alc:capture-stop (al-inputdev al))
    (alc:capture-close-device (al-inputdev al))
    (%al:source-stop-v 1 (al-source al))
    (loop for ii from 0 below 1
          do (%al:source-i (mem-aref (al-source al) :uint 0) :buffer 0))
    (%al:delete-sources 1 (al-source al))
    (%al:delete-buffers 16 (al-buf al))
    (cffi:foreign-free (al-source al))
    (cffi:foreign-free (al-buf al))
    (setf error-code (al:get-error))
    (alc:make-context-current (null-pointer))
    (setf error-code (al:get-error))
    (alc:destroy-context (al-audiocontext al))
    (alc:close-device (al-audiodev al))
    ))

;;音声再生部分いらない
(defun altest2 (imgs moge ol)
  (let* ((error-code 0)
         (al (make-al)))
    (al-init al)
    (alc:capture-start (al-inputdev al))
    (setf error-code (alc:get-error (al-inputdev al)))
    (loop  while (null *end*)
           do (%alc:get-integer-v (al-inputdev al) :capture-samples 1 (al-samplesin al))
              (when (> (mem-ref (al-samplesin al) :int) *cap_size*)
                (%alc:capture-samples (al-inputdev al) (al-buffer al) *cap_size*)
                (when (and (find-if #'(lambda (x) (>= (abs x) 300))
                                    (loop for i from 0 below *cap_size*
                                          collect (mem-aref (al-buffer al) :short i)))
                           (equal (draw-mouth moge) (imgs-close-mouth imgs)))
                  (kuchipaku imgs moge ol)))
              (gtk-main-iteration-do nil))
    (al-free al)))

(defun key-event (key imgs moge ol window)
  (let ((hoge (gdk-event-key-keyval key)))
    (cond
      ((= (char-code #\q) hoge)
       (setf *end* t)
       (gtk-widget-destroy window)
       (leave-gtk-main))
      ((= (char-code #\w) hoge)
       (setf *end* t) ;;test
       ;;(mabataki imgs moge eye-f ol)
       (if (gtk-window-decorated window)
           (setf (gtk-window-decorated window) nil)
           (setf (gtk-window-decorated window) t)))
      ((= (char-code #\s) hoge)
       (setf *end* nil)
       ;;(mabataki imgs moge eye-f ol)
       (altest2 imgs moge ol)))))

(defun main ()
  (setf *random-state* (make-random-state t))
  (within-main-loop
    (let* ((window (gtk-window-new :toplevel))
           (body-f (make-instance 'gtk-frame :shadow-type :in :app-paintable t))
           (eye-f (make-instance 'gtk-frame :shadow-type :in :app-paintable t))
           (mouth-f (make-instance 'gtk-frame :shadow-type :in :app-paintable t))
           (ol (make-instance 'gtk-overlay :app-paintable t))
           (imgs (make-imgs))
           (moge (make-draw :body (imgs-body imgs)
                            :eye (imgs-open-eye imgs)
                            :mouth (imgs-close-mouth imgs))))
      ;; Signal handler for the window to handle the signal "destroy".
      (gtk-window-resize window 480 320)
      (setf (gtk-window-title window) "vpeer")
      (setf (gtk-widget-app-paintable window) t)
      ;;(setf (gtk-window-decorated window) nil)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect window "key-press-event"
                        (lambda (widget key)
                          (declare (ignore widget))
                          (key-event key imgs moge ol window )))
      (g-timeout-add
       3000
       (lambda ()
         (if (> 3 (random 4))
             (mabataki imgs moge ol))
         t))
      (let* ((screen (gtk-widget-get-screen  window))
             (visual (gdk-screen-get-rgba-visual screen)))
        (gtk-widget-set-visual window visual)


        ;;(gtk-container-add body-f (draw-body moge))
        ;;(gtk-container-add eye-f (draw-eye moge))
        ;;(gtk-container-add mouth-f (draw-mouth moge))
        ;; (gtk-overlay-add-overlay ol body-f)
        ;; (gtk-overlay-add-overlay ol eye-f)
        ;;(gtk-overlay-add-overlay ol mouth-f)
        (gtk-overlay-add-overlay ol (draw-body moge))
        (gtk-overlay-add-overlay ol (draw-eye moge))
        (gtk-overlay-add-overlay ol (draw-mouth moge))
        (gtk-container-add window ol)
        ;; Show the window.
        (gtk-widget-show-all window))
      ;;(altest2 imgs frame window)
      ))
  (join-gtk-main))

(main)
