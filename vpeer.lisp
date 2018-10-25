(ql:quickload '(:cl-cffi-gtk :cl-openal :cl-alc :cl-alut))


(defpackage :vpeer
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :vpeer)

(defparameter *end* nil)
(defparameter *freq* 4410)
(defparameter *cap_size* 2048)


(defparameter *gazou-path-list*
  '("./img/tail0.png" "./img/tail1.png" "./img/tail2.png"
    "./img/lf-leg0.png" "./img/lf-leg1.png" "./img/lf-leg2.png" "./img/lf-leg3.png"
    "./img/lf-leg4.png" "./img/hige.png" "./img/hige-up.png" "./img/hige-down.png"
    "./img/body.png" "./img/open-mouth.png" "./img/close-mouth.png"
    "./img/open-eye.png"  "./img/eye1.png" "./img/close-eye.png"
    "./img/rf-leg0.png" "./img/rf-leg1.png" "./img/rf-leg2.png"
    "./img/rf-leg3.png"
    ))

(defparameter *imgs-func-list*
  (list 'imgs-tail0 'imgs-tail1 'imgs-tail2 'imgs-lf-leg 'imgs-lf-leg1
        'imgs-lf-leg2 'imgs-lf-leg3 'imgs-lf-leg4 'imgs-hige
        'imgs-hige-up 'imgs-hige-down 'imgs-body 'imgs-open-mouth
        'imgs-close-mouth 'imgs-open-eye 'imgs-looking-eye 'imgs-close-eye
        'imgs-rf-leg0 'imgs-rf-leg1 'imgs-rf-leg2 'imgs-rf-leg3))

(defun load-img (img)
  (gtk-image-new-from-pixbuf (gdk-pixbuf-new-from-file img)))

(defstruct imgs
  (img nil)

  (tail0 (load-img "./img/tail0.png"))
  (tail1 (load-img "./img/tail1.png"))
  (tail2 (load-img "./img/tail2.png"))
  (lf-leg (load-img "./img/lf-leg0.png"))
  (lf-leg1 (load-img "./img/lf-leg1.png"))
  (lf-leg2 (load-img "./img/lf-leg2.png"))
  (lf-leg3 (load-img "./img/lf-leg3.png"))
  (lf-leg4 (load-img "./img/lf-leg4.png"))
  (rf-leg0 (load-img "./img/rf-leg0.png"))
  (rf-leg1 (load-img "./img/rf-leg1.png"))
  (rf-leg2 (load-img "./img/rf-leg2.png"))
  (rf-leg3 (load-img "./img/rf-leg3.png"))
  (hige (load-img "./img/hige.png"))
  (hige-up (load-img "./img/hige-up.png"))
  (hige-down (load-img "./img/hige-down.png"))
  (body (load-img "./img/body.png"))
  (open-mouth (load-img "./img/open-mouth.png"))
  (close-mouth (load-img "./img/close-mouth.png"))
  (open-eye (load-img "./img/open-eye.png"))
  (looking-eye (load-img "./img/eye1.png"))
  (close-eye (load-img "./img/close-eye.png")))

(defstruct draw
  (rf-fr (make-instance 'gtk-frame))
  (hige-fr (make-instance 'gtk-frame))
  (body nil)
  (hige nil)
  (eye nil)
  (tail nil)
  (lf-leg nil)
  (rf-leg nil)
  (mouth nil))

(defparameter *draw-func-list*
  (list 'draw-body 'draw-hige 'draw-eye 'draw-tail 'draw-lf-leg 'draw-mouth))

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

;;新しい画像を表示
(defun re-draw (new-img moge func ol)
  (gtk-widget-destroy (funcall func moge))
  (funcall (fdefinition `(setf ,func)) new-img moge) ;;現在描画されてる画像
  (gtk-overlay-add-overlay ol new-img)
  (when (eq func 'draw-rf-leg)
    (gtk-overlay-reorder-overlay ol new-img 1)) ;;右手をひげの裏にする
  (gtk-widget-show (funcall func moge)))

;;time後にredraw
(defun time-redraw (time bool new-img moge func ol)
  (g-timeout-add time (lambda () (re-draw new-img moge func ol) bool)))


;;まばたきアニメ
(defun mabataki (imgs moge ol)
  (re-draw (imgs-close-eye imgs) moge 'draw-eye ol)
  (time-redraw 200 nil (imgs-open-eye imgs) moge 'draw-eye ol))

;;口パク
(defun kuchipaku (imgs moge ol)
  (re-draw (imgs-open-mouth imgs) moge 'draw-mouth ol)
  (time-redraw 200 nil (imgs-close-mouth imgs) moge 'draw-mouth ol))

;;ひげアニメ
(defun hige-anime (imgs moge ol)
  (re-draw (imgs-hige-up imgs) moge 'draw-hige ol)
  (time-redraw 100 nil (imgs-hige imgs) moge 'draw-hige ol)
  (time-redraw 200 nil (imgs-hige-down imgs) moge 'draw-hige ol)
  (time-redraw 300 nil (imgs-hige imgs) moge 'draw-hige ol))

;;しっぽあにめ
(defun tail-anime (imgs moge ol)
  (re-draw (imgs-tail1 imgs) moge 'draw-tail ol)
  (time-redraw 100 nil (imgs-tail2 imgs) moge 'draw-tail ol)
  (time-redraw 200 nil (imgs-tail1 imgs) moge 'draw-tail ol)
  (time-redraw 300 nil (imgs-tail0 imgs) moge 'draw-tail ol))

;;ノシ
(defun noshi (imgs moge ol)
  (re-draw (imgs-rf-leg1 imgs) moge 'draw-rf-leg ol)
  (time-redraw 100 nil (imgs-rf-leg2 imgs) moge 'draw-rf-leg ol)
  (time-redraw 200 nil (imgs-rf-leg1 imgs) moge 'draw-rf-leg ol)
  (time-redraw 300 nil (imgs-rf-leg3 imgs) moge 'draw-rf-leg ol)
  (time-redraw 400 nil (imgs-rf-leg1 imgs) moge 'draw-rf-leg ol)
  (time-redraw 500 nil (imgs-rf-leg2 imgs) moge 'draw-rf-leg ol)
  (time-redraw 600 nil (imgs-rf-leg1 imgs) moge 'draw-rf-leg ol)
  (time-redraw 700 nil (imgs-rf-leg3 imgs) moge 'draw-rf-leg ol)
  (time-redraw 800 nil (imgs-rf-leg1 imgs) moge 'draw-rf-leg ol)
  (time-redraw 900 nil (imgs-rf-leg2 imgs) moge 'draw-rf-leg ol)
  (time-redraw 1000 nil (imgs-rf-leg1 imgs) moge 'draw-rf-leg ol)
  (time-redraw 1100 nil (imgs-rf-leg3 imgs) moge 'draw-rf-leg ol)
  (time-redraw 1200 nil (imgs-rf-leg1 imgs) moge 'draw-rf-leg ol)
  (time-redraw 1300 nil (imgs-rf-leg0 imgs) moge 'draw-rf-leg ol)
  )

;;鼻の横カキカキ
(defun kakikaki (imgs moge ol)
  (re-draw (imgs-lf-leg1 imgs) moge 'draw-lf-leg ol)
  (time-redraw 100 nil (imgs-lf-leg2 imgs) moge 'draw-lf-leg ol)
  (time-redraw 200 nil (imgs-lf-leg3 imgs) moge 'draw-lf-leg ol)
  (time-redraw 400 nil (imgs-lf-leg4 imgs) moge 'draw-lf-leg ol)
  (time-redraw 600 nil (imgs-lf-leg3 imgs) moge 'draw-lf-leg ol)
  (time-redraw 800 nil (imgs-lf-leg4 imgs) moge 'draw-lf-leg ol)
  (time-redraw 1000 nil (imgs-lf-leg3 imgs) moge 'draw-lf-leg ol)
  (time-redraw 1100 nil (imgs-lf-leg2 imgs) moge 'draw-lf-leg ol)
  (time-redraw 1200 nil (imgs-lf-leg1 imgs) moge 'draw-lf-leg ol)
  (time-redraw 1300 nil (imgs-lf-leg imgs) moge 'draw-lf-leg ol))

;;audio init
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

;;free
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

;;マイクから音ひろう
(defun altest2 (imgs moge ol )
  (let* ((error-code 0)
         (al (make-al)))
    (al-init al)
    (alc:capture-start (al-inputdev al))
    (setf error-code (alc:get-error (al-inputdev al)))
    (loop  while (null *end*)
           do (%alc:get-integer-v (al-inputdev al) :capture-samples 1 (al-samplesin al))
              (when (> (mem-ref (al-samplesin al) :int) *cap_size*)
                (%alc:capture-samples (al-inputdev al) (al-buffer al) *cap_size*)
                ;;)
                (loop for i from 0 below *cap_size*
                      do (when (and (>= (abs (mem-aref (al-buffer al) :short i)) 300)
                                    (equalp (draw-mouth moge) (imgs-close-mouth imgs)))
                           (kuchipaku imgs moge ol)
                           (return))))
              (gtk-main-iteration-do nil))
    (al-free al)))

;;キーイベント
(defun key-event (key imgs moge ol window )
  (let ((hoge (gdk-event-key-keyval key)))
    (cond
      ((= (char-code #\q) hoge) ;;終了
       (setf *end* t)
       (gtk-widget-destroy window)
       (leave-gtk-main))
      ((= (char-code #\w) hoge) ;;ウィンドウ枠消したりつけたり
       ;;(setf *end* t) ;;test
       (if (gtk-window-decorated window)
           (setf (gtk-window-decorated window) nil)
           (setf (gtk-window-decorated window) t)))
      ((= (char-code #\n) hoge)
       (noshi imgs moge ol))
      ((= (char-code #\s) hoge) ;;音声入力開始
       (setf *end* nil)
       (altest2 imgs moge ol )))))

;;目線変更
(defun change-mesen (imgs moge ol)
  (when (not (equalp (draw-eye moge) (imgs-close-eye imgs)))
    (if (equalp (draw-eye moge) (imgs-open-eye imgs))
        (re-draw (imgs-looking-eye imgs) moge 'draw-eye ol)
        (re-draw (imgs-open-eye imgs) moge 'draw-eye ol))))

;;アニメーション
(defun random-anime (imgs moge ol)
  (g-timeout-add
   3000
   (lambda ()
     (let ((x (random 6)))
       (case x
         (0 (mabataki imgs moge ol))
         (1 (hige-anime imgs moge ol))
         (2 (kakikaki imgs moge ol))
         (3 (tail-anime imgs moge ol))
         (5 (change-mesen imgs moge ol))
         (4 nil)))
     t)))

;;画像サイズ変更
(defun gazou-reset (imgs width height)
  (loop for path in *gazou-path-list*
        for func in *imgs-func-list*
        do (let* ((pix (gdk-pixbuf-new-from-file path))
                  (new-pix (gdk-pixbuf-scale-simple pix width height :nearest))
                  (img (gtk-image-new-from-pixbuf new-pix)))
             (funcall (fdefinition `(setf ,func)) img imgs))))

;;画像のサイズ変更
(defun change-size (imgs moge ol window)
  (dolist (func *draw-func-list*)
    (gtk-widget-destroy (funcall func moge)))
  (multiple-value-bind (width height)
      (gtk-window-get-size window)
    (gazou-reset imgs width height)
    (setf (draw-body moge) (imgs-body imgs)
          (draw-eye moge) (imgs-open-eye imgs)
          (draw-hige moge) (imgs-hige imgs)
          (draw-mouth moge) (imgs-close-mouth imgs)
          (draw-tail moge) (imgs-tail0 imgs)
          (draw-rf-leg moge) (imgs-rf-leg0 imgs)
          (draw-lf-leg moge) (imgs-lf-leg imgs))
    (dolist (func *draw-func-list*)
      (gtk-overlay-add-overlay ol (funcall func moge))
      (gtk-widget-show (funcall func moge)))))

(defun main ()
  (setf *random-state* (make-random-state t))
  (within-main-loop
    (let* ((window (gtk-window-new :toplevel))
           (ol (make-instance 'gtk-overlay :app-paintable t))
           (imgs (make-imgs))
           (moge (make-draw :body (imgs-body imgs) :lf-leg (imgs-lf-leg imgs)
                            :hige (imgs-hige imgs) :tail (imgs-tail0 imgs)
                            :eye (imgs-open-eye imgs) :rf-leg (imgs-rf-leg0 imgs)
                            :mouth (imgs-close-mouth imgs))))
      (gtk-window-resize window 570 380)
      (change-size imgs moge ol window) ;;画像リサイズ
      (setf (gtk-window-title window) "vpeer")
      (setf (gtk-widget-app-paintable window) t)
      ;;(setf (gtk-window-decorated window) nil)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (setf *end* t)
                          (leave-gtk-main)))
      (g-signal-connect window "key-press-event"
                        (lambda (widget key)
                          (declare (ignore widget))
                          (key-event key imgs moge ol window )))
      ;;重たい
      ;; (g-signal-connect window "configure-event"
      ;;                   (lambda (widget hoge)
      ;;                     (declare (ignore widget hoge))
      ;;                     (change-size imgs moge ol window)))
      (random-anime imgs moge ol)
      (let* ((screen (gtk-widget-get-screen  window))
             (visual (gdk-screen-get-rgba-visual screen)))
        (gtk-widget-set-visual window visual)
        (gtk-overlay-add-overlay ol (draw-body moge))
        (gtk-overlay-add-overlay ol (draw-eye moge))
        (gtk-overlay-add-overlay ol (draw-tail moge))
        (gtk-overlay-add-overlay ol (draw-mouth moge))
        (gtk-overlay-add-overlay ol (draw-rf-leg moge))
        (gtk-overlay-add-overlay ol (draw-hige moge))
        (gtk-overlay-add-overlay ol (draw-lf-leg moge))
        (gtk-container-add window ol)
        ;; Show the window.
        (gtk-widget-show-all window))
      ))
  (join-gtk-main))

(main)
