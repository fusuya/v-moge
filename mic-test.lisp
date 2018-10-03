(ql:quickload '(:cl-cffi-gtk :cl-openal :cl-alc :cl-alut))


(defpackage :mic
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp))


(in-package :mic)



(defparameter *freq* 22050)
(defparameter *cap_size* 2048)

;;参考
;;https://stackoverflow.com/questions/4087727/openal-how-to-create-simple-microphone-echo-programm
;;マイクから入ってきた音声を再生するだけ。ちょっと遅れて再生される

;;やりたいこと→音声に合わせて口パク
;;マイクからの音声の音量取得？して一定以上だったら口開ける画像表示する　わからん
(defun altest2 ()
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
    (loop for count = 0 then count ;;test
          do (%al:get-source-i (mem-aref hellosource :uint 0) :buffers-processed availbuffers)
             (when (> (mem-ref availbuffers :int) 0)
               (%al:source-unqueue-buffers (mem-aref hellosource :uint 0) (mem-ref availbuffers :int) bufferholder)
               (loop for ii from 0 below (mem-ref availbuffers :int)
                     do (setf bufq (append bufq (list (mem-aref bufferholder :uint ii))))))
             (%alc:get-integer-v inputdev :capture-samples 1 samplesin)
             (when (> (mem-ref samplesin :int) *cap_size*)
               (%alc:capture-samples inputdev buffer *cap_size*)
               (when bufq
                 (let ((state (cffi:foreign-alloc :int :initial-element 0)))
                   (setf (mem-ref mybuff :uint) (car bufq)
                         bufq (cdr bufq))
                   (al:buffer-data (mem-ref mybuff :uint) #x1101 buffer (* *cap_size* (cffi:foreign-type-size :short)) *freq*)
                   (%al:source-queue-buffers (mem-aref hellosource :uint 0) 1 mybuff)
                   (%al:get-source-i (mem-aref hellosource :uint 0) :source-state state)
                   (when (/= (mem-ref state :int) #x1012)
                     (al:source-play (mem-aref hellosource :uint 0)))
                   (incf count)
                   (cffi:foreign-free state))))
             (when (= count 50)
               (return)))

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


;;(altest2)