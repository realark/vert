(in-package :recurse.vert)

(defstruct (color :conc-name)
  "RGBA Color Representation"
  (r 0 :type (unsigned-byte 8))
  (g 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 8))
  (a 255 :type (unsigned-byte 8)))

(defun lerp (from-color to-color percent-to &key (output-color (make-color)))
  (declare (color from-color to-color output-color)
           ((float 0.0 1.0) percent-to))
  (flet ((interpolate-color (from-color to-color percent-to)
           (declare ((unsigned-byte 8) from-color to-color)
                    ((float 0.0 1.0) percent-to))
           (let ((percent-from (- 1.0 percent-to)))
             (the (unsigned-byte 8) (+ (floor (* from-color percent-from))
                                       (ceiling (* to-color percent-to)))))))
    (setf (r output-color) (interpolate-color (r from-color)
                                              (r to-color)
                                              percent-to)
          (g output-color) (interpolate-color (g from-color)
                                              (g to-color)
                                              percent-to)
          (b output-color) (interpolate-color (b from-color)
                                              (b to-color)
                                              percent-to)
          (a output-color) (interpolate-color (a from-color)
                                              (a to-color)
                                              percent-to)))
  output-color)

@export
(defun make-random-color (&optional random-alpha)
  (make-color :r (random 256)
              :g (random 256)
              :b (random 256)
              :a (if random-alpha (random 256) 255)))

@export
(defparameter *black* (make-color :r 0 :g 0 :b 0 :a 255))
@export
(defparameter *white* (make-color :r 255 :g 255 :b 255 :a 255))
@export
(defparameter *red*   (make-color :r 255 :g 0 :b 0 :a 255))
@export
(defparameter *orange*(make-color :r 255 :g 140 :b 0 :a 255))
@export
(defparameter *green* (make-color :r 0 :g 255 :b 0 :a 255))
@export
(defparameter *blue*  (make-color :r 0 :g 0 :b 255 :a 255))
@export
(defparameter *yelllow*  (make-color :r 255 :g 255 :b 0 :a 255))
@export
(defparameter *invisible*  (make-color :r 0 :g 0 :b 0 :a 0))
