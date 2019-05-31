(in-package :recurse.vert)

(progn
  (defstruct (color (:conc-name %color-))
    "RGBA Color Representation"
    (r 0.0 :type single-float)
    (g 0.0 :type single-float)
    (b 0.0 :type single-float)
    (a 1.0 :type single-float))
  (export '(make-color)))

@export
(defun make-color-rgba (&key (r 0) (g 0) (b 0) (a 255))
  (declare ((unsigned-byte 8) r g b a))
  (make-color :r (float (/ r 255))
              :g (float (/ g 255))
              :b (float (/ b 255))
              :a (float (/ a 255))))

(progn
  (defstruct (immutable-color (:include color))
    "Immutable RGBA Color Representation")
  (export '(make-immutable-color)))

@export
(defun make-immutable-color-rgba (&key (r 0) (g 0) (b 0) (a 255))
  (declare ((unsigned-byte 8) r g b a))
  (make-immutable-color :r (float (/ r 255))
                        :g (float (/ g 255))
                        :b (float (/ b 255))
                        :a (float (/ a 255))))

@export
(defun lerp (from-color to-color percent-to &key (output-color (make-color)))
  (declare (color from-color to-color output-color)
           ((single-float 0.0 1.0) percent-to))
  (flet ((interpolate-color (from-color to-color percent-to)
           (declare (single-float from-color to-color)
                    ((float 0.0 1.0) percent-to))
           (let ((percent-from (- 1.0 percent-to)))
             (+ (* from-color percent-from)
                (* to-color percent-to)))))
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

(macrolet ((def-color-getter-and-setter (c)
             (assert (typep c 'symbol))
             `(progn
                @export
                (defgeneric ,c (color)
                  (:method ((color color))
                    (,(alexandria:symbolicate '%color- c) color)))

                @export
                (defgeneric (setf ,c) (new-val color)
                  (:method (new-val (color color))
                    (setf (,(alexandria:symbolicate '%color- c) color) new-val))
                  (:method (new-val (immutable-color immutable-color))
                    (error "cannot change immutable color: ~A" immutable-color))))))
  (def-color-getter-and-setter r)
  (def-color-getter-and-setter g)
  (def-color-getter-and-setter b)
  (def-color-getter-and-setter a))


;; TODO: allow for fixed colors in random
@export
(defun make-random-color-rgba (&optional random-alpha)
  (make-color-rgba :r (random 256)
                   :g (random 256)
                   :b (random 256)
                   :a (if random-alpha (random 256) 255)))

@export
(defparameter *black* (make-immutable-color-rgba :r 0 :g 0 :b 0 :a 255))
@export
(defparameter *white* (make-immutable-color-rgba :r 255 :g 255 :b 255 :a 255))
@export
(defparameter *grey* (make-immutable-color-rgba :r 128 :g 128 :b 128))
@export
(defparameter *gray* *grey*)
@export
(defparameter *red*   (make-immutable-color-rgba :r 255 :g 0 :b 0 :a 255))
@export
(defparameter *orange*(make-immutable-color-rgba :r 255 :g 140 :b 0 :a 255))
@export
(defparameter *green* (make-immutable-color-rgba :r 0 :g 255 :b 0 :a 255))
@export
(defparameter *blue*  (make-immutable-color-rgba :r 0 :g 0 :b 255 :a 255))
@export
(defparameter *yellow*  (make-immutable-color-rgba :r 255 :g 255 :b 0 :a 255))
@export
(defparameter *invisible*  (make-immutable-color-rgba :r 0 :g 0 :b 0 :a 0))
