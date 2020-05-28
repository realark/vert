(in-package :recurse.vert)

;;;; Goals:
;;;; - be able to use gl-pipeline features on sprite class
;;;; - be able to set sprite data on the sprite class
;;;; - call render on the sprite class and have it "do the right thing"


;;;; Options
;;;; subclass pipeline and store sprite rendering in a separate component
;;;; - pros:
;;;;  - easy to add new effects
;;;;  - can simply call RENDER on object
;;;; - cons:
;;;;  - all manipulations to the base sprite would be tricky
;;;;   - setting sprite-source would require a pass-through method to the underlying draw component

;;;; subclass drawable and put pipeline in a component
;;;; - pro/cons are the reverse of the previous options. Easy to manipulate drawing info, but fucky when rendering and fucky when wanting to access the pipeline
;;;; - con: pipeline would contain itself

;;;; subclass pipeline and gl-drawable, then add custom sprite options
;;;; - cons
;;;;  - rendering would be super weird, as the pipeline would contain itself

@export-class
(defclass sprite (gl-pipeline obb)
  ((path :initarg :path
         :accessor sprite-path
         :documentation "Location of the sprite png to render. If null only the color mod will render."
         :initform nil)
   (color :initarg :color
          :initform nil
          :accessor color
          :documentation "Optional color mod to apply to the sprite.")
   (texture-id :initform -1
               :documentation "opengl texture id")
   (quad :initform nil)
   (sprite-releaser :initform nil))
  (:documentation "An object which renders a sprite (or portion of a sprite) to the screen."))

(defmethod initialize-instance :around ((sprite sprite) &rest args)
  ;; NOTE: consing
  (let ((all-args (append (list sprite) args)))
    (prog1 (apply #'call-next-method all-args)
      (with-slots (path quad color) sprite
        (setf quad (make-instance 'gl-quad
                                  :color color
                                  :render-area sprite))
        (gl-pipeline-add sprite quad))
      ;; TODO vvv
      #+nil
      (resource-autoloader-add-object *resource-autoloader*
                                      (tg:make-weak-pointer sprite)))))

(defmethod (setf color) :after (new-color (sprite sprite))
  (with-slots (quad color) sprite
    (setf (color quad) color)))

;; TODO
;; (defmethod (setf sprite-path) :around (new-sprite-path (static-sprite static-sprite))
;;   (let ((old-path (path-to-sprite static-sprite)))
;;     (prog1 (call-next-method new-sprite-path static-sprite)
;;       (unless (or (equal old-path (path-to-sprite static-sprite))
;;                   (null *engine-manager*))
;;         (release-resources static-sprite)
;;         (load-resources static-sprite)))))

(defmethod load-resources ((sprite sprite))
  (prog1 (call-next-method sprite)
    ;; TODO load up texture and set releaser
    ;; set texture on the base class so it is automagically scooped up by gl-quad
    ))

(defmethod release-resources ((sprite sprite))
  (prog1 (call-next-method sprite)
    ;; TODO release texture and unset releaser
    ))
