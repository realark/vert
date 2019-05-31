(in-package :recurse.vert)

(defclass gl-drawable ()
  ((shader :initform nil :reader shader)
   (vao :initform 0 :reader vao)
   (texture :initform nil :reader texture))
  (:documentation "A class drawn with opengl.
Its reader slots will be used by the game scene to optimize rendering by reducing gl state changes."))

(defun gl-< (gl-drawable1 gl-drawable2)
  (declare (gl-drawable gl-drawable1 gl-drawable2))
  (with-accessors ((s1 shader)
                   (vao1 vao)
                   (t1 texture))
      gl-drawable1
    (with-accessors ((s2 shader)
                     (vao2 vao)
                     (t2 texture))
        gl-drawable2
      (cond
        ((< (if s1 (shader-program-id s1) 0)
            (if s2 (shader-program-id s2) 0))
         t)
        ((> (if s1 (shader-program-id s1) 0)
            (if s2 (shader-program-id s2) 0))
         nil)
        ((< vao1 vao2)
         t)
        ((> vao1 vao2)
         nil)
        ((< (if t1 (texture-id t1) 0)
            (if t2 (texture-id t2) 0))
         t)
        ((> (if t1 (texture-id t1) 0)
            (if t2 (texture-id t2) 0))
         nil)
        ;; gl drawables are equal
        (t t)))))
