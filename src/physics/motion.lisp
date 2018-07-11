(in-package :recurse.vert)

(defmacro defmotion (name ((object-name object-class) delta-t-ms (physics-context physics-context-class)) &body body)
  "Define continuous motion for an object over a period of time."
  (loop for name-or-class in (list name object-name object-class) do
       (unless (symbolp name-or-class)
         (error "Expected symbol. Got ~A" name-or-class)))
  `(defmethod update-motion :after
     ((,object-name ,object-class) ,delta-t-ms (,physics-context ,physics-context-class))
     ,@body))
