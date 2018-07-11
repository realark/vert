(in-package :recurse.vert)

(defparameter *dev-mode*
  nil
  "Set to non-nil to enable dev features (potentially at a cost to performance).")

(defun dev-mode-post-game-loop-iteration ()
  (reload-textures-if-changed))

(defun reload-textures-if-changed ()
  "If any textures have changed, flush the texture cache and reload."
  (flet ((reload-texture (objects-using-texture)
           (loop for object across (copy-seq objects-using-texture) do
                (release-resources object))))
    (%do-cache (*resource-cache* path tex :mtime original-mtime :objects-using objects-using-texture)
      (declare (ignore tex))
      (let ((current-mtime (file-write-date path)))
        (when (/= current-mtime original-mtime)
          (reload-texture objects-using-texture))))))
