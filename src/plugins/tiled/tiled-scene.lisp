(in-package :recurse.vert)

(progn
  @export
  (defclass tiled-scene (game-scene)
    ((tiled-map :initarg :tiled-map
                :initform (error ":tiled-map required")
                :reader tiled-map
                :documentation "Path to tiled map file.")
     (tile-size :initarg :tile-size
                :reader tile-size
                :initform (error ":tile-size required"))
     ;; set defaults for scene width and height, which will be resized when map is read
     (width :initform 1 :accessor width)
     (height :initform 1 :accessor height))
    (:documentation "A scene which reads from tiled json files to initialize itself."))
  (export 'tiled-map)
  (export 'tile-size))

(defmethod initialize-instance :after ((tiled-scene tiled-scene) &rest args)
  (declare (ignore args))
  (read-tiled-file tiled-scene))

@export
(defgeneric on-map-read (tiled-scene tiled-map-path map-num-cols map-num-rows map-tile-size)
  (:documentation "Invoked when a tiled tile is read. Implementers will resize TILED-SCENE to have enough space to fit the map."))

(progn
  @export
  (defstruct tileset
    "Internal representation of a tiled tileset file"
    (image-source (error ":image-source required"):type string)
    (columns (error ":columns required"):type integer)
    (tile-width (error ":tile-width required"):type integer)
    (tile-height (error ":tile-height required"):type integer))
  (export 'tileset-image-source)
  (export 'tileset-columns)
  (export 'tileset-tile-width)
  (export 'tileset-tile-height))

@export
(defgeneric on-tile-read (tiled-scene tileset tile-map-col tile-map-row tile-source-col tile-source-row)
  (:documentation "Invoked when a tiled tile is read. Implementers will add the appropriate game-object to TILED-SCENE."))

@export
(defun read-tiled-file (tiled-scene)
  (declare (tiled-scene tiled-scene))
  (labels ((tilemap-name-to-key (tilemap-name)
             (or (parse-integer tilemap-name :junk-allowed t)
                 (alexandria:make-keyword (string-upcase tilemap-name))))
           (json-val (json key)
             (cdr (assoc key json)))
           (read-tileset (tileset-path)
             (with-open-file (tileset-stream tileset-path)
               (let* ((json:*json-identifier-name-to-lisp* #'identity)
                      (json:*identifier-name-to-key* #'tilemap-name-to-key)
                      (json (json:decode-json tileset-stream))
                      (image-path (concatenate 'string
                                               (directory-namestring tileset-path)
                                               (json-val json :image))))
                 (make-tileset :image-source image-path
                               :columns (json-val json :columns)
                               :tile-width (json-val json :tilewidth)
                               :tile-height (json-val json :tileheight)))))
           (tileset-for-tile (tile-number tilesets-cache)
             "Returns values TILESET GID of the tileset for TILE-NUMBER"
             (loop :with active-gid = nil
                   :for tileset-gid :being :the hash-keys :of tilesets-cache :do
                     (when (>= tile-number tileset-gid)
                       (setf active-gid tileset-gid))
                   :finally (return (values (gethash active-gid tilesets-cache) active-gid)))))
    (with-slots ((tiled-map-path  tiled-map)) tiled-scene
      (with-open-file (stream tiled-map-path)
        (let* ((json:*json-identifier-name-to-lisp* #'identity)
               (json:*identifier-name-to-key* #'tilemap-name-to-key)
               (json (json:decode-json stream))
               (map-width-tiles (json-val json :width))
               (map-height-tiles (json-val json :height))
               (map-tile-width (json-val json :tilewidth))
               (map-orientation (json-val json :orientation))
               (map-render-order (json-val json :renderorder))
               (tilesets (make-hash-table :test #'equalp))
               (layers (json-val json :layers)))
          ;; assert tiled options are supported
          (unless (equalp "orthogonal" map-orientation)
            (error "Error reading ~A. orientation = ~A unsupported."
                   tiled-map-path
                   map-orientation))
          (unless (equalp "right-down" map-render-order)
            (error "Error reading ~A. render-order = ~A unsupported."
                   tiled-map-path
                   map-render-order))
          (on-map-read tiled-scene tiled-map-path map-width-tiles map-height-tiles map-tile-width)

          (loop :for tileset-json :in (json-val json :tilesets) :do
            (setf (gethash (json-val tileset-json :firstgid) tilesets)
                  (read-tileset (concatenate 'string
                                             (directory-namestring tiled-map-path)
                                             (json-val tileset-json :source)))))

          (loop :for layer-json :in layers :do
            (loop :for tile-number :in (json-val layer-json :data) :for i :from 0 :do
              (unless (= 0 tile-number)
                (multiple-value-bind (tileset gid) (tileset-for-tile tile-number tilesets)
                  (multiple-value-bind (map-row map-col) (floor i map-width-tiles)
                    (multiple-value-bind (source-row source-col) (floor (- tile-number gid) (tileset-columns tileset))
                      (on-tile-read tiled-scene
                                    tileset
                                    map-col
                                    map-row
                                    source-col
                                    source-row)))))))
          (values))))))
