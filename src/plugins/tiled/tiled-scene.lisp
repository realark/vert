(in-package :recurse.vert)

(progn
  @export
  (defclass tiled-scene (game-scene)
    ((tiled-map :initarg :tiled-map
                :initform (error ":tiled-map required")
                :reader tiled-map
                :documentation "Path to tiled map file.")
     ;; set defaults for scene width and height, which will be resized when map is read
     (width :initform 1 :accessor width)
     (height :initform 1 :accessor height))
    (:documentation "A scene which reads from tiled json files to initialize itself."))
  (export 'tiled-map)
  (export 'tile-size))

(defmethod initialize-instance :after ((tiled-scene tiled-scene) &rest args)
  (declare (ignore args))
  (unless (null (slot-value tiled-scene 'tiled-map))
    (let ((t0 (ticks)))
      (read-tiled-file tiled-scene)
      (log:info "read tiled file in ~Ams" (- (ticks) t0)))
    (with-slots (spatial-partition) tiled-scene
      (let ((t0 (ticks))
            (new-partition (make-instance 'quadtree
                                          :initial-size (vector2
                                                         (* 2.0 (width tiled-scene))
                                                         (* 2.0 (height tiled-scene))))))
        ;; map size likely changed. Easiest to just use a new spatial partition
        (do-spatial-partition (object spatial-partition :static-iteration-p t)
          (start-tracking new-partition object))
        (partition-clear spatial-partition)
        (setf spatial-partition new-partition)
        (log:info "rebuilt partition in ~Ams" (- (ticks) t0))))))

(progn
  @export
  (defstruct tileset
    "Internal representation of a tiled tileset file"
    (image-source (error ":image-source required"):type string)
    (columns (error ":columns required"):type integer)
    (tile-width (error ":tile-width required"):type integer)
    (tile-height (error ":tile-height required"):type integer)
    (tile-objects ; hash TILE-ID -> Objects json
     (make-hash-table :test #'equalp)
     :type hash-table)
    (tile-properties ; hash TILE-ID -> (hash :prop-name -> "prop-val")
     (make-hash-table :test #'equalp)
     :type hash-table))
  (export '(tileset-image-source
            tileset-columns
            tileset-tile-width
            tileset-tile-height
            tileset-tile-objects
            tileset-tile-properties)))

@export
(defun tileset-source-row-col (tileset tile-id)
  "Return (values source-row source-col) for TILE-ID in TILESET"
  (floor tile-id (tileset-columns tileset)))

(progn
  @export
  (defstruct (tiled-object (:constructor %make-tiled-object))
    "An object from a tiled object layer"
    (props nil :type list))
  (export 'tiled-object-props)

  @export
  (defun make-tiled-object (&key props)
    (when (assoc :gid props)
      ;; tiled uses a y == up coordinate system for image-objects.
      ;; convert the y prop to its vert equivalent
      (setf (cdr (assoc :y props))
            (- (cdr (assoc :y props))
               (cdr (assoc :height props)))))
    (%make-tiled-object :props props)))

@export
(defgeneric on-map-read (tiled-scene tiled-map-path map-num-cols map-num-rows map-tile-width map-tile-height)
  (:documentation "Invoked when a tiled tile is read. Implementers will resize TILED-SCENE to have enough space to fit the map."))


@export
(defgeneric on-tile-read (tiled-scene layer-json tileset tile-number tile-map-col tile-map-row tile-source-col tile-source-row source-tile-id)
  (:documentation "Invoked when a tiled tile is read. Implementers will add the appropriate game-object to TILED-SCENE."))

@export
(defgeneric on-object-read (tiled-scene object)
  (:documentation "Invoked when an object is read from a tiled object layer. Implementers will add the object to the tiled scene in the best game-specific way."))

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
                                               (json-val json :image)))
                      (tile-properties (make-hash-table :test #'equalp))
                      (tile-objects-hash (make-hash-table :test #'equalp)))
                 (loop :for tile-props :in (json-val json :tileproperties) :do
                      (when (> (length tile-props) 1)
                        (let ((tile-id (first tile-props))
                              (prop-hash (make-hash-table :test #'equalp)))
                          (loop :for prop :in (rest tile-props) :do
                               (let ((prop-key (car prop))
                                     (prop-val (cdr prop)))
                                 (setf (gethash prop-key prop-hash)
                                       prop-val)))
                          ;; add to tile-properties hash
                          (setf (gethash tile-id tile-properties)
                                prop-hash))))
                 (loop :for tile-json :in (json-val json :tiles) :do
                      ;; read object group for object collisions
                      (when (and (listp tile-json)
                                 (json-val (cdr tile-json) :objectgroup)
                                 (json-val (json-val (cdr tile-json) :objectgroup) :objects))
                        (loop :with tile-id = (car tile-json)
                           :for tile-object-json
                           :in (json-val (json-val (cdr tile-json) :objectgroup) :objects)
                           :do
                             (unless (gethash tile-id tile-objects-hash)
                               (setf (gethash tile-id tile-objects-hash) (make-array 1 :fill-pointer 0 :adjustable t)))
                             (let ((tile-objects-array (gethash tile-id tile-objects-hash)))
                               (vector-push-extend
                                tile-object-json
                                tile-objects-array)))))
                 (make-tileset :image-source image-path
                               :columns (json-val json :columns)
                               :tile-width (json-val json :tilewidth)
                               :tile-height (json-val json :tileheight)
                               :tile-objects tile-objects-hash
                               :tile-properties tile-properties))))
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
               (map-num-cols (json-val json :width))
               (map-num-rows (json-val json :height))
               (map-tile-width (json-val json :tilewidth))
               (map-tile-height (json-val json :tileheight))
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
          (on-map-read tiled-scene tiled-map-path map-num-cols map-num-rows map-tile-width map-tile-height)

          (loop :for tileset-json :in (json-val json :tilesets) :do
            (setf (gethash (json-val tileset-json :firstgid) tilesets)
                  (read-tileset (concatenate 'string
                                             (directory-namestring tiled-map-path)
                                             (json-val tileset-json :source)))))

          (loop :for layer-json :in layers :do
            (alexandria:switch ((json-val layer-json :type) :test #'equalp)
              ("tilelayer"
               (loop :for tile-number :in (json-val layer-json :data) :for i :from 0 :do
                 (unless (= 0 tile-number)
                   (multiple-value-bind (tileset gid) (tileset-for-tile tile-number tilesets)
                     (multiple-value-bind (map-row map-col) (floor i map-num-cols)
                       (let ((source-tile-id (- tile-number gid)))
                         (multiple-value-bind (source-row source-col) (tileset-source-row-col tileset source-tile-id)
                           (on-tile-read tiled-scene
                                         layer-json
                                         tileset
                                         (- tile-number gid)
                                         map-col
                                         map-row
                                         source-tile-id
                                         source-col
                                         source-row))))))))
              ("objectgroup"
               (loop :for object-json :in (json-val layer-json :objects):do
                 (on-object-read tiled-scene (make-tiled-object :props object-json))))))
          (values))))))
