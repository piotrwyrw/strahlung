(defvar *screen-width* 300)
(defvar *screen-height* 300)
(defvar *focal-length* 0.1)

(defvar *img-stream* (open "output.ppm" :direction :output :if-exists :supersede))
(defvar *pixels* (make-array
	(list *screen-width* *screen-height*)
	:initial-element (list 0 0 0)))

(defvar *shading-functions* (make-hash-table))


;; Image utilities

(defun get-pixel (x y)
	(aref *pixels* x y))

(defun set-pixel (x y r g b)
	(setf (aref *pixels* x y) (list r g b)))

(defun i->xy (ix)
	(list
		(mod ix *screen-width*)
		(floor ix *screen-width*)))

(defun write-image ()
  	(format t "Writing pixels ...~&")
  	(write-line "P3" *img-stream*)
	(write-line (format nil "~a ~a" *screen-width* *screen-height*) *img-stream*)
	(write-line "255" *img-stream*)
	(dotimes (ix (* *screen-width* *screen-height*))
	  	(let*	((xy-pair (i->xy ix))
			(x (nth 0 xy-pair))
			(y (nth 1 xy-pair))
			(px (get-pixel x y)))
			(write-line
				(format nil "~a ~a ~a"
					(nth 0 px)
					(nth 1 px)
					(nth 2 px)) *img-stream*)))
	(format t "Done.~&"))

(defun close-stream ()
	(close *img-stream*))


;; Linear Algebra

(defclass vec3d ()
	((x	:initform 0
		:initarg :x
		:accessor vec-x)
	(y	:initform 0
		:initarg :y
		:accessor vec-y)
	(z	:initform 0
		:initarg :z
		:accessor vec-z)))

(defmethod vector-magnitude ((vec vec3d))
	(sqrt (+
		(expt (vec-x vec) 2)
		(expt (vec-y vec) 2)
		(expt (vec-z vec) 2))))

(defmethod vector-normalize ((vec vec3d))
	(let ((magnitude (vector-magnitude vec)))
	  	(if (/= magnitude 0.0)
			(progn
				(setf (vec-x vec) (/ (vec-x vec) magnitude))
				(setf (vec-y vec) (/ (vec-y vec) magnitude))
				(setf (vec-z vec) (/ (vec-z vec) magnitude))
				vec))))

(defclass ray ()
	((origin	:initarg :origin
			:accessor ray-origin)
	 (direction	:initarg :direction
			:accessor ray-direction)))

(defun screen-ray-direction (screen-x screen-y)
	(make-instance 'vec3d
		:x (+ -1.0 (* (/ 2.0 *screen-width*) screen-x))
		:y (- 1.0 (* (/ 2.0 *screen-height*) screen-y))
		:z *focal-length*))

(defun screen-ray (screen-x screen-y)
	(make-instance 'ray
		:origin (make-instance 'vec3d :x 0 :y 0 :z 0)
		:direction (vector-normalize (screen-ray-direction screen-x screen-y))))

(defclass shape () ())

(defclass sphere (shape)
	((center	:initarg :center
			:accessor sphere-center)
	 (radius	:initarg :radius
			:accessor sphere-radius)))

(defgeneric ray-vs-shape (ray shape))

(defclass intersect ()
	((shape		:initarg :shape
			:accessor intersect-shape)
	 (ray		:initarg :ray
			:accessor intersect-ray)
	 (point		:initarg :point
			:accessor intersect-point)
	 (distance	:initarg :distance
			:accessor intersect-distance
			:documentation "The distance along the ray to the intersection point")))

(defmethod ray-vs-shape ((ray ray) (shape sphere))
	(format t "Hello, World!~&")) ;; TODO Implement the intersection equation here

(defun trace-all-rays ()
	(dotimes (x *screen-width*)
		(dotimes (y *screen-height*)
			(let* ((ray (screen-ray x y)) (direction (ray-direction ray)))
				(set-pixel x y
					(+ 128 (floor (* 128 (vec-x direction))))
					(+ 128 (floor (* 128 (vec-y direction))))
					(+ 128 (floor (* 128 (vec-z direction)))))))))

(defun register-shader (id fun)
	(setf (gethash id *shading-functions*) fun))

(defun retrieve-shader-lambda (id)
	(gethash id *shading-functions*))

(defun call-shader (id _intersection)
	(funcall (retrieve-shader-lambda id) _intersection))

(register-shader 'binary-shader
	(lambda (_intersection)
		(if _intersection
			(list 255 255 255)
		(list 0 0 0))))

(call-shader 'binary-shader nil)

(format t "Rendering ...~&")
(trace-all-rays)
(write-image)
(close-stream)
