(defparameter *screen-width* 1000)
(defparameter *screen-height* 700)
(defparameter *focal-length* 2)

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

(defun screen-ray-direction (screen-x screen-y)
	(make-instance 'vec3d
		:x (+ -1.0 (* (/ 2.0 *screen-width*) screen-x))
		:y (- 1.0 (* (/ 2.0 *screen-height*) screen-y))
		:z *focal-length*))

(defclass ray ()
	((origin	:initarg :origin
			:accessor ray-origin)
	 (direction	:initarg :direction
			:accessor ray-direction)))

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
	(print "Hello, World!")) ;; TODO Implement the intersection equation here

(defvar sample-sphere (make-instance 'sphere :center (screen-ray-direction 3 2) :radius 2.3))
(defvar sample-ray (make-instance 'ray :origin (screen-ray-direction 3 2) :direction (screen-ray-direction 5 3)))

(ray-vs-shape
	sample-ray
	sample-sphere)

