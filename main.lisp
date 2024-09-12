;;
;; S T R A H L U N G
;;

(defpackage :strahlung
	(:use :cl))

(in-package :strahlung)

(defvar *screen-width* 500)
(defvar *screen-height* 500)
(defvar *focal-length* 3.0)

(defvar *pixels* (make-array
	(list *screen-width* *screen-height*)
	:initial-element (list 0 0 0)))

(defvar *shading-functions* (make-hash-table))

(defvar *shapes* nil)

;; This is used to add a bit of noise to the scene
(defvar *ray-variance* 0.0025)

(defun add-shape (shape)
	(setf *shapes* (append *shapes* shape)))

(defvar *epsilon* 0.000001)

;; Image utilities

(defun get-pixel (x y)
	(aref *pixels* x y))

(defun set-pixel (x y rgb-list)
	(setf (aref *pixels* x y) rgb-list))

(defun rgb-average (rgb-1 rgb-2)
	(list
		(floor (+ (nth 0 rgb-1) (nth 0 rgb-2) 2))
		(floor (+ (nth 1 rgb-1) (nth 1 rgb-2) 2))
		(floor (+ (nth 2 rgb-1) (nth 2 rgb-2) 2))))

(defun i->xy (ix)
	(list
		(mod ix *screen-width*)
		(floor ix *screen-width*)))

(defun close-stream (str)
	(close str))

(defun write-image ()
  	(format t "Preparing file for writing ...~&")
	(let ((img-stream (open "output.ppm" :direction :output :if-exists :supersede)))
  		(format t "Writing pixels ...~&")
  		(write-line "P3" img-stream)
		(write-line (format nil "~a ~a" *screen-width* *screen-height*) img-stream)
		(write-line "255" img-stream)
		(dotimes (ix (* *screen-width* *screen-height*))
	  		(let*	((xy-pair (i->xy ix))
				(x (nth 0 xy-pair))
				(y (nth 1 xy-pair))
				(px (get-pixel x y)))
				(write-line
					(format nil "~a ~a ~a"
						(nth 0 px)
						(nth 1 px)
						(nth 2 px)) img-stream)))
		(format t "Closing file ...~&")
		(close-stream img-stream))
	(format t "Done.~&"))

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

(defun vector-magnitude (vec)
	(sqrt (+
		(expt (vec-x vec) 2)
		(expt (vec-y vec) 2)
		(expt (vec-z vec) 2))))

(defun vector-normalize (vec)
	(let ((magnitude (vector-magnitude vec)))
	  	(if (/= magnitude 0.0)
			(make-instance 'vec3d
				:x (/ (vec-x vec) magnitude)
				:y (/ (vec-y vec) magnitude)
				:z (/ (vec-z vec) magnitude))
		vec)))

(defun vector-mul (vec d)
	(make-instance 'vec3d
			:x (* (vec-x vec) d)
			:y (* (vec-y vec) d)
			:z (* (vec-z vec) d)))

(defun vector-add (vec another)
	(make-instance 'vec3d
			:x (+ (vec-x vec) (vec-x another))
			:y (+ (vec-y vec) (vec-y another))
			:z (+ (vec-z vec) (vec-z another))))

(defun vector-sub (vec another)
	(make-instance 'vec3d
			:x (- (vec-x vec) (vec-x another))
			:y (- (vec-y vec) (vec-y another))
			:z (- (vec-z vec) (vec-z another))))



(defun vector-color (vec)
	(let ((norm (vector-normalize vec)))
		(list
			(floor (+ 128 (* 128 (vec-x norm))))
			(floor (+ 128 (* 128 (vec-y norm))))
			(floor (+ 128 (* 128 (vec-z norm)))))))

(defclass ray ()
	((origin	:initarg :origin
			:accessor ray-origin)
	 (direction	:initarg :direction
			:accessor ray-direction)))

(defun variance-vector ()
	(make-instance 'vec3d
		:x (random *ray-variance*)
		:y (random *ray-variance*)
		:z 0)) ;; The ray variance should have no effect on the Z axis

(defun varied-vector (original)
	(vector-normalize
		(vector-add original
			(variance-vector))))

(defun screen-ray-direction (screen-x screen-y)
	(varied-vector
		(make-instance 'vec3d
			:x (+ -1.0 (* (/ 2.0 *screen-width*) screen-x))
			:y (- 1.0 (* (/ 2.0 *screen-height*) screen-y))
			:z *focal-length*)))

(defun screen-ray (screen-x screen-y)
	(make-instance 'ray
		:origin (make-instance 'vec3d :x 0 :y 0 :z 0)
		:direction (screen-ray-direction screen-x screen-y)))

(defun point-along-ray (ray distance)
	(vector-add
		(vector-mul (ray-direction ray) distance)
		(ray-origin ray)))

(defclass shape () ())

(defclass sphere (shape)
	((center	:initarg :center
			:accessor sphere-center)
	 (radius	:initarg :radius
			:accessor sphere-radius)
	 (shader	:initarg :shader
			:accessor sphere-shader)))

(defgeneric ray-vs-shape (ray shape))
(defgeneric shape-normal (shape point))

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

(defmacro square (v)
	"Squares a given expression"
	`(* ,v ,v))

(defun quadratic-solve (a b c)
	"Solve the quadratic equation and return a list of results or NIL"
	(let* ((quad-disc (- (square b) (* 4 a c))) (result nil) (quad-denominator (* 2 a)) (tmp-res 0))
	  	(cond	((>=	quad-disc 0) (progn
						(setf tmp-res (/ (+ (* -1 b) (sqrt quad-disc)) quad-denominator))
						(if (> tmp-res *epsilon*)
							(setf result (append result (list tmp-res)))))))
		(cond	((>	quad-disc 0) (progn
						(setf tmp-res (/ (- (* -1 b) (sqrt quad-disc)) quad-denominator))
						(if (> tmp-res *epsilon*)
							(setf result (append result (list tmp-res)))))))
		result))

(defmethod ray-vs-shape ((ray ray) (shape sphere))
  	"Find the intersection between a ray and a shape. Returns an instance of an intersection"
	(let* (	(O (ray-origin ray))
	       	(D (ray-direction ray))
		(C (sphere-center shape))

		(Ox (vec-x O))
		(Oy (vec-y O))
		(Oz (vec-z O))

		(Cx (vec-x C))
		(Cy (vec-y C))
		(Cz (vec-z C))

		(Dx (vec-x D))
		(Dy (vec-y D))
		(Dz (vec-z D))

		(r (sphere-radius shape))

		(a (+ (square Dx) (square Dy) (square Dz)))

		(b (+	(* -2 Cx Dx)
			(* -2 Cy Dy)
			(* -2 Cz Dz)
			(* 2 Dx Ox)
			(* 2 Dy Oy)
			(* 2 Dz Oz)))

	  	(c (+	(square Oz)
			(* -2 Cz Oz)
			(square Cx)
			(* -2 Cx Ox)
			(square Cy)
			(* -2 Cy Oy)
			(square Cz)
			(square Ox)
			(square Oy)
			(* -1 (square r))))

		(dst (quadratic-solve a b c)))

		(if dst
		  	(progn 
				(make-instance 'intersect :shape shape :ray ray
					:point (point-along-ray ray
						(reduce #'min dst))
					:distance dst))
		nil)))

(defmethod shape-normal ((shape sphere) (point vec3d))
	(vector-normalize
	  	(vector-sub
			(sphere-center shape)
			point)))

(defun register-shader (id fun)
	(setf (gethash id *shading-functions*) fun))

(defun retrieve-shader-lambda (id)
	(gethash id *shading-functions*))

(defun call-shader (id _intersection)
	(funcall (retrieve-shader-lambda id) _intersection))

(defmacro defshader (name interId _lambda)
	`(register-shader ,name (lambda (,interId) ,_lambda)))

(defshader 'normal-shader inter
	(vector-color
		(shape-normal
			(intersect-shape inter)
			(intersect-point inter))))

(defvar sample-sphere (make-instance 'sphere
			:center (make-instance 'vec3d
					:x 0.0
					:y 0.0
					:z 20.0)
			:radius 5.0
			:shader 'normal-shader))

(defun trace-all-rays ()
	(dotimes (x *screen-width*)
		(dotimes (y *screen-height*)
			(let* ((ray (screen-ray x y)) (inter (ray-vs-shape ray sample-sphere)))
					(if inter
						(set-pixel x y (call-shader (sphere-shader sample-sphere) inter))
					(set-pixel x y (list 0 0 0)))))))

(defun main()
	(format t "Rendering ...~&")
	(trace-all-rays)
	(write-image)
	(sb-ext:quit))
