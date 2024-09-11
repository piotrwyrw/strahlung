;;
;; S T R A H L U N G
;;

(defvar *screen-width* 300)
(defvar *screen-height* 300)
(defvar *focal-length* 1.0)

(defvar *img-stream* (open "output.ppm" :direction :output :if-exists :supersede))
(defvar *pixels* (make-array
	(list *screen-width* *screen-height*)
	:initial-element (list 0 0 0)))

(defvar *shading-functions* (make-hash-table))

(defconstant *epsilon* 0.0000001)

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

(defun vector-magnitude (vec)
	(sqrt (+
		(expt (vec-x vec) 2)
		(expt (vec-y vec) 2)
		(expt (vec-z vec) 2))))

(defun vector-normalize (vec)
	(let ((magnitude (vector-magnitude vec)))
	  	(if (/= magnitude 0.0)
			(progn
				(setf (vec-x vec) (/ (vec-x vec) magnitude))
				(setf (vec-y vec) (/ (vec-y vec) magnitude))
				(setf (vec-z vec) (/ (vec-z vec) magnitude))
				vec))))

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

(defun point-along-ray (ray distance)
	(vector-add
		(vector-mul (ray-direction ray) distance)
		(ray-origin ray)))

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

		(Cx (vec-z C))
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
			(square Oy)))

		(dst (quadratic-solve a b c)))

		(if dst
			(make-instance 'intersect :shape shape :ray ray :point (point-along-ray ray (reduce #'min dst)) :distance dst)
		nil)))

(defvar sample-sphere (make-instance 'sphere
			:center (make-instance 'vec3d
					:x 0.0
					:y 0.0
					:z -2.0)
			:radius 0.5))

(defun trace-all-rays ()
	(dotimes (x *screen-width*)
		(dotimes (y *screen-height*)
			(let* ((ray (screen-ray x y)) (direction (ray-direction ray)) (inter (ray-vs-shape ray sample-sphere)))
					(if inter
						(set-pixel x y 255 255 255)
					(set-pixel x y 0 0 0))))))

(defun register-shader (id fun)
	(setf (gethash id *shading-functions*) fun))

(defun retrieve-shader-lambda (id)
	(gethash id *shading-functions*))

(defun call-shader (id _intersection)
	(funcall (retrieve-shader-lambda id) _intersection))

(defmacro defshader (name interId _lambda)
	`(register-shader ,name (lambda (,interId) ,_lambda)))

(defshader 'hello-world-shader inter
	(format t "~a~&" inter))

(call-shader 'hello-world-shader "Hello, World!")

(format t "Rendering ...~&")
(trace-all-rays)
(write-image)
(close-stream)
