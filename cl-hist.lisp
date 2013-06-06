

(defpackage #:cl-hist
  (:use #:cl)  
  (:export #:hist
		   #:hist-2d
		   #:make-hist
		   #:make-hist-2d
		   #:update-hist
		   #:update-hist-2d
		   #:reset-hist
		   #:reset-hist-2d
		   #:output-hist
		   #:output-hist-2d
		   #:maphist
		   #:maphist*
		   #:maphist-2d
		   #:maphist-2d*))

(in-package #:cl-hist)
			
(defclass hist ()
  ((data :accessor hist-data :initarg :data)
   (start :reader hist-start :initarg :start)
   (end :reader hist-end :initarg :end)
   (n :reader hist-num :initarg :n)
   (step :reader hist-step :initarg :step)
   (counter :accessor hist-counter :initform 0)))

(defclass hist-2d ()
  ((data :accessor hist-data :initarg :data)
   
   (start1 :reader hist-start1 :initarg :start1)
   (end1 :reader hist-end1 :initarg :end1)
   (n1 :reader hist-num1 :initarg :n1)
   (step1 :reader hist-step1 :initarg :step1)

   (start2 :accessor hist-start2 :initarg :start2)
   (end2 :accessor hist-end2 :initarg :end2)
   (step2 :accessor hist-step2 :initarg :step2)
   (n2 :accessor hist-num2 :initarg :n2)

   (counter :accessor hist-counter :initform 0.0)))

(defmethod print-object ((h hist) stream)
  (print-unreadable-object (h stream)
	(format stream "HIST :START ~A :END ~A :NUM ~A"
			(hist-start h) (hist-end h) (hist-num h))))

(defmethod print-object ((h hist-2d) stream)
  (print-unreadable-object (h stream)
	(format stream "HIST-2D :START1 ~A :END1 ~A :NUM1 ~A :START2 ~A :END2 ~A :NUM2 ~A"
			(hist-start1 h) (hist-end1 h) (hist-num1 h)
			(hist-start2 h) (hist-end2 h) (hist-num2 h))))

(defun make-hist (start end num)
  (make-instance 'hist
				 :data (make-array num :initial-element 0.0)
				 :start start
				 :end end
				 :n num
				 :step (coerce (/ (- end start) num) 'float)))

(defun make-hist-2d (start1 end1 num1 start2 end2 num2)
  (make-instance 'hist-2d
				 :data (make-array (list num1 num2) :initial-element 0.0)
				 :start1 start1
				 :end1 end1
				 :n1 num1
				 :step1 (coerce (/ (- end1 start1) num1) 'float)
				 
				 :start2 start2
				 :end2 end2
				 :n2 num2
				 :step2 (coerce (/ (- end2 start2) num2) 'float)))

(defun update-hist (h val &optional (amount 1))
  (let ((p (floor (- val (hist-start h))
				  (hist-step h))))
	(if (and (>= p 0) (< p (hist-num h)))
		(progn
		  (incf (svref (hist-data h) p)
				amount)
		  (incf (hist-counter h) amount)
		  t))))

(defun update-hist-2d (h val1 val2 &optional (amount 1))
  (let ((p1 (floor (- val1 (hist-start1 h))
				  (hist-step1 h)))
		(p2 (floor (- val2 (hist-start2 h))
				   (hist-step2 h))))
	(if (and (>= p1 0) (< p1 (hist-num1 h))
			 (>= p2 0) (< p2 (hist-num2 h)))
		(progn
		  (incf (aref (hist-data h) p1 p2)
				amount)
		  (incf (hist-counter h) amount)
		  t))))

(defun output-hist (h stream)
  (dotimes (i (hist-num h))
	(let ((x (+ (hist-start h)
				(* (hist-step h) i))))
	  (format stream "~A ~A ~A~%"
			  x
			  (svref (hist-data h) i)
			  (/ (svref (hist-data h) i)
				 (* (hist-step h) (hist-counter h)))))))

(defun output-hist-2d (h stream)
  (dotimes (i (hist-num1 h))
	(let ((x (+ (hist-start1 h)
				(* (hist-step1 h) i))))
	  (dotimes (j (hist-num2 h))
		(let ((y (+ (hist-start2 h)
					(* (hist-step2 h) j))))
		  (format stream "~A ~A ~A ~A~%"
				  x y
				  (aref (hist-data h) i j)
				  (/ (svref (hist-data h) i)
					 (* (hist-step1 h) (hist-step2 h) (hist-counter h)))))))))

(defun reset-hist (h)
  (setf (hist-counter h) 0)
  (dotimes (i (hist-num h))
	(setf (svref (hist-data h) i) 0)))

(defun reset-hist-2d (h)
  (setf (hist-counter h) 0)
  (dotimes (i (hist-num1 h))
	(dotimes (j (hist-num2 h))
	  (setf (aref (hist-data h) i j) 0))))

(defun maphist (function hist)
  "Apply FUNCTION to the values in the histogram. Return a new histogram with these as the values."
  (let ((h (make-hist (hist-start hist)
					  (hist-end hist)
					  (hist-num hist))))
	(dotimes (i (hist-num hist))
	  (let ((val (funcall function (svref (hist-data hist) i))))
		(setf (svref (hist-data h) i) val)
		(incf (hist-counter h) val)))
	h))

(defun maphist-2d (function hist)
  (let ((h (make-hist-2d (hist-start1 hist)
						 (hist-end1 hist)
						 (hist-num1 hist)
						 (hist-start2 hist)
						 (hist-end2 hist)
						 (hist-num2 hist))))
	(dotimes (i (hist-num1 hist))
	  (dotimes (j (hist-num2 hist))
		(let ((val (funcall function (aref (hist-data hist) i j))))
		  (setf (aref (hist-data h) i j) val)
		  (incf (hist-counter h) val))))
	h))

(defun maphist* (function hist)
  "Apply FUNCTION to the values in the histogram. Return hist"
  (dotimes (i (hist-num hist))
	(funcall function (svref (hist-data hist) i)))
  hist)

(defun maphist-2d* (function hist)
  (dotimes (i (hist-num1 hist))
	(dotimes (j (hist-num2 hist))
	  (funcall function (aref (hist-data hist) i j))))
  hist)




