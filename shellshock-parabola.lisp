;;;; shellshock-parabola.lisp

(in-package #:shellshock-parabola)

;;; "shellshock-parabola" goes here. Hacks and glory await!

(defvar *loop* 0.0)
(defvar *quad* nil)
(defvar *quad-stream* nil)
(defparameter *running* nil)
(defparameter *last-time* (get-internal-real-time))
(defparameter *size* #(2000 2000))
(defparameter *max-size* #(2000 2000))
(defparameter *line-height* 0.0)
(defparameter *transform* (m4:* (m4:scale (v! (/ (x *size*) (x *max-size*))
                                              (/ (y *size*) (y *max-size*))
                                              1))
                                (m4:translation (v! (/ (- (x *max-size*)
                                                          (x *size*))
                                                       2 (x *max-size*))
                                                    (/ (- (y *max-size*)
                                                          (y *size*))
                                                       2 (x *max-size*))
                                                    0))))
(defun update-transform ()
  (setf *transform* (m4:* (m4:translation (v! (/ (- (x *max-size*)
                                                    (x *size*))
                                                 -1 (x *max-size*))
                                              (/ (- (y *max-size*)
                                                    (y *size*))
                                                 -1 (y *max-size*))
                                              0))
                          (m4:scale (v! (/ (x *size*) (x *max-size*))
                                        (/ (y *size*) (y *max-size*))
                                        1))))
  )

(defun-g vert ((quad g-pt))
  (values (* *transform* (v! (pos quad) 1)) (s~ (pos quad) :xy)))
(defun-g frag ((pos-2d :vec2))
  (cond ((< (abs (- (y pos-2d)
                    (- 1 (* 2 (pow (x pos-2d) 2)))))
            0.005)
         (v! 1.0 0.0 0.0))
        ((< (abs (- (y pos-2d) *line-height*)) 0.005)
         (v! 0.0 1.0 0.0))
        (t (v! 0.5 0.5 0.5))))

(def-g-> prog-1 ()
  #'vert #'frag)

(defun step-demo ()
  (let ((now (get-internal-real-time)))
    (if (< (- now *last-time*) (/ 1000 30))
        (sleep (/ (- 1000/30 (- now *last-time*)) 1000)))
    (setf *last-time* now))
  (incf *loop* 0.01)
  (step-host)
  (clear)
  (map-g #'prog-1 *quad-stream*)
  (swap))

(defun run-loop ()
  (setf *running* t)
  (setf *quad* (make-gpu-array
                (list (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
                      (list (v! -1.0  -1.0 0 0) (v!  0.0   0.0))
                      (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
                      (list (v! -1.0   1.0 0 0) (v!  0.0   1.0))
                      (list (v!  1.0  -1.0 0 0) (v!  1.0   0.0))
                      (list (v!  1.0   1.0 0 0) (v!  1.0   1.0)))
                :element-type 'g-pt
                :dimensions 6))
  (setf *quad-stream* (make-buffer-stream *quad* :retain-arrays t))
  (skitter:whilst-listening-to
      ((#'window-size-callback (skitter:window 0) :size)
       (#'mouse-callback (skitter:mouse 0) :button))
    (loop :while (and *running* (not (shutting-down-p))) :do
      (continuable (step-demo)
        (livesupport:update-repl-link)))))

(defun stop-loop ()
  (setf *running* nil))

;;--------------------------------------------------------------
;; controls

(defun mouse-callback (event timestamp)
  (declare (ignore event timestamp))
  (when (skitter:mouse-down-p skitter.sdl2.mouse-buttons:mouse.left)
    (let ((mouse-y (y (skitter:xy-pos-vec (skitter:mouse-pos (skitter:mouse 0))))))
      (setf *line-height* (- 1 (* 2 (/ mouse-y (y *size*)))))
      (calc-values (x *size*) (y *size*)))))

;;--------------------------------------------------------------
;; window

(defun radians->degrees (r)
  (* 180 (/ r pi)))

(defun calc-values (d h)
  (let* ((factor 1.00056)
         (d (* d (sqrt (/ (- 1 *line-height*) 2))))
         (h (* h (- 1 (/ (+ *line-height* 1) 2))))
         (angle (atan (/ (* 4 h) d)))
         (short-distance (* 35 (sqrt (/ d
                                        (sin (* 2 angle)) 338))))
         )
    (print (round (* short-distance (expt factor short-distance))))
    (print (round (radians->degrees angle)))
     ))

(defun window-size-callback (event timestamp)
  (declare (ignore timestamp))
  (let* ((xy (skitter:size-2d-vec event))
         (d (aref xy 0))
         (h (aref xy 1)))
    (print xy)
    (calc-values d h)
    (setf *size* xy)
    (update-transform)))
