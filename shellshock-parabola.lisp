;;;; shellshock-parabola.lisp

(in-package #:shellshock-parabola)

;;; "shellshock-parabola" goes here. Hacks and glory await!

(defvar *loop* 0.0)
(defvar *blending-params* (make-blending-params))
(defvar *quad* nil)
(defvar *quad-stream* nil)
(defparameter *radius-pixels* 334)
(defparameter *identity-height-pixels* 94)
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
(defun calc-transform (size)
  (setf *transform* (m4:* (m4:translation (v! (/ (- (x *max-size*)
                                                    (x size))
                                                 -1 (x *max-size*))
                                              (/ (- (y *max-size*)
                                                    (y size))
                                                 -1 (y *max-size*))
                                              0))
                          (m4:scale (v! (/ (x size) (x *max-size*))
                                        (/ (y size) (y *max-size*))
                                        1)))))

(defun round-to (n &optional (place 0) (func #'round))
  (if (= place 0)
      (let ((exp (expt 10 place)))
        (float (/ (funcall func (* n exp)) exp)))
      (funcall func n)))

(defun-g vert ((quad g-pt) &uniform (transform :mat4))
  (values (* transform (v! (pos quad) 1)) (s~ (pos quad) :xy)))
(defun-g main-curve-frag ((pos-2d :vec2) &uniform (color :vec3))
  (cond ((< (abs (- (x pos-2d)
                    (- (sqrt (/ (- 1 (y pos-2d)) 2)))))
            0.005)
         (v! color 1.0))
        ((< (abs (- (x pos-2d)
                    (sqrt (/ (- 1 (y pos-2d)) 2))))
            0.005)
         (v! color 1.0))
        (t (v! 0.5 0.5 0.5 0.0))))
(defun-g main-curve-frag-left ((pos-2d :vec2) &uniform (color :vec3))
  (cond ((< (abs (- (x pos-2d)
                    (- (sqrt (/ (- 1 (y pos-2d)) 2)))))
            0.005)
         (v! color 1.0))
        (t (v! 0.5 0.5 0.5 0.0))))
(defun-g main-curve-frag-right ((pos-2d :vec2) &uniform (color :vec3))
  (cond ((< (abs (- (x pos-2d)
                    (sqrt (/ (- 1 (y pos-2d)) 2))))
            0.005)
         (v! color 1.0))
        (t (v! 0.5 0.5 0.5 0.0))))

(defun-g line-frag ((pos-2d :vec2) &uniform (height :float) (color :vec3))
  (if (< (abs (- (y pos-2d) height)) 0.005)
      (v! color 1.0)
      (v! 0.0 0.0 0.0 0.0)))

(def-g-> prog-1 ()
  #'vert #'main-curve-frag)
(def-g-> prog-1-left ()
  #'vert #'main-curve-frag-left)
(def-g-> prog-1-right ()
  #'vert #'main-curve-frag-right)
(def-g-> prog-2 ()
  #'vert #'line-frag)

(defun step-demo ()
  (let ((now (get-internal-real-time)))
    (if (< (- now *last-time*) 1000/60)
        (sleep (/ (- 1000/60 (- now *last-time*)) 1000)))
    (setf *last-time* now))
  (step-host))

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
  (setf *last-time* (get-internal-real-time))
  (gl:depth-func :lequal)
  (skitter:whilst-listening-to
      ((#'window-size-callback (skitter:window 0) :size)
       (#'mouse-callback (skitter:mouse 0) :button)
       (#'mouse-callback (skitter:mouse 0) :pos))
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
      (setf *line-height* (- 1 (* 2 (/ mouse-y (y *size*))))))
    (update-interface)))


;;--------------------------------------------------------------
;; window

(defun radians->degrees (r)
  (* 180 (/ r pi)))
(defun degrees->radians (d)
  (/ (* d pi) 180))
(defun round-radians-as-degrees (r)
  (let ((deg (radians->degrees r)))
    (degrees->radians (round deg))))

(defun dist-height->power-angle (d h)
  (let* ((angle (atan (/ (* 4 h) d)))
         (low-power (* 35 (sqrt (/ d
                                   (sin (* 2 angle)) *radius-pixels*)))))
    (values low-power angle)))

(defun update-interface ()
  (let* ((factor 1.00056)
         (d (+ 0.00001 (* (x *size*) (sqrt (/ (- 1 *line-height*) 2)))))
         (h (+ 0.00001 (* (y *size*) (- 1 (/ (+ *line-height* 1) 2))))))
    (multiple-value-bind (low-power angle)
        (dist-height->power-angle d h)
      (let ((power (* low-power (expt factor low-power))))
        (print *size*)
        (print (round-to power))
        (print (round-to (radians->degrees angle)))
        (with-blending *blending-params*
          (clear)
          (map-g #'prog-1 *quad-stream* :color (v! 1.0 0.0 0.0) :transform (calc-transform *size*))
          (multiple-value-bind (low-power angle)
              (dist-height->power-angle (x *size*) (y *size*))
            (let* ((dist (* *radius-pixels* (expt (/ (round low-power) 35) 2) (sin (* 2 (round-radians-as-degrees angle)))))
                   (height (* (tan (round-radians-as-degrees angle)) dist 1/4))
                   (red-x-dist (- (x *size*) d))
                   (blue-x-dist (- dist (* dist (sqrt (/ (- 1 *line-height*) 2)))))
                   (gl-x-diff (/ (- red-x-dist blue-x-dist) 2.0 dist))
                   (gl-y-diff (/ (- (y *size*) height) 2 height))
                   (mirror-diff (* 2 (/ (- (x *size*) dist) dist))))
              (print (round power))
              (print mirror-diff)
              (map-g #'prog-1-right *quad-stream* :color (v! 0.0 0.0 1.0) :transform (m4:* (calc-transform (make-array 2 :initial-contents (list dist height)))
                                                                                     (m4:translation (v! gl-x-diff gl-y-diff 0.0))))
              (map-g #'prog-1-left *quad-stream* :color (v! 0.0 0.0 1.0) :transform (m4:* (calc-transform (make-array 2 :initial-contents (list dist height)))
                                                                                     (m4:translation (v! (- mirror-diff gl-x-diff) gl-y-diff 0.0))))))
          (map-g #'prog-2 *quad-stream* :height *line-height* :color (v! 0.0 1.0 0.0) :transform (calc-transform *size*))
          (swap))))))

(defun window-size-callback (event timestamp)
  (declare (ignore timestamp))
  (setf *size* (skitter:size-2d-vec event))
  (update-interface))
