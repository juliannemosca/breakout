;;; This source was ported to "modern" Common-Lisp from the original
;;; source for the TI-Explorer.
;;;
;;; This version was written in 2020 by Julianne Mosca.
;;; The original file header is below for credit & reference.
;;;
;;; ==================================================================
;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:USER -*-

;;; File "BREAKOUT"
;;; Implements the classic (read: quite dull) video game.
;;; Written by Jamie Zawinski.
;;;
;;; ChangeLog:
;;;
;;; 20 Dec 88  Jamie Zawinski    Created.
;;;
;;; ==================================================================

;;(defpackage :breakout
;;  (:use :common-lisp)
;;  (:export
;;   #:breakout))

;;(in-package :breakout)

(defconstant +tv-sheet-inside-width+ 1024)
(defconstant +tv-sheet-inside-height+ 768)

(defparameter *game-state* nil)

(defclass window ()
  ()
  )

(defmethod draw-filled-rectangle ((w window) x y width height color)

  (unless x
    (setf x 0))
  (unless y
    (setf y 0))
  
  (sdl:draw-box (sdl:rectangle :x x :y y :w width :h height) :color color)
  )

(defclass breakout-window (window)
  ((speed :initform 0)
    (paddle-width :initform 100)
    (ball-x :initform 0)
    (ball-y :initform 0)
    (ball-dx :initform 0)
    (ball-dy :initform 0)
    (bricks :initform nil)
    (horizontal-bricks :initform 20)
    (vertical-bricks :initform 6)
    (brick-count :initform 0)
    (ball-size :initform 10)
    (paddle-height :initform 15)
    (brick-height :initform 20)
    (top-margin :initform 50)
    (bottom-margin :initform 150)
    (score :initform 0)
    (balls :initform 3)
    (demo-mode :initform nil)
    (accelerated-p :initform nil)
    ))

(defparameter *breakout-background-color* sdl:*blue*) ;;(sdl:color :r 0 :g 191 :b 255))
(defparameter *breakout-foreground-color* sdl:*white*)

(defparameter *breakout-brick-colors*
  (make-array 6 :initial-contents
    ;;(list W:PURPLE W:ORANGE W:75%-GRAY-COLOR W:50%-GRAY-COLOR W:33%-GRAY-COLOR W:WHITE))
    ;;(list sdl:*cyan* sdl:*magenta* sdl:*blue* sdl:*green* sdl:*yellow* sdl:*white*))
    (list
      (sdl:color :r 128 :g 0 :b 255)
      (sdl:color :r 255 :g 128 :b 0)
      (sdl:color :r 64 :g 64 :b 64)
      (sdl:color :r 128 :g 128 :b 128)
      (sdl:color :r 192 :g 192 :b 192)
      sdl:*white*))
  "The colors of the rows of bricks.")

(defmethod show-stats ((window breakout-window))
  (with-slots (balls speed score) window
    (sdl:draw-string-solid-*
      (format nil "Balls: ~D;  Speed: ~D;  Score: ~D." balls speed score) 10 10
      :justify :left :color *breakout-foreground-color*)))

(defmethod show-msg-box ((window breakout-window) msg &optional wider)
  (with-slots (balls speed score) window
    (let* (
            (xm (if wider 1 2))
            (wm (if wider 3 1))
            (x (* xm (floor (/ +tv-sheet-inside-width+ 5))))
            (y (* 2 (floor (/ +tv-sheet-inside-height+ 5))))
            (w (* wm (floor (/ +tv-sheet-inside-width+ 5))))
            (h (floor (/ +tv-sheet-inside-height+ 5))))

      (sdl:draw-box (sdl:rectangle :x x :y y :w w :h h) :color sdl:*white*)
      (sdl:draw-rectangle
        (sdl:rectangle :x (+ 10 x) :y (+ 10 y) :w (- w 20) :h (- h 20))
        :color *breakout-background-color*)

      (sdl:draw-string-solid-* msg
        (+ x (floor (/ w 2)))
        (+ y (floor (/ h 2)))
        :justify :center
        :color *breakout-background-color*)
      )))

(defmethod reset-bricks ((window breakout-window))
  (with-slots (bricks horizontal-bricks vertical-bricks brick-count) window
    (cond (bricks
            (dotimes (i (array-dimension bricks 0))
              (dotimes (j (array-dimension bricks 1))
                (let ((color (aref *breakout-brick-colors* j)))
                  (setf (aref bricks i j) color)))))
      (t
        (setq bricks (make-array (list horizontal-bricks vertical-bricks)
                       :element-type 'sdl:color :initial-element sdl:*white*))))
    (setq brick-count (* horizontal-bricks vertical-bricks))))

(defmethod erase-ball ((window breakout-window))
  (with-slots (ball-x ball-y ball-size) window
    (draw-filled-rectangle
      window
      ball-x ball-y
      ball-size
      ball-size
      *breakout-background-color* ;;*breakout-ball-offset* TV:ALU-SUB))
      )))

(defmethod draw-ball ((window breakout-window))
  (with-slots (ball-x ball-y ball-size) window
    ;;(format t "ball-x: ~D ball-y: ~D~%" ball-x ball-y)
    (draw-filled-rectangle
      window
      ball-x ball-y
      ball-size
      ball-size
      sdl:*white* ;;*breakout-ball-offset* TV:ALU-ADD))
      )))

(defmethod erase-paddle ((window breakout-window) x)
  (with-slots (bottom-margin paddle-height paddle-width) window
    (draw-filled-rectangle window
      x
      (- +tv-sheet-inside-height+
        (+ bottom-margin paddle-height))
      paddle-width
      paddle-height
      *breakout-background-color* ;; *breakout-paddle-offset* TV:ALU-SUB
      )))

(defmethod draw-paddle ((window breakout-window) x)
  (with-slots (bottom-margin paddle-height paddle-width) window
    (draw-filled-rectangle window
      x
      (- +tv-sheet-inside-height+
        (+ bottom-margin paddle-height))
      paddle-width
      paddle-height
      sdl:*white* ;; *breakout-paddle-offset* TV:ALU-ADD
      )))

(defmethod erase-brick ((window breakout-window) x y)
  (with-slots (top-margin brick-height horizontal-bricks) window
    (let* ((width (round +tv-sheet-inside-width+ horizontal-bricks))
            (real-x (* x width))
            (real-y (+ top-margin (* y brick-height))))
      (draw-filled-rectangle window
        real-x
        real-y
        width
        brick-height
        *breakout-background-color*))))

(defmethod draw-brick ((window breakout-window) x y color)
  ;;(break) ;; DEBUG!!!
  (with-slots (top-margin brick-height horizontal-bricks) window
    (let* ((width (round +tv-sheet-inside-width+ horizontal-bricks))
            (real-x (* x width))
            (real-y (+ top-margin (* y brick-height)))
            (gap 1))
      (draw-filled-rectangle window
        real-x
        real-y
        (- width gap)
        (- brick-height gap)
        color))))

(defmethod draw-all-bricks ((window breakout-window))
  (erase-ball window)
  (with-slots (bricks) window
    (dotimes (i (array-dimension bricks 0))
      (dotimes (j (array-dimension bricks 1))
        (let ((b (aref bricks i j)))
          (if (not (eq *breakout-background-color* b))
            (draw-brick window i j b)
            (erase-brick window i j)))))
    (draw-ball window)))

(defun breakout-beep (type)
  (declare (ignore type))
  ;;
  ;; sorry, no sound yet :( ... I could not yet
  ;; find a way for lispbuilder-sdl to open my audio
  ;; device... too bad!
  ;;
  ;; (ecase type
  ;;  (:PADDLE (tv:simple-beep 128 50))
  ;;  (:WALL   (tv:simple-beep 512 50))
  ;;  (:BRICK  (tv:simple-beep 1024 50)))
  )

(defun any-key-was-pressed ()
  (or
    (sdl:mouse-left-p)
    (sdl:mouse-middle-p)
    (sdl:mouse-right-p)
    (remove-if
      (lambda (x) (sdl:key= x :sdl-key-numlock))
      (sdl:keys-down-p))))

(defun pause-key-was-pressed ()
  (sdl:key-down-p :sdl-key-p))

(defun handle-paused-input (paused-input-state)
  (cond ((and
           (eq paused-input-state :pause-pressed)
           (not (pause-key-was-pressed)))
          :pause-pressed2)
    ((and
       (eq paused-input-state :pause-pressed2)
       (pause-key-was-pressed))
      :pause-pressed3)
    ((and
       (eq paused-input-state :pause-pressed3)
       (not (pause-key-was-pressed)))
      nil)
    (t paused-input-state)
    ))

(defmethod move-ball ((window breakout-window))
  (with-slots (accelerated-p
                demo-mode
                ball-x          ball-y
                ball-dx         ball-dy
                paddle-height   paddle-width
                top-margin      bottom-margin
                balls           bricks
                score           speed
                vertical-bricks horizontal-bricks
                brick-height    brick-count
                )
    window
    (let* ((dx (if accelerated-p (* ball-dx 2) ball-dx))
            (dy (if accelerated-p (* ball-dy 2) ball-dy))
            (new-x (+ ball-x dx))
            (new-y (+ ball-y dy))
            (paddle-x (sdl:mouse-x))
            (erase-brick-x nil)
            (erase-brick-y nil)
            )
      (cond ;;
        ;; In the danger zone.
        ;;
        ((and (plusp ball-dy)
           (<= (- +tv-sheet-inside-height+ (+ bottom-margin paddle-height))
             new-y))
          (cond ;;
            ;; A Hit!
            ;;
            ((or demo-mode
               (and (<= new-y (- +tv-sheet-inside-height+ bottom-margin))
                 (<= paddle-x new-x (+ paddle-x paddle-width))))
              (breakout-beep :paddle)
              (setq ball-dy (- ball-dy))
              (when (zerop brick-count)
                (reset-bricks window)
                (draw-all-bricks window)
                (setq speed (min 10 (1+ speed)))))
            ;;
            ;; A Miss!
            ;;
            ((> new-y +tv-sheet-inside-height+)
              (erase-ball window)
              (decf balls)
              (setf *game-state* :getting-ready)
              (throw 'MISS t))))
        ;;
        ;; Bounce off a wall.
        ;;
        ((<= new-x 0)
          (unless (plusp ball-dx)
            (breakout-beep :wall)
            (setq ball-dx (- ball-dx))))
        ((>= new-x +tv-sheet-inside-width+)
          (unless (minusp ball-dx)
            (breakout-beep :wall)
            (setq ball-dx (- ball-dx))))
        ((<= new-y 0)
          (unless (plusp ball-dy)
            (breakout-beep :wall)
            (setq ball-dy (- ball-dy))))
        ;;
        ;; In the brick zone.
        ;;
        ((<= (- top-margin brick-height)
           new-y
           (+ top-margin (* (1+ vertical-bricks) brick-height)))
          (let* ((max-x (array-dimension bricks 0))
                  (max-y (array-dimension bricks 1))
                  (brick-x (floor new-x (round +tv-sheet-inside-width+ horizontal-bricks)))
                  (brick-y (floor (- new-y top-margin) brick-height)))
            
            (cond #+COMMENT ((and (<= 0 brick-x (1- max-x))
                               (<= 0 brick-y (1- max-y))
                               (not (eq *breakout-background-color* (aref bricks brick-x brick-y))))
                              (setq erase-brick-x brick-x
                                erase-brick-y brick-y))

              ((and (minusp ball-dy)
                 (<= 0 brick-x      (1- max-x))
                 (<= 0 (1- brick-y) (1- max-y))
                 (not (eq *breakout-background-color* (aref bricks brick-x (1- brick-y)))))
                (setq erase-brick-x brick-x
                  erase-brick-y (1- brick-y)))

              ((and (plusp ball-dy)
                 (<= 0 brick-x      (1- max-x))
                 (<= 0 (1+ brick-y) (1- max-y))
                 (not (eq *breakout-background-color* (aref bricks brick-x (1+ brick-y)))))
                (setq erase-brick-x brick-x
                  erase-brick-y (1+ brick-y)))))

          (when erase-brick-x
            (setq ball-dy (- ball-dy))
            (setf (aref bricks erase-brick-x erase-brick-y) *breakout-background-color*))))

      ;;(erase-ball window)

      (when erase-brick-x
        (breakout-beep :brick)
        (erase-brick window erase-brick-x erase-brick-y)
        (incf score)
        (when (< erase-brick-y 2) (setq accelerated-p t))
        (decf brick-count))
      (setq ball-x new-x
        ball-y new-y)
      (draw-ball window))))

(defmethod drop-ball ((window breakout-window))
  (let* ((dx (- (random 5) 10)))
    (with-slots (ball-x ball-y ball-dx ball-dy speed accelerated-p) window
      (setq
        ball-x (round +tv-sheet-inside-width+ 2)
        ball-y (round +tv-sheet-inside-height+ 2)
        ball-dx (ceiling (* (1+ speed) dx) 2)
        ball-dy (ceiling (* (1+ speed) 5) 2)
        accelerated-p nil
        )))
  (draw-ball window))

(defmethod w-loop ((window breakout-window))
  (let* (
          (paddle-x nil)
          (ball-tick (get-internal-run-time))
          (input-state nil)
          )
    (setf *game-state* nil)
    (sdl:with-init ()
      (sdl:window
        +tv-sheet-inside-width+ +tv-sheet-inside-height+
        :fullscreen nil :title-caption "Breakout")
      (setf (sdl:frame-rate) 60)
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()

          (block GAME-OVER

            (with-slots (score balls) window

              (when (not *game-state*)

                (sdl:clear-display *breakout-background-color*)

                (setq score 0 balls 2)
                (reset-bricks window)
                (draw-all-bricks window)
                (show-stats window)
                (show-msg-box window "Any key to begin.")

                (sdl:update-display)

                (if (any-key-was-pressed)
                  (setf *game-state* :getting-ready)))

              ;;(break) ;; DEBUG!

              (catch 'MISS
                (when (or
                        (eq *game-state* :getting-ready)
                        (eq *game-state* :game-over))

                  (if (and
                        (eq *game-state* :game-over)
                        (any-key-was-pressed))
                    (setf input-state :key-pressed))

                  (when (and
                          (eq *game-state* :game-over)
                          (eq input-state :key-pressed)
                          (not (any-key-was-pressed)))
                    (setf input-state nil)
                    (setf *game-state* nil))

                  (when (eq *game-state* :getting-ready)

                    (when (minusp balls)
                      (show-msg-box window (format nil "GAME OVER!  ~D points." score))
                      (sdl:update-display)
                      (setf *game-state* :game-over)
                      (return-from GAME-OVER))

                    (show-msg-box window "Any key for next ball.")
                    (sdl:update-display)

                    (when (any-key-was-pressed)
                      (drop-ball window)
                      ;;(show-stats2 window)

                      (setf *game-state* :playing)))
                  )

                (when (eq *game-state* :paused)
                  (cond
                    ((eq input-state nil)
                      (setf *game-state* :playing))
                    (t (setf input-state (handle-paused-input
                                           input-state)))))

                (when (eq *game-state* :playing)
                  (sdl:clear-display *breakout-background-color*)

                  (draw-all-bricks window)
                  (show-stats window)
                  (draw-paddle window paddle-x)

                  ;;(setq paddle-x (min tv:mouse-x (- (tv:sheet-inside-width) paddle-width)))
                  (setf paddle-x (sdl:mouse-x))

                  ;; for some reason, re-drawing the whole screen
                  ;; is faster than doing the computations below
                  ;; and updating only the necessary... go figure :P
                  ;;
                  ;; (when (and old-paddle-x
                  ;;         (/= paddle-x old-paddle-x))
                  ;;   (erase-paddle window old-paddle-x))
                  ;; (when (or (null old-paddle-x)
                  ;;         (/= paddle-x old-paddle-x))
                  ;;   (draw-paddle window paddle-x))
                  ;;
                  ;; (setq old-paddle-x paddle-x)

                  (let* ((now (get-internal-run-time)))
                    (when (/= now ball-tick)
                      (move-ball window)
                      (setq ball-tick now)))

                  (when (pause-key-was-pressed)
                    (show-msg-box window "Paused. Press 'P' to continue." t)
                    (setf input-state :pause-pressed)
                    (setf *game-state* :paused))

                  (sdl:update-display)

                  )
                ) ;; end catch
              )
            )

          )))
    ))

(defmethod set-speed ((window breakout-window) n)
  (check-type n (integer 0 10))
  (with-slots (speed paddle-width) window
    (setq speed n)
    (setq paddle-width (+ 50 (* (- 10 n) 5)))
    ;;(when (tv:sheet-exposed-p self)
    ;;(send self :show-stats))
    ;;(show-stats window) ;; TODO: ??
    n
    )
  )

(defun breakout (&optional (speed 1) demo-p)
  "Play the world's dullest video game."
  (let* ((window (make-instance 'BREAKOUT-WINDOW)))
    (sdl:initialise-default-font)
    (set-speed window speed)
    (setf (slot-value window 'demo-mode) demo-p)
    (w-loop window)))
