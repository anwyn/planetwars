;;; simulation.lisp --- Google AI competition 2010 game engine
;;;      _                 _       _   _
;;;  ___(_)_ __ ___  _   _| | __ _| |_(_) ___  _ __
;;; / __| | '_ ` _ \| | | | |/ _` | __| |/ _ \| '_ \
;;; \__ \ | | | | | | |_| | | (_| | |_| | (_) | | | |
;;; |___/_|_| |_| |_|\__,_|_|\__,_|\__|_|\___/|_| |_|

;;;
;;; Copyright (C) 2010 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :planetwars)

(defvar *log-output* *error-output*
  "Output stream for the engine.")

(defun copy-world (world &optional (player1 1))
  "Copy the game world. Current bot will always be player nr. 1"
  (flet ((adjust-owner (owner)
           (cond ((= owner 1) player1)
                 ((= owner player1) 1)
                 (t owner))))
    (make-instance 'world
                   :planets (loop :for p :in (planets world)
                                  :collect (make-instance 'planet
                                                :id (id p)
                                                :owner (adjust-owner (owner p))
                                                :ships (ships p)
                                                :x (x p) :y (y p)
                                                :growth (growth p)))
                   :fleets (loop :for f :in (fleets world)
                                 :collect (make-instance 'fleet
                                               :id (id f)
                                               :owner (adjust-owner (owner f))
                                               :ships (ships f)
                                               :source (id (source f))
                                               :destination (id (destination f))
                                               :trip-length (trip-length f)
                                               :turns-remaining (turns-remaining f))))))

(defun planets-under-attack (world)
  "Selector for planets having arriving fleets."
  (remove-if-not (lambda (p)
                   (some #'arrived? (incoming-fleets p (fleets world))))
                 (planets world)))

(defun fight-battles (world)
  (dolist (planet (planets-under-attack world))
    (let ((fleets (remove-if-not #'arrived?
                                 (incoming-fleets planet (fleets world)))))

      (dolist (nr '(1 2))
        (let ((arrivals (owned-by nr fleets)))
          (when arrivals
            (format *log-output* "Player ~D's ~D ships arrive at player ~D's planet ~D, home to ~D ships.~%"
                    nr (reduce #'+ arrivals :key #'ships) (owner planet) (id planet) (ships planet)))))

      (invasion planet fleets))))

;;; this can surely be done better...
(defun invasion (planet fleets)
  "Attack the planet with fleets."
  (let* ((owner (owner planet))
         (homeforce (cons owner (+ (ships planet)
                                   (reduce #'+ (owned-by owner fleets) :key #'ships)))))
    (if (neutral? planet)
        (let* ((force1 (cons 1 (reduce #'+ (owned-by 1 fleets) :key #'ships)))
               (force2 (cons 2 (reduce #'+ (owned-by 2 fleets) :key #'ships)))
               (forces (sort (list homeforce force1 force2) #'> :key #'cdr))
               (biggestforce (first forces))
               (secondforce (second forces)))

          (if (= (cdr biggestforce) (cdr secondforce))
              (setf (ships planet) 0)
              (progn
                (format *log-output*
                        "Player ~D captures neutral planet ~D.~%"
                        (car biggestforce) (id planet))

                (setf (owner planet) (car biggestforce)
                      (ships planet) (- (cdr biggestforce) (cdr secondforce))))))

        (let ((enemyforce (cons (if (= owner 1) 2 1)
                                (reduce #'+ (not-owned-by owner fleets) :key #'ships))))
          (setf (ships planet) (- (cdr homeforce) (cdr enemyforce)))

          (if (< (ships planet) 0)
              (progn
                (format *log-output*
                        "Player ~D captures planet ~D from player ~D.~%"
                        (car enemyforce) (id planet) (car homeforce))

                (setf (owner planet) (car enemyforce)
                      (ships planet) (abs (ships planet))))
              (format *log-output*
                      "Player ~D attacks planet ~D but fails.~%"
                      (car enemyforce) (id planet)))))))


(defun advance-fleets (world)
  (dolist (fleet (fleets world))
    (decf (turns-remaining fleet))))

(defun remove-arrived-fleets (world)
  (setf (fleets world)
        (remove-if #'arrived? (fleets world))))

(defun grow-planets (world)
  (dolist (p (remove-if #'neutral? (planets world)))
    (incf (ships p) (growth p))))

(defun execute-order (world player order)
  "Execute a single ORDER from a PLAYER."
  (destructuring-bind (source destination num-ships) order
    (let* ((src (elt (planets world) source))
           (dst (elt (planets world) destination))
           (dist (distance src dst)))
      (assert (not (eq src dst)))
      (assert (> num-ships 0))
      (assert (<= num-ships (ships src)))
      (assert (= (owner src) player))
      (decf (ships src) num-ships)

      (format *log-output*
              "Player ~D sends ~D ships from planet ~D to planet ~D owned by player ~D. Arrival in ~D turns.~%"
              player num-ships source destination (owner dst) dist)

      (push (make-instance 'fleet
                           :id (length (fleets world))
                           :owner player
                           :ships num-ships
                           :source src
                           :destination dst
                           :trip-length dist
                           :turns-remaining dist)
            (fleets world)))))

(defun simulate (world &rest players)
  "Run one simulation step."
  ;; first run bots on copies of the world,
  ;; then execute their orders, which side-effect the world
  (let ((orderlist (loop :for player :in players
                         :for player-nr :upfrom 1
                         :collect (run-bot player (copy-world world player-nr)))))
    (loop :for orders :in orderlist
          :for player-nr :upfrom 1
          :do (dolist (order orders)
                (execute-order world player-nr order))))

  ;; move fleets forward one turn
  (advance-fleets world)

  ;; add ship production to player led planets
  (grow-planets world)

  ;; fight over planets
  (fight-battles world)

  ;; retire veteran fleets
  (remove-arrived-fleets world))


(defun game-over? (world turn maxturns)
  "Test end conditions of game.
    * The turn limit is reached. The winner is the player with the most ships, both on planets and in fleets. If both players have the same number of ships, it’s a draw.
    * One player runs out of ships entirely. The winner is the other player.
    * Both players run out of ships at the same time. The game is a draw.
"
  (let* ((planets (planets world))
         (fleets (fleets world))
         (entities (append fleets planets))
         (p1-ships (reduce #'+ (owned-by 1 entities) :key #'ships))
         (p2-ships (reduce #'+ (owned-by 2 entities) :key #'ships)))
    (flet ((explain-ending (cause)
             (format *log-output* "~A, ~A~%" cause
                     (cond ((> p1-ships p2-ships) " player 1 wins.")
                           ((> p2-ships p1-ships) " player 2 wins.")
                           (t " it's a draw.")))
             (format *log-output* "Player 1 owns ~D planets, player 2 owns ~D.~%"
                     (length (owned-by 1 planets)) (length (owned-by 2 planets)))
             (format *log-output* "Player 1 has ~D ships, player 2 has ~D.~%"
                     p1-ships p2-ships)
             (format *log-output* "World:~%~A" world)
             (force-output *log-output*)))
      (cond
        ((>= turn (1- maxturns))
         (explain-ending "Turnlimit reached")
         t)
        ((or (<= p1-ships 0)
             (<= p2-ships 0))
         (explain-ending "Out of ships")
         t)
        (t nil)))))

(defun run-game (world turns &rest bots)
  "Run a game in the given world for n turns."
  (let ((*random-state* (make-random-state t)))
    (format *log-output* "Initializing bots.~%")
    (mapc #'startup bots)
    (format *log-output* "Starting game in world~%~A" world)
    (unwind-protect
         (dotimes (turn turns)
           (format *log-output* "Starting turn ~D~%" turn)
           (apply #'simulate world bots)
           (when (game-over? world turn turns)
             (return)))
      (format *log-output* "Game has finished.~%")
      (mapc #'shutdown bots)
      (format *log-output* "Bots have been shut down.~%"))))

(defun read-map (map)
  "Read the game world from a MAP file."
  (with-open-file (stream map :direction :input :if-does-not-exist :error)
    (parse-game-world stream)))

(defun play-game (map turns &rest players)
  "Play a game."
  (flet ((make-bot (player)
           (etypecase player
             (bot player)
             (symbol (make-instance player))
             (string (make-instance 'piped-bot :command player)))))
    (apply #'run-game (read-map map) turns (mapcar #'make-bot players))))

;;;; ----------------------------------------------------------------------------
;;;; * Piped Bot
;;;;
;;;; Starts an external command, feeds it the game state via stdin and read the
;;;; orders via stdout of the process, like the official game engine does.
;;;;

(defclass piped-bot (bot)
  ((command :accessor command :initarg :command)
   (process :accessor process )))

(defmethod startup ((bot piped-bot))
  (setf (process bot) (sb-ext:run-program "/bin/sh"
                                          (list "-c" (command bot))
                                          :wait nil
                                          :input :stream
                                          :output :stream
                                          :error nil)))

(defmethod shutdown ((bot piped-bot))
  (let* ((proc (process bot))
         (out (process-input proc))
         (in (process-output proc)))
    (finish-output out)
    (close out)
    (close in)
    (with-timeout 1
      (process-wait proc))
    (when (process-alive-p proc)
      (process-kill proc 15))))

(defun read-orders (&optional (stream *standard-input*))
  "Read orders from a stream and parse them into a list."
  (loop :for line = (read-line stream nil nil)
        :with orders = ()
        :until (string= line "go")
        :do (let* ((idx (position #\# line))
                   (line (if idx (subseq line idx) line)))
              (let ((members (concatenate 'string "(" line ")")))
                (destructuring-bind (src dst num-ships)
                    (read-from-string members)
                  (push (list src dst num-ships) orders))))
        :finally (return (nreverse orders))))

(defmethod do-turn ((bot piped-bot))
  (let ((out (process-input (process bot)))
        (in (process-output (process bot))))
    (clear-input in)
    (print-game-world out)
    (format out "go~%")
    (force-output out)
    (dolist (order (read-orders in))
      (apply #'issue-order order))))

;;; simulation.lisp ends here
