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
  "Copy the game world. Current bot will always be player nr.1"
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
  (remove-if-not (lambda (p)
                   (some #'arrived? (incoming-fleets p (fleets world))))
                 (planets world)))

(defun fight-battles (world)
  (dolist (planet (planets-under-attack world))
    (invasion planet
              (remove-if-not #'arrived?
                             (incoming-fleets planet (fleets world))))))

(defun invasion (planet fleets)
  "Attack the planet with fleets"
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



(defun execute-order (world owner order)
  (destructuring-bind (source destination num-ships) order
    (let* ((src (elt (planets world) source))
           (dst (elt (planets world) destination))
           (dist (distance src dst)))
      (assert (not (eq src dst)))
      (assert (> num-ships 0))
      (assert (<= num-ships (ships src)))
      (assert (= (owner src) owner))
      (decf (ships src) num-ships)

      (format *log-output*
              "Player ~D sends ~D ships from planet ~D to planet ~D.~%"
              owner num-ships source destination)

      (push (make-instance 'fleet
                           :id (length (fleets world))
                           :owner owner
                           :ships num-ships
                           :source src
                           :destination dst
                           :trip-length dist
                           :turns-remaining dist)
            (fleets world)))))


(defun advance-fleets (world)
  (dolist (fleet (fleets world))
    (decf (turns-remaining fleet))))

(defun remove-arrived-fleets (world)
  (setf (fleets world)
        (remove-if #'arrived? (fleets world))))

(defun grow-planets (world)
  (dolist (p (remove-if #'neutral? (planets world)))
    (incf (ships p) (growth p))))


(defun simulate (world orders1 orders2)
  (dolist (order orders1)
    (execute-order world 1 order))

  (dolist (order orders2)
    (execute-order world 2 order))

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
  (let ((p1-ships (reduce #'+ (owned-by 1 (append (fleets world) (planets world))) :key #'ships))
        (p2-ships (reduce #'+ (owned-by 2 (append (fleets world) (planets world))) :key #'ships)))
    (cond ((>= turn (1- maxturns))
           (break)
           (format *log-output* "Turnlimit reached, ~A~%"
                   (cond ((> p1-ships p2-ships) " Player 1 wins.")
                         ((> p2-ships p1-ships) " Player 2 wins.")
                         (t " it's a draw.")))

           (format *log-output* "Player 1 has ~D ships, Player 2 has ~D.~%"
                   p1-ships p2-ships)

           (format *log-output* "~A" world)

           (force-output *log-output*)
           t)
          ((or (<= p1-ships 0)
               (<= p2-ships 0))

           (format *log-output* "Out of ships, ~A~%"
                   (cond ((> p1-ships p2-ships) " Player 1 wins.")
                         ((> p2-ships p1-ships) " Player 2 wins.")
                         (t " it's a draw.")))

           (format *log-output* "Player 1 has ~D ships, Player 2 has ~D.~%"
                   p1-ships p2-ships)

           (format *log-output* "~A" world)

           (force-output *log-output*)
           t)
          (t nil))))


(defun run-game (world bot1 bot2 &key (turns 1000))
  (startup bot1)
  (startup bot2)
  (format *log-output* "Starting game...~%")
  (unwind-protect
       (dotimes (turn turns)
         (format *log-output* "Starting turn ~D~%" turn)
         (simulate world
                   (run-bot bot1 (copy-world world 1))
                   (run-bot bot2 (copy-world world 2)))
         (when (game-over? world turn turns)
           (return)))
    (shutdown bot1)
    (shutdown bot2)))


(defun read-map (map)
  (with-open-file (stream map :direction :input :if-does-not-exist :error)
    (parse-game-world stream)))

(defun play-game (map player1 player2 &key (turns 1000))
  (flet ((make-bot (player)
           (etypecase player
             (bot player)
             (symbol (make-instance player))
             (string (make-instance 'piped-bot :command player))
             (list (make-instance 'piped-bot :command player)))))
      (let ((world (read-map map))
            (bot1 (make-bot player1))
            (bot2 (make-bot player2)))
        (run-game world bot1 bot2 :turns turns))))

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
