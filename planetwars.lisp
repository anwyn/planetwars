;;; planetwars.lisp --- Google AI Challenge 2010
;;;
;;;        _                  _
;;;  _ __ | | __ _ _ __   ___| |___      ____ _ _ __ ___
;;; | '_ \| |/ _` | '_ \ / _ \ __\ \ /\ / / _` | '__/ __|
;;; | |_) | | (_| | | | |  __/ |_ \ V  V / (_| | |  \__ \
;;; | .__/|_|\__,_|_| |_|\___|\__| \_/\_/ \__,_|_|  |___/
;;; |_|
;;;
;;; Author: Ole Arndt <anwyn@github.com>
;;; License: MIT
;;;

(defpackage :planetwars
    (:use :cl))

(in-package :planetwars)

;;;; ----------------------------------------------------------------------------
;;;; * World description

(defvar *planets*)
(defvar *fleets*)
(defvar *turn*)

(defclass entity ()
  ((id    :accessor id :initarg :id)
   (owner :accessor owner :initarg :owner)
   (ships :accessor ships :initarg :ships)))

(defclass planet (entity)
  ((x      :accessor x      :initarg :x)
   (y      :accessor y      :initarg :y)
   (growth :accessor growth :initarg :growth)))

(defclass fleet (entity)
  ((source          :accessor source          :initarg :source)
   (destination     :accessor destination     :initarg :destination)
   (trip-length     :accessor trip-length     :initarg :trip-length)
   (turns-remaining :accessor turns-remaining :initarg :turns-remaining)))

;;;; ----------------------------------------------------------------------------
;;;; * Basic Bot

(defclass bot ()
  ())

(defgeneric run-one-turn (bot)
  (:documentation "Called every turn with the world already set up."))

;;;; ----------------------------------------------------------------------------
;;;; * Utilities
;;;;

(defun enemy-planets ()
  "Candidates to be liberated."
  (remove-if-not (lambda (planet)
                   (> (owner planet) 1))
                 *planets*))

(defun my-planets ()
  "Homes of the braves."
  (remove-if-not (lambda (planet)
                   (= (owner planet) 1))
                 *planets*))

(defun neutral-planets ()
  "Soon to be mine."
  (remove-if-not (lambda (planet)
                   (= (owner planet) 0))
                 *planets*))

(defun not-my-planets ()
  "All planets not under my administration."
  (nconc (enemy-planets) (neutral-planets)))

(defun distance (source destination)
  "Return the distance between two planets. This is also the number
of turns a fleet from SOURCE needs to reach the DESTINATION."
  (let ((dx (- (x source) (x destination)))
        (dy (- (x source) (y destination))))
    (ceiling (sqrt (+ (* dx dx) (* dy dy))))))

(defun incoming-fleets (target)
  "Get fleets with TARGET as destination."
  (let ((idx (ensure-index target)))
    (remove-if-not (lambda (fleet)
                     (= idx (destination fleet)))
                   *fleets*)))

;;;; ----------------------------------------------------------------------------
;;;; * Game control

(defun ensure-index (planet)
  "Translate a planet instance to its index."
  (etypecase planet
    (planet (id planet))
    (integer planet)))

(defun ensure-planet (index)
  "Translate a planet index to the instance."
  (etypecase index
    (planet index)
    (integer (nth index *planets*))))

(defun issue-order (source destination num-ships)
  "Issue an order: Send NUM-SHIPS from SOURCE to DESTINATION."
  (format t "~D ~D ~D~%"
          (ensure-index source)
          (ensure-index destination)
          (min (ships (ensure-planet source)) (ceiling num-ships))))

(defun finish-turn ()
  "Finish a turn, emit orders."
  (format t "go~%")
  (force-output))

(defun parse-game-state ()
  "Read the game state from the game server."
  (let ((*read-eval* nil)
        (*read-default-float-format* 'double-float))
    (loop :with planets = nil
          :with fleets = nil
          :with num-planets = -1
          :with num-fleets = -1
          :for line = (read-line *standard-input* nil nil)
          :while line
          :until (string= line "go")
          :do (when (> (length line) 3)
                (let ((members (concatenate 'string "(" (subseq line 2) ")")))
                  (case (elt line 0)
                    (#\P (destructuring-bind (x y owner ships growth)
                             (read-from-string members)
                           (push (make-instance 'planet
                                                :id (incf num-planets) :owner owner
                                                :ships ships :x x :y y :growth growth)
                                 planets)))
                    (#\F (destructuring-bind (owner ships src dst len turns)
                             (read-from-string members)
                           (push (make-instance 'fleet
                                                :id (incf num-fleets) :owner owner
                                                :ships ships :source src :destination dst
                                                :trip-length len :turns-remaining turns)
                                 fleets))))))
          :finally (return (values (nreverse planets)
                                   (nreverse fleets))))))

;;;; ----------------------------------------------------------------------------
;;;; * Logging utilities

(defmethod print-object ((planet planet) stream)
  (with-slots (x y owner ships growth) planet
    (format stream "P ~a ~a ~a ~a ~a"
            x y owner ships growth)))

(defmethod print-object ((fleet fleet) stream)
  (with-slots (owner ships source destination trip-length turns-remaining) fleet
    (format stream "F ~a ~a ~a ~a ~a ~a"
            owner ships source destination trip-length turns-remaining)))

;;;; ----------------------------------------------------------------------------
;;;; * Game loop

(defun planetwars (bot)
  "The game loop."
  (let ((*turn* 0))
    (loop :for char = (peek-char nil *standard-input* nil nil)
          :until (null char)
          :do (progn
                (incf *turn*)
                (multiple-value-bind (*planets* *fleets*)
                    (parse-game-state)
                  (run-one-turn bot))
                (finish-turn))
          :finally (return 0))))

;;; planetwars.lisp ends here
