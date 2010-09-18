;;; planetwars.lisp --- Google AI Challenge 2010
;;;        _                  _
;;;  _ __ | | __ _ _ __   ___| |___      ____ _ _ __ ___
;;; | '_ \| |/ _` | '_ \ / _ \ __\ \ /\ / / _` | '__/ __|
;;; | |_) | | (_| | | | |  __/ |_ \ V  V / (_| | |  \__ \
;;; | .__/|_|\__,_|_| |_|\___|\__| \_/\_/ \__,_|_|  |___/
;;; |_|
;;;
;;; Author: Ole Arndt <anwyn@sugarshark.com>
;;; License: Apache Software License 2.0
;;;

(defpackage :planetwars
    (:use :cl :sb-ext))

(in-package :planetwars)

;;;; ----------------------------------------------------------------------------
;;;; * World description

(defvar *world*)
(defvar *orders*)
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

(defclass world ()
  ((planets :accessor planets :initarg :planets)
   (fleets  :accessor fleets  :initarg :fleets)))

(defmethod initialize-instance :after ((world world) &key &allow-other-keys)
  (with-accessors ((planets planets)
                   (fleets fleets)) world
    (dolist (fleet fleets)
      (with-accessors ((src source)
                       (dst destination)) fleet
        (when (and (integerp src) (integerp dst))
          (setf src (elt planets src)
                dst (elt planets dst)))))))

(defun parse-game-world (&optional (stream *standard-input*))
  "Read the game state from the game server."
  (let ((*read-eval* nil)
        (*read-default-float-format* 'double-float))
    (loop :with planets = nil
          :with fleets = nil
          :with num-planets = -1
          :with num-fleets = -1
          :for line = (read-line stream nil nil)
          :while line
          :until (string= line "go")
          :do (let* ((idx (position #\# line))
                     (line (if idx (subseq line idx) line)))
                (when (> (length line) 3)
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
                                   fleets)))))))
          :finally (return (make-instance 'world
                                          :planets (nreverse planets)
                                          :fleets (nreverse fleets))))))

;;;; ----------------------------------------------------------------------------
;;;; * Basic Bot

(defclass bot ()
  ())

(defgeneric startup (bot)
  (:documentation "Startup the bot. This is called before the game state is sent.")
  (:method (bot)))

(defgeneric shutdown (bot)
  (:documentation "Shutdown the bot. This is called at the end of the game.")
  (:method (bot)))

(defgeneric do-turn (bot)
  (:documentation "Called every turn with the world already set up in the
dynamic variable *WORLD*")
  (:method (bot)))


;;;; ----------------------------------------------------------------------------
;;;; * Utilities
;;;;

;;; predicates

(defun owner? (entity owner)
  (= (owner entity) owner))

(defun hostile? (entity)
  (> (owner entity) 1))

(defun friendly? (entity)
  (owner? entity 1))

(defun neutral? (entity)
  (owner? entity 0))

(defun arrived? (fleet)
  (= (turns-remaining fleet) 0))

;;; selectors for entities

(defun owned-by (owner entities)
  (remove-if-not (lambda (e) (owner? e owner)) entities))

(defun not-owned-by (owner entities)
  (remove-if (lambda (e) (owner? e owner)) entities))

(defun hostile (entities)
  (remove-if-not #'hostile? entities))

(defun friendly (entities)
  (remove-if-not #'friendly? entities))

(defun neutral (entities)
  (remove-if-not #'neutral? entities))

(defun unfriendly (entities)
  (remove-if #'friendly? entities))

(defun distance (source destination)
  "Return the distance between two planets. This is also the number
of turns a fleet from SOURCE needs to reach the DESTINATION."
  (let ((dx (- (x source) (x destination)))
        (dy (- (x source) (y destination))))
    (ceiling (sqrt (+ (* dx dx) (* dy dy))))))

(defun incoming-fleets (target &optional (fleets (fleets *world*)))
  "Get fleets with TARGET as destination, nearest first."
  (sort (remove-if-not (lambda (fleet)
                         (eq target (destination fleet)))
                       fleets)
        #'< :key #'turns-remaining))

(defun outgoing-fleets (source &optional (fleets (fleets *world*)))
  "Get fleets originating from source, furtherst first."
  (sort (remove-if-not (lambda (fleet)
                         (eq source (destination fleet)))
                       fleets)
        #'> :key #'turns-remaining))

;;;; ----------------------------------------------------------------------------
;;;; * Game control
;;;;


(defgeneric issue-order (source destination num-ships)
  (:documentation "Issue an order: Send NUM-SHIPS from SOURCE to DESTINATION.")
  (:method ((source integer) (destination integer) (num-ships number))
    (push (list source destination (ceiling num-ships)) *orders*))
  (:method ((source planet) (destination planet) (num-ships number))
    (push (list (id source) (id destination)
                (min (ships source) (ceiling num-ships)))
          *orders*)))


;;;; ----------------------------------------------------------------------------
;;;; * Logging utilities

(defmethod print-object ((planet planet) stream)
  (with-slots (x y owner ships growth) planet
    (format stream "P ~,10F ~,10F ~D ~D ~D"
            x y owner ships growth)))

(defmethod print-object ((fleet fleet) stream)
  (with-slots (owner ships source destination trip-length turns-remaining) fleet
    (format stream "F ~D ~D ~D ~D ~D ~D"
            owner ships (id source) (id destination) trip-length turns-remaining)))

(defmethod print-object ((world world) stream)
  (with-slots (planets fleets) world
    (format stream "~{~A~%~}~%~{~A~%~}" planets fleets)))

(defun print-game-world (&optional (stream *standard-output*))
  (format stream "~A" *world*))

;;;; ----------------------------------------------------------------------------
;;;; * Game loop

(defun run-bot (bot world)
  "Run a bot and return the orders it issued."
  (let ((*orders* nil)
        (*world* world))
    (do-turn bot)
    *orders*))

(defun finish-turn (orders &optional (stream *standard-output*))
  "Finish a turn, emit orders."
  (format stream "~{~D ~D ~D~%~}" orders)
  (format stream "go~%")
  (force-output stream))

(defun planetwars (bot &optional (input *standard-input*) (output *standard-output*))
  "The game loop for one bot when driven by the external game engine."
  (let ((*turn* 0))
    (startup bot)
    (unwind-protect
         (loop :for char = (peek-char nil input nil nil)
               :until (null char)
               :do (let ((world (parse-game-world input)))
                     (incf *turn*)
                     (finish-turn (run-bot bot world) output))))
    (shutdown bot)))


;;; planetwars.lisp ends here
