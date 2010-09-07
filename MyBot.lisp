;;; MyBot.lisp --- Google AI Challenge 2010
;;;
;;;  __  __       ____        _
;;; |  \/  |_   _| __ )  ___ | |_
;;; | |\/| | | | |  _ \ / _ \| __|
;;; | |  | | |_| | |_) | (_) | |_
;;; |_|  |_|\__, |____/ \___/ \__|
;;;         |___/
;;;
;;; Author: Ole Arndt <anwyn@github.com>
;;; License: MIT
;;;

;; load world description and utilities
(load "planetwars")

(in-package :planetwars)

;;;; ----------------------------------------------------------------------------
;;;; * Utilities

(defun random-element (sequence)
  "Pick a random element from a sequence."
  (let ((len (length sequence)))
    (when (> len 0)
      (elt sequence (random len)))))

(defun fleet-needed (source target)
  (+ (ships target)
     (* (growth target)
        (distance source target))))

;;;; ----------------------------------------------------------------------------
;;;; * Example bot

(defclass my-bot (bot)
  ((ships-to-retain :accessor ships-to-retain :initarg :ships-to-retain)))

(defmethod run-one-turn ((bot my-bot))
  "The bot tries to find a random planet that he owns that has more
ships than an randomly picked target planet and sends a fleet
big enough to conquer it.
It takes the growth rate of the other planet into account."
  (loop :for source = (random-element (my-planets))
        :for target = (random-element (not-my-planets))
        :for tries from 1
        :while (< tries 10)
        :when (and source
                   target
                   (null (incoming-fleets target))
                   (> (- (ships source)
                         (fleet-needed source target))
                      (ships-to-retain bot)))
        :do (let* ((max-ships (- (ships source) (ships-to-retain bot)))
                   (needed-ships (fleet-needed source target))
                   (ships-in-fleet (/ (+ max-ships needed-ships) 2)))
              (issue-order source target ships-in-fleet)
              (decf (ships source) ships-in-fleet))))

;;;; ----------------------------------------------------------------------------
;;;; * Startup

(defun main ()
  "Main entry point."
  (setf *random-state* (make-random-state t))
  (planetwars (make-instance 'my-bot :ships-to-retain 3)))

;;; Go, go, go
#+sbcl
(sb-ext:quit :unix-status (main))
#+abcl
(extensions:quit :status (main))
#+ccl
(ccl:quit (planetwars (main)))

