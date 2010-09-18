;;; MyBot.lisp --- Google AI Challenge 2010
;;;
;;;  __  __       ____        _
;;; |  \/  |_   _| __ )  ___ | |_
;;; | |\/| | | | |  _ \ / _ \| __|
;;; | |  | | |_| | |_) | (_) | |_
;;; |_|  |_|\__, |____/ \___/ \__|
;;;         |___/
;;;
;;; Author: Ole Arndt <anwyn@sugarshark.com>
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
  (+ 1
     (ships target)
     (if (neutral? target)
         (* (growth target)
            (distance source target))
         0)))

;;;; ----------------------------------------------------------------------------
;;;; * Example bot

(defclass my-bot (bot)
  ((ships-to-retain :accessor ships-to-retain :initarg :ships-to-retain :initform 3)))

(defmethod do-turn ((bot my-bot))
  "The bot tries to find a random planet that he owns that has more
ships than an randomly picked target planet and sends a fleet
big enough to conquer it.
It takes the growth rate of the other planet into account."
  (loop :for source = (random-element (friendly (planets *world*)))
        :for target = (random-element (unfriendly (planets *world*)))
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
              (when (and (> ships-in-fleet 0) (< ships-in-fleet max-ships))
                (issue-order source target ships-in-fleet)
                (decf (ships source) ships-in-fleet)))))

;;;; ----------------------------------------------------------------------------
;;;; * Startup

(defpackage :pwbot
    (:use #:planetwars #:common-lisp)
  (:export #:main))

(in-package :pwbot)
(defun main ()
  "Main entry point."
  (let ((*random-state* (make-random-state t)))
    (planetwars (make-instance 'planetwars::my-bot :ships-to-retain 3))))

