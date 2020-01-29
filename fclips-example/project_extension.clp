;;;======================================================
;;;     Project Funding Expert System
;;;
;;;     CLIPS Version 6.3 Fuzzy Logic Extension Example
;;; 
;;;     To execute, load fuzzy_extension.clp first 
;;;     Then load this, reset, and run.
;;;     Answer questions with numbers.
;;;======================================================


;;;*************************
;;;* Ask Questions and init*
;;;*************************

(deffunction getNumber (?question ?from ?to)
  (printout t ?question " [" ?from " to " ?to "]: ")
  (bind ?answer (read))
  (while (not (and (numberp ?answer)
                   (>= ?answer ?from)
                   (<= ?answer ?to)
              )
         )
    do
      (printout t "Please enter a numeric answer within the specified range" crlf 
                  ?question " [" ?from " to " ?to "]: ")
      (bind ?answer (read))
  )
  ?answer
)

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question ?allowed-values )
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer
)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE 
       else FALSE))

(deffunction initEngine ()
    (assert (project_funding (getNumber "How is the project funding?" 0.0 1.0)))
    (assert (project_staff (getNumber "How is the project staff?" 0.0 1.0)))
)

(defrule start
=>
    ; Make MFunc Instances  

    (make-instance adequate of MFunc
        (x 0 0.4 0.9 1)
        (y 0 0 1 1)
    )

    (make-instance marginal of MFunc
        (x 0 0.3 0.55 0.8 1)
        (y 0 0 1 0 0)
    )

    (make-instance inadequate of MFunc
        (x 0 0.3 0.4 1)
        (y 1 1 0 0)
    )

    (make-instance small of MFunc
        (x 0 0.1 0.65 1)
        (y 1 1 0 0)
    )

    (make-instance large of MFunc
        (x 0 0.32 0.72 1)
        (y 0 0 1 1)
    )


    ; Make FVar Instances   

    (make-instance risk of FVar
        (label low normal high)
        (weight 0 0 0)
        (val 0.2 0.5 0.8)
    )

    (initEngine) 

)

;;;*************************
;;;*Define Inference Engine*
;;;*************************


(defrule Engine
    (declare (salience -10))
    ?fl <- (project_funding ?x) 
    ?f2 <- (project_staff ?y)
=>
    (retract ?fl ?f2)
    (printout t "The Risk will be " (send [risk] defuzzy) " ." crlf)
    (if (yes-or-no-p "Would you like to go over again?") then 
        (initEngine)
    )
)

;;;*************************
;;;* Define Fuzzy Rules    *
;;;*************************

(defrule Rule1
    (project_funding ?x&:(send [adequate] is ?x))
    (project_staff ?y&:(send [small] is ?y))
=>
    (bind ?m (fmax ?x [adequate] ?y [small]))
    (send [risk] set low ?m)
)

(defrule Rule2
    (project_funding ?x&:(send [marginal] is ?x))
    (project_staff ?y&:(send [large] is ?y))
=>
    (bind ?m (fmin ?x [marginal] ?y [large]))
    (send [risk] set normal ?m)
)

(defrule Rule3
    (project_funding ?x&:(send [inadequate] is ?x))
=>
    (send [risk] set high (send [inadequate] degree ?x))
)
