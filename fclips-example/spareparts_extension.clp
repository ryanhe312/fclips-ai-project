;;;======================================================
;;;     Spare Parts Managemant Expert System
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
    (assert (mean_delay (getNumber "How is the mean delay?" 0.0 1.0)))
    (assert (number_of_servers (getNumber "How is the number of servers?" 0.0 1.0)))
    (assert (utilization_factor (getNumber "How is the utilization factor?" 0.0 1.0)))
)

(defrule start
=>
    ; Make MFunc Instances  

    (make-instance mean_delay_VS of MFunc
        (x 0    0.1     0.3     0.7)
        (y 1    1       0       0)
    )

    (make-instance mean_delay_S of MFunc
        (x 0    0.1     0.3     0.5     0.7)
        (y 0    0       1       0       0)
    )

    (make-instance mean_delay_M of MFunc
        (x 0    0.4     0.6     0.7)
        (y 0    0       1       1)
    )

    (make-instance number_of_servers_S of MFunc
        (x 0    0.15    0.35    1 )
        (y 1    1       0       0 )
    )

    (make-instance number_of_servers_M of MFunc
        (x 0    0.35    0.5     0.7     1)
        (y 0    0       1       0       0)
    )

    (make-instance number_of_servers_L of MFunc
        (x 0    0.6    0.8      1)
        (y 0    0       1       1)
    )

    (make-instance utilization_factor_L of MFunc
        (x 0    0.4     0.6     1)
        (y 1    1       0       0)
    )

    (make-instance utilization_factor_M of MFunc
        (x 0    0.4     0.6     0.8     1)
        (y 0    0       1       0       0)
    )

    (make-instance utilization_factor_H of MFunc
        (x 0    0.6     0.8     1)
        (y 0    0       1       1)
    )


    ; Make FVar Instances   

    (make-instance number_of_spares of FVar
        (label  VS  S   RS      M   RL      L   VL)
        (weight 0   0   0       0   0       0   0)
        (val    0.1 0.2 0.35    0.5 0.65    0.8 0.9)
    )

    (initEngine) 

)

;;;*************************
;;;*Define Inference Engine*
;;;*************************


(defrule Engine
    (declare (salience -10))
    ?f1 <- (mean_delay ?x) 
    ?f2 <- (number_of_servers ?y)
    ?f3 <- (utilization_factor ?z)
=>
    (retract ?f1 ?f2 ?f3)
    (printout t "The number of spares will be " (send [number_of_spares] defuzzy) " ." crlf)
    (if (yes-or-no-p "Would you like to go over again?") then 
        (initEngine)
    )
)

;;;*************************
;;;* Define Fuzzy Rules    *
;;;*************************

(defrule Rule1
    (utilization_factor ?x&:(send [utilization_factor_L] is ?x))
=>
    (send [number_of_spares] set S (send [utilization_factor_L] degree ?x))
)

(defrule Rule2
    (utilization_factor ?x&:(send [utilization_factor_M] is ?x))
=>
    (send [number_of_spares] set M (send [utilization_factor_M] degree ?x))
)

(defrule Rule3
    (utilization_factor ?x&:(send [utilization_factor_H] is ?x))
=>
    (send [number_of_spares] set L (send [utilization_factor_H] degree ?x))
)

(defrule Rule4
    (mean_delay ?x&:(send [mean_delay_VS] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_S] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_VS] ?y [number_of_servers_S]))
    (send [number_of_spares] set VL ?m)
)

(defrule Rule5
    (mean_delay ?x&:(send [mean_delay_S] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_S] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_S] ?y [number_of_servers_S]))
    (send [number_of_spares] set L ?m)
)

(defrule Rule6
    (mean_delay ?x&:(send [mean_delay_M] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_S] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_M] ?y [number_of_servers_S]))
    (send [number_of_spares] set M ?m)
)

(defrule Rule7
    (mean_delay ?x&:(send [mean_delay_VS] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_M] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_VS] ?y [number_of_servers_M]))
    (send [number_of_spares] set RL ?m)
)

(defrule Rule8
    (mean_delay ?x&:(send [mean_delay_S] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_M] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_S] ?y [number_of_servers_M]))
    (send [number_of_spares] set RS ?m)
)

(defrule Rule9
    (mean_delay ?x&:(send [mean_delay_M] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_M] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_M] ?y [number_of_servers_M]))
    (send [number_of_spares] set S ?m)
)

(defrule Rule10
    (mean_delay ?x&:(send [mean_delay_VS] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_L] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_VS] ?y [number_of_servers_L]))
    (send [number_of_spares] set M ?m)
)

(defrule Rule11
    (mean_delay ?x&:(send [mean_delay_S] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_L] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_S] ?y [number_of_servers_L]))
    (send [number_of_spares] set S ?m)
)

(defrule Rule12
    (mean_delay ?x&:(send [mean_delay_M] is ?x))
    (number_of_servers ?y&:(send [number_of_servers_L] is ?y))
=>
    (bind ?m (fmin ?x [mean_delay_M] ?y [number_of_servers_L]))
    (send [number_of_spares] set VS ?m)
)