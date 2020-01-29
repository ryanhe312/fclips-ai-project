;;;======================================================
;;;     Spare Parts Managemant Expert System
;;;
;;;     Fuzzy CLIPS Version 6.31 Example
;;; 
;;;     To execute, merely load, reset, and run.
;;;     Answer questions with numbers.
;;;======================================================

;;;*************************
;;;* Define Fuzzy Rules    *
;;;*************************

(deftemplate mean_delay
   0 0.7
  ((VS  (0.1 1) (0.3 0))
   (S   (0.1 0) (0.3 1) (0.5 0))
   (M   (0.4 0) (0.6 1)) )
)

(deftemplate number_of_servers
   0 1
  ((S    (0.15 1) (0.35 0))
   (M    (0.35 0)(0.5 1)(0.7 0))
   (L    (0.6 0)(0.8 1)))
)

(deftemplate utilization_factor
   0 1
  ((L    (0.4 1) (0.6 0))
   (M    (0.4 0)(0.6 1)(0.8 0))
   (H    (0.6 0)(0.8 1)))
)

(deftemplate number_of_spares
   0 1
  ((VS    (0.1 1) (0.2 0))
   (S     (0.1 0) (0.15 1)  (0.4 0))
   (RS    (0.2 0) (0.325 1) (0.45 0))
   (M     (0.25 0)(0.5 1)   (0.7 0))
   (RL    (0.5 0) (0.65 0)  (0.8 0))
   (L     (0.6 0) (0.8 1))
   (VL    (0.7 0) (0.9 1)))
)

(defrule Rule1
   (utilization_factor L)
  =>
   (assert (number_of_spares S))
)

(defrule Rule2
   (utilization_factor M)
  =>
   (assert (number_of_spares M))
)

(defrule Rule3
   (utilization_factor H)
  =>
   (assert (number_of_spares L))
)

(defrule Rule4
   (mean_delay VS)
   (number_of_servers S)
  =>
   (assert (number_of_spares VL))
)

(defrule Rule5
   (mean_delay S)
   (number_of_servers S)
  =>
   (assert (number_of_spares L))
)

(defrule Rule6
   (mean_delay M)
   (number_of_servers S)
  =>
   (assert (number_of_spares M))
)

(defrule Rule7
   (mean_delay VS)
   (number_of_servers M)
  =>
   (assert (number_of_spares RL))
)

(defrule Rule8
   (mean_delay S)
   (number_of_servers M)
  =>
   (assert (number_of_spares RS))
)

(defrule Rule9
   (mean_delay M)
   (number_of_servers M)
  =>
   (assert (number_of_spares S))
)

(defrule Rule10
   (mean_delay VS)
   (number_of_servers L)
  =>
   (assert (number_of_spares M))
)

(defrule Rule11
   (mean_delay S)
   (number_of_servers L)
  =>
   (assert (number_of_spares S))
)

(defrule Rule12
   (mean_delay M)
   (number_of_servers L)
  =>
   (assert (number_of_spares VS))
)

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
    (bind ?delay (getNumber "How is the mean delay?" 0.0 1.0))
    (assert (mean_delay (?delay 0) (?delay 1) (?delay 0)))
    (bind ?server (getNumber "How is the number of servers?" 0.0 1.0))
    (assert (number_of_servers (?server 0) (?server 1) (?server 0)))
    (bind ?factor (getNumber "How is the utilization factor?" 0.0 1.0))
    (assert (utilization_factor (?factor 0) (?factor 1) (?factor 0)))
)

(defrule start
=>    (initEngine) 

)

;;;*************************
;;;*Define Inference Engine*
;;;*************************

(defrule Engine
    (declare (salience -10))
    ?f1 <- (mean_delay ?x) 
    ?f2 <- (number_of_servers ?y)
    ?f3 <- (utilization_factor ?z)
    ?f4 <- (number_of_spares ?a)
=>
    
    (bind ?spares (moment-defuzzify ?f4))
    (printout t "The Risk will be " ?spares " ." crlf)
    (retract ?f1 ?f2 ?f3 ?f4)
    (if (yes-or-no-p "Would you like to go over again?") then 
        (initEngine)
    )
    
)
