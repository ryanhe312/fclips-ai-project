;;;======================================================
;;;     Project Funding Expert System
;;;
;;;     Fuzzy CLIPS Version 6.31 Example
;;; 
;;;     To execute, merely load, reset, and run.
;;;     Answer questions with numbers.
;;;======================================================

;;;*************************
;;;* Define Fuzzy Rules    *
;;;*************************

(deftemplate project_funding
   0 1
  ((adequate  (0.4 0) (0.9 1))
   (marginal    (0.3 0) (0.55 1) (0.8 0))
   (inadequate   (0.3 1) (0.4 0)) )
)

(deftemplate project_staff
   0 1
  ((small  (0.1 1) (0.65 0))
   (large    (0.32 0)(0.72 1)))
)

(deftemplate risk
   0 1
  ((low    (0.3 1) (0.5 0))
   (normal     (0.2 0)(0.4 1)(0.6 1)(0.8 0))
   (high (0.5 0)(0.7 1)) )
)

(defrule Rule1
   (project_funding adequate)
   (project_staff small)
  =>
   (assert (risk low))
)

(defrule Rule2
   (project_funding marginal)
   (project_staff large)
  =>
   (assert (risk normal))
)

(defrule Rule3
   (project_funding inadequate)
  =>
   (assert (risk high))
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
    (bind ?funding (getNumber "How is the project funding?" 0.0 1.0))
    (assert (project_funding (?funding 0) (?funding 1) (?funding 0)))
    (bind ?staff (getNumber "How is the project staff?" 0.0 1.0))
    (assert (project_staff (?staff 0) (?staff 1) (?staff 0)))
)

(defrule start
=>    (initEngine) 

)

;;;*************************
;;;*Define Inference Engine*
;;;*************************

(defrule Engine
    (declare (salience -10))
    ?fl <- (project_funding ?) 
    ?f2 <- (project_staff ?)
    ?f3 <- (risk ?)
=>
    
    (bind ?risk (moment-defuzzify ?f3))
    (printout t "The Risk will be " ?risk " ." crlf)
    (retract ?fl ?f2 ?f3)
    (if (yes-or-no-p "Would you like to go over again?") then 
        (initEngine)
    )
    
)
