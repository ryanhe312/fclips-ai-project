;;;======================================================
;;;     Fuzzy Logic Extension
;;;
;;;     Require CLIPS Version 6.3
;;; 
;;;     Using Sugeno FIS Only MISO systems
;;;======================================================

(defclass MFunc (is-a USER)
    (multislot x
        (type NUMBER)
    )
    (multislot y
        (type NUMBER)
    )
)


(defmessage-handler MFunc degree (?x)
    (bind ?leftx 0)
    (bind ?rightx (nth$ (length ?self:x) ?self:x))
    (bind ?lefty 0.0)
    (bind ?righty 0.0)
    (loop-for-count (?cnt (length ?self:x)) do
        (if (and (>= ?x (nth$ ?cnt ?self:x)) (<= ?leftx (nth$ ?cnt ?self:x))) then
            (bind ?leftx (nth$ ?cnt ?self:x))
            (bind ?lefty (nth$ ?cnt ?self:y))
        )
        (if (and (< ?x (nth$ ?cnt ?self:x)) (>= ?rightx (nth$ ?cnt ?self:x))) then
            (bind ?rightx (nth$ ?cnt ?self:x))
            (bind ?righty (nth$ ?cnt ?self:y))
        )
    )
    (bind ?degree 
        (+
            (/ 
                (*
                    (- ?x ?leftx) 
                    (- ?righty ?lefty)
                )
                (- ?rightx ?leftx)
            )
            ?lefty
        )
    )
    (return ?degree)
)


(defmessage-handler MFunc is (?x)
    (bind ?is TRUE)
    (bind ?is 
        (and
            (> ?x (nth$ 1 ?self:x))
            ?is
        )
    )
    (bind ?is 
        (and
            (< ?x (nth$ (length ?self:x) ?self:x))
            ?is
        )
    )
    (return ?is)
)

(defclass FVar (is-a USER)
    (multislot label
        (type SYMBOL)
    )

    (multislot weight
        (type NUMBER)
    )

    (multislot val
        (type NUMBER)
    )
)

(defmessage-handler FVar defuzzy ()
    (bind ?sum 0)
    (bind ?weight 0)
    (loop-for-count (?cnt (length ?self:label)) do
        (bind ?sum
            (+
                ?sum
                (* 
                    (nth$ ?cnt ?self:weight) 
                    (nth$ ?cnt ?self:val)
                )
            )
        )
        (bind ?weight 
            (+
                ?weight
                (nth$ ?cnt ?self:weight) 
            )
        )
    )
    (return (/ ?sum ?weight))
)

(defmessage-handler FVar set (?label ?weight)
    (loop-for-count (?cnt (length ?self:label)) do
        (if (eq ?label (nth$ ?cnt ?self:label)) then
            (send ?self put-weight
                (replace$ ?self:weight ?cnt ?cnt ?weight)
            )
        )
    )
    (return TRUE)
)

(deffunction fmin (?x1 ?MFunc1 ?x2 ?MFunc2)
    (return  
        (min
            (send ?MFunc1 degree ?x1)
            (send ?MFunc2 degree ?x2)
        )
    )
)

(deffunction fmax (?x1 ?MFunc1 ?x2 ?MFunc2)
    (return  
        (max
            (send ?MFunc1 degree ?x1)
            (send ?MFunc2 degree ?x2)
        )
    )
)