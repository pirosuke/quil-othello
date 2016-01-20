(ns quil-othello.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def bsize 60)

(def fields (to-array-2d (for [x (range 8)] (repeat 8 0))))

(defn draw-background-board
  []
  (q/background 0 160 0)
  (q/stroke 0)
  (doseq [i (range 1 8)]
    (q/line (* i bsize) 0 (* i bsize) (q/height))
    (q/line 0 (* i bsize) (q/width) (* i bsize)))
  (q/no-stroke)
  (q/fill 0)
  (q/ellipse (* bsize 2) (* bsize 2) 10 10)
  (q/ellipse (* bsize 6) (* bsize 2) 10 10)
  (q/ellipse (* bsize 2) (* bsize 6) 10 10)
  (q/ellipse (* bsize 6) (* bsize 6) 10 10))

(defn draw-stone
  [color x y]
  (q/fill color)
  (q/ellipse
   (/ (* (+ (* x 2) 1) bsize) 2)
   (/ (* (+ (* y 2) 1) bsize) 2)
   (* bsize 0.8)
   (* bsize 0.8)))

(defn setup
  []
  (js/console.log "setup")
  ; Set frame rate per second.
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (doseq [x (range 8) y (range 8)]
    (cond
     (and (= x 3) (= y 3)) (aset fields x y 1)
     (and (= x 4) (= y 4)) (aset fields x y 1)
     (and (= x 3) (= y 4)) (aset fields x y -1)
     (and (= x 4) (= y 3)) (aset fields x y -1)))
  ; setup function returns initial state.
  {:black-turn? true})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  ;(js/console.log fields)
  state)

(defn draw-state [state]
  (draw-background-board)
  (q/no-stroke)
  (doseq [x (range 8) y (range 8)]
    (cond
     (= (aget fields x y) 1) (draw-stone 0 x y)
     (= (aget fields x y) -1) (draw-stone 255 x y))))

(defn current-stone
  [state]
  (if (:black-turn? state) 1 -1))

(defn check-bound
  [x y]
  (and (and (>= x 0) (< x 8)) (and (>= y 0) (< y 8))))

(defn check-stones
  [state x y direction-x direction-y]
  (let [new-x (+ x direction-x)
        new-y (+ y direction-y)]
    (cond
     (and (check-bound new-x new-y) (= (aget fields new-x new-y) (current-stone state))) true
     (and (check-bound new-x new-y) (= (aget fields new-x new-y) 0)) false
     (and (check-bound new-x new-y) (check-stones state new-x new-y direction-x direction-y)) (do
                                                                                          (aset fields new-x new-y (current-stone state))
                                                                                          true)
     :else false)))

(defn check-direction
  [state x y direction-x direction-y]
  (let [new-x (+ x direction-x)
        new-y (+ y direction-y)]
    (if (and (check-bound new-x new-y) (not= (aget fields new-x new-y) (current-stone state)))
      (check-stones state x y direction-x direction-y)
      false)))

(defn on-mouse-released
  [state event]
  (js/console.log (pr-str state))
  (let [x (long (/ (:x event) bsize))
        y (long (/ (:y event) bsize))
        puttable (if (= (aget fields x y) 0) (->> false
                                                     (or (check-direction state x y -1 -1))
                                                     (or (check-direction state x y -1 0))
                                                     (or (check-direction state x y -1 1))
                                                     (or (check-direction state x y 0 -1))
                                                     (or (check-direction state x y 0 1))
                                                     (or (check-direction state x y 1 -1))
                                                     (or (check-direction state x y 1 0))
                                                     (or (check-direction state x y 1 1))) false)]
    (cond
     puttable (do
                  (aset fields x y (current-stone state))
                  (js/console.log "putted")
                  (assoc state :black-turn? (not (:black-turn? state))))
     :else state)))

(q/defsketch quil-othello
  :host "quil-othello"
  :size [(* 8 bsize) (* 8 bsize)]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :mouse-released on-mouse-released
  :update update-state
  :draw draw-state
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
