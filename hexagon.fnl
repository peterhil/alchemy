;; title:  Hexagonal map
;; author: peterhil
;; desc:   Hexagonal map demo
;; script: fennel

(local scr {:w 240 :h 136})
(local transp 0)

;; Useful helpers
(fn add [a b ...]
    (let [sum (+ (or a 0) (or b 0))]
      (if (= (select :# ...) 0)
          sum
          (add sum ...))))

(fn sub [a b ...]
    (let [sum (- (or a 0) (or b 0))]
      (if (= (select :# ...) 0)
          sum
          (sub sum ...))))

(fn half [v] (/ v 2))

(fn even? [v] (= 0 (% v 2)))
(fn odd? [v] (= 1 (% v 2)))

(fn incr [v a] (+ v (or a 1)))
(fn decr [v a] (- v (or a 1)))

(fn << [b disp] (% (* b (^ 2 disp)) (^ 2 32)))
(fn >> [b disp] (math.floor (/ (% b (^ 2 32)) (^ 2 disp))))

(fn halfstep [v]
    "Floor rounding in half steps so you get zero on interval [-0.25 0.25).
In absolute value, each increment of 0.5 increases the result by 0.5."
    (/ (math.floor (+ (* v 2) 0.5)) 2))

(local pi2 (* 2 math.pi))

(fn chexp [theta ?mag]
    "Complex number exponential in polar coordinates,
but rounded to multiples of 0.5 so it works on hexagonal grid"
    (let [w (* pi2 theta)]
      (values (halfstep (* (or ?mag 1) (math.cos w)))
              (halfstep (* (or ?mag 1) (math.sin w))))))

(fn cartesian [...]
    "Pack two values into a map with x and y coordinates"
    (let [(x y) ...]
      {: x : y}))

(fn is [typ v] (= (type v) typ))

(lambda iv? [v low high]
  "Is value within half open interval"
  (and (>= v low) (< v high)))

(fn zbi [v]
    "Decrease numeric values by one for zero based indexing"
    (if (is "number" v)
        (decr v)
        v))

(fn rev-idx [map]
    "Swap values as keys with zero based indexing"
    ;; Collect could be used on Fennel 0.9.x:
    ;; (collect [i v (pairs map)] (values v (zbi i)))
    (let [idx {}]
      (each [i v (pairs map)]
            (match (values v i)
                   (key val) (tset idx key (zbi val))))
      idx))

;; Complex numbers

(local cx {})
(local cx-meta {})

(fn cx.new [x ?y]
    "Complex number vector"
    (let [v {:x x
             :y (or ?y 0)}]
      (setmetatable v cx-meta)))

(fn cx.from [num ?imag]
    "Return complex number from numeric argument"
    (let [cm cx-meta]
      (match
       [(type num) (getmetatable num)]
       [:table cm] num
       [:table  _] (let [{: x : y} num] (cx.new x y))
       [:number _] (cx.new num
                           ;: TODO Check type, now non-numbers pass through as nils
                           (tonumber ?imag))
       [_ _] (error (.. "Can’t make a complex number from: " num)))))

(fn cx.add [a b]
    (let [ca (cx.from a)
          cb (cx.from b)
          v {:x (+ ca.x cb.x)
             :y (+ ca.y cb.y)}]
      (setmetatable v cx-meta)))

(tset cx :add cx.add)

(tset cx-meta :__call (fn __call [_ x ?y] (cx.from x ?y)))
(tset cx-meta :__add cx.add)
(setmetatable cx cx-meta)

;; Sprite map
(local sp {:green 2
           :blue 4
           :bg 34})

;; General
(local air sp.blue)
(local map {:w 9 :h 6 :dx 3 :dy 0 :thr 0.278 :wrap true})
(local plr (cx {:y 0 :x 7}))
(var time 0)

;; Buttons
(local bt (rev-idx [:u :d :l :r :x :z :a :s]))
(local directions
           {:r (/ 0 6)
            :z (/ 1 6)
            :x (/ 2 6)
            :l (/ 3 6)
            :a (/ 4 6)
            :s (/ 5 6)
            :u (/ 3 4)
            :d (/ 1 4)})

;; Hexagon grid
(local size 7)
(local sq3 (math.sqrt 3))
(local hex {:w (* size 2) ; width
            :h (math.floor (* size sq3)) ; height
            :sp 2 ; spacing
            :kind :pointy
            :even false})
(tset hex :col (+ hex.w hex.sp))
(tset hex :row (+ hex.h hex.sp))

(fn hex-offset [v ?sub]
    "Alternate odd rows on grid"
    (let [f (if ?sub sub add)
          o (/ (math.abs (% v 2)) 2)]
      (f 0 o)))

(fn odd-row? [plr]
    (~= hex.even (odd? plr.y)))

(fn alt-row-offset [plr]
    (hex-offset 1 (odd-row? plr)))

;; Game

(fn sp-draw [id cell]
    "Draw sprite id on cell with x and y coordinates"
    (let [{: x : y} cell]
      (spr id
           (* (+ x (hex-offset y hex.even)) hex.col)
           (* y hex.row)
           transp 1 0 0 2 2)))

(fn printc [msg x y ?color]
    "Print message centered on coordinates"
    (local width (print msg 0 scr.h))
    (print msg (- x (half width)) y (or ?color 14)))

(fn hello []
    (printc "HEXAGONAL WORLD!" (half scr.w) (- scr.h 20) 12))

(fn btd [b]
    (print (.. :btn ": " b) 0 (- scr.h 10) 14))

;; Map generation and movement

(fn gen-map [n threshold]
    (var map [])
    (for [i 1 n]
         (table.insert map (if (> threshold (math.random)) sp.bg sp.blue)))
    map)

(fn can-move? [cells y x]
    (= air (. cells (+ (* y map.w) x))))

(fn in-map? [y x]
    (and
     (iv? x 0 map.w)
     (iv? y 0 map.h)))

(fn draw-map [cells]
    "Draw hexagonal grid"
    (var i 0)
    (for [y 0 (- map.h 1)]
         (for [x 0 (- map.w 1)]
              (set i (+ i 1))
              (sp-draw (. cells i)
                       {:y (+ y map.dy)
                        :x (+ x map.dx)}))))

(fn draw-player [plr]
    "Draw player"
    (let [{: y : x} plr
          id 2 ;;(+ 2 (* (// (% time 60) 30) 2))
          ]
      (printc (.. :player " x: " x " y: " y) (half scr.w) (- scr.h 10) 15)
      (spr id
           (* hex.col (+ x map.dx))
           (* hex.row (+ y map.dy))
           transp 1 0 0 2 2)))

(fn move-player! [plr dir cells]
    "Move player to some direction"
    (let [y (if map.wrap
                (% (+ plr.y dir.y) map.h)
                (+ plr.y dir.y))
          x (if map.wrap
                (% (+ plr.x dir.x) map.w)
                (+ plr.x dir.x))
          ty (math.floor y)
          tx (math.floor (incr x))]
      (if (and (in-map? y x)
               (can-move? cells ty tx))
          (do
           (tset plr :y y)
           (tset plr :x x))
          (printc (.. "Can not move to (:y " ty " :x " tx ")") (half scr.w) (- scr.h 30) 12)))
    plr)

(fn deviation [plr key]
    "Angle deviation for up and down movement to align with hex grid on alternate rows"
    (match key
           :u (if (odd-row? plr) (/ -1 12) (/  1 12))
           :d (if (odd-row? plr) (/  1 12) (/ -1 12))))

(fn dir-events [plr]
    "Get directions from button events.
Uses polar coordinates and converts to cartesian."
    (local moves [])
    (each [key angle (pairs directions)]
          (when (btnp (. bt key))
            (do (btd key)
                (let [phi (match key
                                 :u (+ angle (deviation plr key))
                                 :d (+ angle (deviation plr key))
                                 angle)
                      move (cx (chexp phi))]
                  (table.insert moves move)))))
    (cx (add (table.unpack moves))))

(local cells (gen-map (* map.w map.h) map.thr))

(global
 TIC
 (fn tic []
     (cls 0)
     (draw-map cells)

     (local dir (dir-events plr))

     (move-player! plr dir cells)
     (draw-player plr)

     (hello)

     (set time (+ time 1))))

;; <TILES>
;; 002:0000006500006555006555556555555555555555555555555555555555555555
;; 003:6000000055600000555560005555556055555550555555505555555055555550
;; 004:000000ab0000abbb00abbbbbabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
;; 005:a0000000bba00000bbbba000bbbbbba0bbbbbbb0bbbbbbb0bbbbbbb0bbbbbbb0
;; 018:5555555555555555555555555555555565555555006555550000655500000065
;; 019:5555555055555550555555505555555055555560555560005560000060000000
;; 020:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbabbbbbbb00abbbbb0000abbb000000ab
;; 021:bbbbbbb0bbbbbbb0bbbbbbb0bbbbbbb0bbbbbba0bbbba000bba00000a0000000
;; 034:000000ff0000ff0000ff0000ff000000f0000000f0000000f0000000f0000000
;; 035:f00000000ff00000000ff00000000ff0000000f0000000f0000000f0000000f0
;; 050:f0000000f0000000f0000000f0000000ff00000000ff00000000ff00000000ff
;; 051:000000f0000000f0000000f0000000f000000ff0000ff0000ff00000f0000000
;; </TILES>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

