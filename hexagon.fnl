;; title:  Hexagonal map
;; author: peterhil
;; desc:   Hexagonal map demo
;; script: fennel

(local pi2 (* 2 math.pi))
(local sq3 (math.sqrt 3))

;; Config ----------------
(local scr {:w 240 :h 136})
(local map {:w 9  :h 6
            :dx 3 :dy 0
            :thr 0.278
            :wrap true})

;; Palette
(local transp 0)

;; Sprites
(local sp {:green 2
           :blue 4
           :bg 34})
(local air sp.blue)

;; Grid settings
(local size 7)
(local hex {:w (* size 2) ; width
            :h (math.floor (* size sq3)) ; height
            :sp 2 ; spacing
            :kind :pointy
            :even false})
(tset hex :col (+ hex.w hex.sp))
(tset hex :row (+ hex.h hex.sp))

;; Buttons
(local bt {:u 0 :d 1
           :l 2 :r 3
           :x 4 :z 5
           :a 6 :s 7})
(local directions
           {:r (/ 0 6)
            :z (/ 1 6)
            :x (/ 2 6)
            :l (/ 3 6)
            :a (/ 4 6)
            :s (/ 5 6)
            :u (/ 3 4)
            :d (/ 1 4)})


;; Math ----------------
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

(lambda iv? [v low high]
  "Is value within half open interval"
  (and (>= v low) (< v high)))

(fn halfstep [v]
    "Floor rounding in half steps so you get zero on interval [-0.25 0.25).
In absolute value, each increment of 0.5 increases the result by 0.5."
    (/ (math.floor (+ (* v 2) 0.5)) 2))

(fn hexstep [v]
    "Floor rounding in one sixth steps so you get zero on interval [± 1/12).
In absolute value, each increment of 1/6 increases the result by 1/6th."
    (/ (math.floor (+ (* v 6) (/ 1 6))) 6))

(fn sextant [v]
    "Sextant on which the value lands on.
Sextants are numbered counter-clockwise from (:x 1 :y 0)."
    (% (* 6 (hexstep v)) 6))

(fn chexp [theta ?mag]
    "Complex number exponential in polar coordinates,
but rounded to multiples of 0.5 so it works on hexagonal grid"
    (let [w (* pi2 theta)]
      (values (halfstep (* (or ?mag 1) (math.cos w)))
              (halfstep (* (or ?mag 1) (math.sin w))))))


;; Complex numbers ----------------
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
                           ;: TODO Check type, non-numbers pass through as nils
                           (tonumber ?imag))
       [_ _] (error (.. "Can’t make a complex number from: " num)))))

(fn cx.add [a b]
    (let [a (cx.from a)
          b (cx.from b)
          v {:x (+ a.x b.x)
             :y (+ a.y b.y)}]
      (setmetatable v cx-meta)))

(fn cx.mod [a b]
    "Complex modulo for wrapping around the map.
When b is real then it’s real part is used as modulo for y also."
    (let [a (cx.from a)
          b (cx.from b)
          v {:x (% a.x b.x)
             :y (% a.y (if (= 0 b.y) b.x b.y))}]
      (setmetatable v cx-meta)))

(tset cx :add cx.add)

(tset cx-meta :__call (fn __call [_ x ?y] (cx.from x ?y)))
(tset cx-meta :__add cx.add)
(tset cx-meta :__mod cx.mod)
(setmetatable cx cx-meta)


;; Lib ----------------
(fn is [typ v] (= (type v) typ))

;; Printing

(fn printc [msg x y ?color]
    "Print message centered on coordinates"
    (local width (print msg 0 scr.h))
    (print msg (- x (half width)) y (or ?color 14)))

(fn btd [b]
    (print (.. :btn ": " b) 0 (- scr.h 10) 14))

;; Hex grid map

(fn hex-offset [v ?sub]
    "Alternate odd rows on grid"
    (let [f (if ?sub sub add)
          o (/ (math.abs (% v 2)) 2)]
      (f 0 o)))

(fn odd-row? [plr]
    (~= hex.even (odd? plr.y)))

(fn alt-row-offset [plr]
    (hex-offset 1 (odd-row? plr)))

(fn gen-map [n threshold]
    (var map [])
    (for [i 1 n]
         (table.insert map
                       (if (> threshold (math.random))
                           sp.bg
                           sp.blue)))
    map)


;; Map and movement ----------------

(fn can-move? [pos cells]
    (let [fy (math.floor pos.y)
          fx (math.floor (incr pos.x))
          idx (+ (* fy map.w) fx)]
      (= air (. cells idx))))

(fn in-map? [pos]
    (and
     (iv? pos.x 0 map.w)
     (iv? pos.y 0 map.h)))

(fn collision? [pos cells]
    (or (not (in-map? pos))
        (not (can-move? pos cells))))

(fn move [val dir]
    "Move complex value to some direction on map"
    (let [cxmap (cx.new map.w map.h)]
      (if map.wrap
          (% (+ val dir) cxmap)
          (+ val dir))))

(fn new-position [plr dir cells]
    "Move player to some direction"
    (let [pos (move plr dir)]
      (if (collision? pos cells)
          (do (printc (.. "Can not move to (:y " pos.y " :x " pos.x ")")
                      (half scr.w) (- scr.h 30) 12)
              plr)
          pos)))

(fn deviation [plr key]
    "Angle deviation for up and down movement to align with hex grid
on alternate rows"
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


;; Side effects --------

(fn hello []
    (printc "HEXAGONAL WORLD!" (half scr.w) (- scr.h 20) 12))

(fn sp-draw [id cell]
    "Draw sprite id on cell with x and y coordinates"
    (let [{: x : y} cell]
      (spr id
           (* (+ x (hex-offset y hex.even)) hex.col)
           (* y hex.row)
           transp 1 0 0 2 2)))

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
      (printc (.. :player " x: " x " y: " y)
              (half scr.w) (- scr.h 10) 15)
      (spr id
           (* hex.col (+ x map.dx))
           (* hex.row (+ y map.dy))
           transp 1 0 0 2 2)))


;; Main ----------------

(var cells (gen-map (* map.w map.h) map.thr))
(var plr (cx {:y 0 :x 7}))
(var time 0)

(global
 TIC
 (fn tic []
     (cls 0)
     (draw-map cells)

     (local dir (dir-events plr))
     (set plr (new-position plr dir cells))
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

