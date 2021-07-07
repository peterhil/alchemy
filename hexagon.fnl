;; title:  Hexagonal map
;; author: peterhil
;; desc:   Hexagonal map demo
;; script: fennel

(local scr {:w 240 :h 136})
(local transp 0)

;; Useful helpers
(fn add [a b] (+ a b))
(fn sub [a b] (- a b))
(fn half [v] (/ v 2))

(fn even? [v] (= 0 (% v 2)))
(fn odd? [v] (= 1 (% v 2)))

(fn incr [v a] (+ v (or a 1)))
(fn decr [v a] (- v (or a 1)))

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

;; Sprite map
(local sp {:green 2
           :blue 4
           :bg 34})

;; General
(local air sp.blue)
(local map {:w 9 :h 6 :dx 3 :thr 0.278 :wrap true})
(local plr {:y 0 :x 7})
(var time 0)

;; Buttons
(local bt (rev-idx [:u :d :l :r :x :z :a :s]))

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

(fn alt-row-offset [plr]
    (hex-offset 1 (~= hex.even (odd? plr.y))))

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
     (iv? x map.dx (+ map.dx map.w))
     (iv? y 0 map.h)))

(fn draw-map [cells]
    "Draw hexagonal grid"
    (var i 0)
    (for [y 0 (- map.h 1)]
         (for [x 0 (- map.w 1)]
              (set i (+ i 1))
              (sp-draw (. cells i)
                       {: y :x (+ x map.dx)}))))

(fn draw-player [plr]
    "Draw player"
    (let [{: y : x} plr
          id 2 ;;(+ 2 (* (// (% time 60) 30) 2))
          ]
      (printc (.. :player " x: " x " y: " y) (half scr.w) (- scr.h 10) 15)
      (spr id
           (* x hex.col)
           (* y hex.row)
           transp 1 0 0 2 2)))

(fn move-player [plr dir cells]
    "Move player to some direction"
    (let [y (if map.wrap
                (% (+ plr.y dir.y) map.h)
                (+ plr.y dir.y))
          x (if map.wrap
                (+ (% (- (+ plr.x dir.x) map.dx) map.w) map.dx)
                (+ plr.x dir.x))
          ty (math.floor y)
          tx (math.floor (- (+ 1 x) map.dx))]
      (if (and (in-map? y x)
               (can-move? cells ty tx))
          (do
           (tset plr :y y)
           (tset plr :x x))
          (printc (.. "Can not move to (:y " ty " :x " tx ")") (half scr.w) (- scr.h 30) 12)))
    plr)

(fn dir-events [plr]
    "Get directions from button events"
    (local dir {:y 0 :x 0})
    (local ox (alt-row-offset plr))
    (when (btnp bt.l) (do (btd :l) (tset dir :x (- 1))))
    (when (btnp bt.r) (do (btd :r) (tset dir :x (+ 1))))
    (when (btnp bt.u) (do (btd :l) (tset dir :x ox) (tset dir :y (- 1))))
    (when (btnp bt.d) (do (btd :r) (tset dir :x ox) (tset dir :y (+ 1))))
    (when (btnp bt.a) (do (btd :a) (tset dir :x (- 0.5)) (tset dir :y (- 1))))
    (when (btnp bt.s) (do (btd :s) (tset dir :x (+ 0.5)) (tset dir :y (- 1))))
    (when (btnp bt.x) (do (btd :y) (tset dir :x (- 0.5)) (tset dir :y (+ 1))))
    (when (btnp bt.z) (do (btd :x) (tset dir :x (+ 0.5)) (tset dir :y (+ 1))))
    dir)

(local cells (gen-map (* map.w map.h) map.thr))

(global
 TIC
 (fn tic []
     (cls 0)
     (draw-map cells)

     (local dir (dir-events plr))

     (move-player plr dir cells)
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

