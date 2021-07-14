;; title:  Hexagonal map
;; author: peterhil
;; desc:   Hexagonal map demo
;; script: fennel

;; Fennel 0.9.0 collect and icollect
;; MIT Licensed: Copyright © 2016-2021 Calvin Rose and contributors
;; https://fennel-lang.org/reference#collect-icollect-table-comprehension-macros

(macros
 {
  :collect
  (fn collect* [iter-tbl key-value-expr ...]
      "Returns a table made by running an iterator and evaluating an expression
    that returns key-value pairs to be inserted sequentially into the table.
    This can be thought of as a \"table comprehension\". The provided key-value
    expression must return either 2 values, or nil."
      (assert (and (sequence? iter-tbl) (>= (length iter-tbl) 2))
              "expected iterator binding table")
      (assert (not= nil key-value-expr) "expected key-value expression")
      (assert (= nil ...)
              "expected exactly one body expression. Wrap multiple expressions with do")
      `(let [tbl# {}]
         (each ,iter-tbl
               (match ,key-value-expr
                      (k# v#) (tset tbl# k# v#)))
         tbl#))

  :icollect
  (fn icollect* [iter-tbl value-expr ...]
      "Returns a sequential table made by running an iterator and evaluating an
    expression that returns values to be inserted sequentially into the table.
    This can be thought of as a \"list comprehension\".

    For example,
      (icollect [_ v (ipairs [1 2 3 4 5])] (when (> v 2) (* v v)))
    returns
      [9 16 25]"
      (assert (and (sequence? iter-tbl) (>= (length iter-tbl) 2))
              "expected iterator binding table")
      (assert (not= nil value-expr) "expected table value expression")
      (assert (= nil ...)
              "expected exactly one body expression. Wrap multiple expressions with do")
      `(let [tbl# []]
         (each ,iter-tbl
               (tset tbl# (+ (length tbl#) 1) ,value-expr))
         tbl#))
  })

;; Initialise random number seed – otherwise the seed is constant.
;; Read more at http://lua-users.org/wiki/MathLibraryTutorial
(math.randomseed ((or _G.time os.time)))

(local trace (or _G.trace print))

(local pi2 (* 2 math.pi))
(local sq3 (math.sqrt 3))

;; Config ----------------
(local scr {:w 240 :h 136})
(local map {:w 9  :h 6
            :dx 3 :dy 0
            :thr 0.278
            :gems 0.12
            :wrap true})

;; Palette
(local transp 0)

;; Sprites
(local sp {:green {:id 2 :tp transp}
           :blue {:id 4 :tp transp}
           :bg {:id 34 :tp transp}
           :gem {:id 66 :tp 11}
           :hl {:id 290 :tp transp}})
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

(local inf math.huge)
(local nan (/ inf inf))
(fn nan? [v] (~= v v))

(fn sign [v]
    (if (= v 0) 0
        (nan? v) nan
        (> v 0) 1
        (- 1)))
(fn even? [v] (= 0 (% v 2)))
(fn odd? [v] (= 1 (% v 2)))
(fn incr [v a] (+ v (or a 1)))
(fn decr [v a] (- v (or a 1)))
(fn half [v] (/ v 2))
(fn << [b disp] (math.floor (% (* b (^ 2 (math.floor disp))) (^ 2 32))))
(fn >> [b disp] (math.floor (/ (% b (^ 2 32)) (^ 2 disp))))

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

(lambda iv? [v low high]
  "Is value within half open interval"
  (and (>= v low) (< v high)))

;; Obscure math

(fn floor-multiply [n v]
    "It’s like the wheel of fortune: Multiply value with base n, add 1/nth
and floor the result to get the correct sector on the wheel."
    ;; TODO For flat oriented hexagons, do not add the 1/nth
    (math.floor (+ (* v n) (/ 1 n))))

(fn nstep [n v]
    "Floor rounding in one nth steps so you get zero on interval [± 1/(n*2)).
In absolute value, each increment of 1/n increases the result by 1/nth."
    (/ (floor-multiply n v) n))

(local halfstep (partial nstep 2))
(local hexstep  (partial nstep 6))

(fn sector [n v]
    "Divides circle into n sector and tells on which the value v lands on.
Sectors are numbered counter-clockwise from (:x 1 :y 0)."
    (% (floor-multiply n v) n))

(local sextant (partial sector 6))

(fn chexp [theta ?abs]
    "Complex number exponential in polar coordinates,
but rounded to multiples of 0.5 so it works on hexagonal grid"
    (let [w (* pi2 theta)]
      (values (halfstep (* (or ?abs 1) (math.cos w)))
              (halfstep (* (or ?abs 1) (math.sin w))))))


;; Complex numbers ----------------
(local cx {})
(local cx-meta {})

(lambda cx.new [x ?y]
  "Complex number vector"
  (let [v {:x x
           :y (or ?y 0)}]
    (setmetatable v cx-meta)))

(fn cx.type [a]
    (match (getmetatable a)
           cx-meta :complex
           _ (type a)))

(fn cx.from [num ?imag]
    "Return complex number from numeric argument"
    (let [cm cx-meta]
      (match [(type num) (getmetatable num)]
             [:table cm] num
             [:table  _] (let [{: x : y} num] (cx.new x y))
             [:number _] (cx.new num
                                 ;; TODO Check type, non-numbers pass through as nils
                                 (tonumber (or ?imag 0)))
             [:nil nil] (error (.. "Nil given to cx.from"))
             [_ _] (error (.. "Can’t make a complex number from: " num)))))

(fn cx.abs [a]
    (let [a (cx.from a)]
      (math.sqrt (+ (^ a.x 2) (^ a.y 2)))))

(fn cx.angle [a]
    (let [a (cx.from a)]
      (math.atan a.y a.x)))

(lambda cx.equals [a b]
  (let [a (cx.from a)
        b (cx.from b)]
    (and (= a.x b.x)
         (= a.y b.y))))

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
(tset cx :equals cx.equals)

(tset cx-meta :__call (fn __call [_ x ?y] (cx.from x ?y)))
(tset cx-meta :abs cx.abs)
(tset cx-meta :angle cx.angle)
(tset cx-meta :__add cx.add)
(tset cx-meta :__eq cx.equals)
(tset cx-meta :__mod cx.mod)
(setmetatable cx cx-meta)


;; Lib ----------------
(fn is [typ v] (= (type v) typ))

;; Iterators

(local co coroutine)

(fn count [from to ?step]
    (var i from)
    (let [inc (or ?step (sign (- to from)))]
      (assert (= (sign inc) (sign (- to from)))
              "Can‘t make a range")
      (while (< i to)
             (co.yield i i)
             (set i (incr i inc)))))

(fn irange [from to ?step]
    (local counter (co.wrap count))
    (fn [] (counter from to ?step)))

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

(fn random-piece []
    (let [dice (math.random)]
      (if (< dice map.gems) sp.gem
          (< dice map.thr) sp.bg
          sp.blue)))

(fn gen-map [n]
    (icollect [_ _ (irange 0 n)]
              (random-piece)))


;; Map and movement ----------------

(fn get-cell [pos cells]
    (let [fy (math.floor pos.y)
          fx (math.floor (incr pos.x))
          idx (+ (* fy map.w) fx)]
      (. cells idx)))

(fn can-move? [pos cells]
    (let [cell (get-cell pos cells)]
      (or (= air cell)
          (= sp.gem cell))))

(fn is-gem? [pos cells]
    (let [cell (get-cell pos cells)]
      (= sp.gem cell)))

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
          (do
           (printc (.. "Can not move to (:y " pos.y " :x " pos.x ")")
                   (half scr.w) (- scr.h 30) 12)
           plr)
          (do
           (if (not (= (cx 0) dir))
               (trace (.. "Moving to direction (:x " dir.x " :y " dir.y ") to sextant: "
                          (sextant (cx.angle dir)))))
           pos))))

(fn neighbours [pos]
    (icollect [_ phi (irange 0 1 (/ 1 6))]
              (+ (cx pos) (cx (chexp phi)))))

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
          (when (_G.btnp (. bt key))
            (do
             (btd key)
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

(fn sp-draw [sprite cell]
    "Draw sprite id on cell with x and y coordinates"
    (let [{: x : y} cell]
      (_G.spr sprite.id
           (* (+ x (hex-offset y hex.even)) hex.col)
           (* y hex.row)
           sprite.tp 1 0 0 2 2)))

(fn draw-map [cells]
    "Draw hexagonal grid"
    (var i 0)
    (for [y 0 (- map.h 1)]
         (for [x 0 (- map.w 1)]
              (set i (+ i 1))
              (let [cell (. cells i)
                    pos (cx {:y (+ y map.dy)
                             :x (+ x map.dx)})]
                (when (is-gem? {: x : y} cells)
                  (sp-draw sp.blue pos))
                (sp-draw cell pos)))))

(fn draw-player [plr]
    "Draw player"
    (let [{: y : x} plr
          id 2 ;;(+ 2 (* (// (% time 60) 30) 2))
          ]
      (printc (.. :player " x: " x " y: " y)
              (half scr.w) (- scr.h 10) 15)
      (_G.spr id
           (* hex.col (+ x map.dx))
           (* hex.row (+ y map.dy))
           transp 1 0 0 2 2)))


;; Main ----------------

(var cells (gen-map (* map.w map.h)))
(var plr (cx {:y 0 :x 7}))
(var time 0)

(global TIC (fn tic []
                (_G.cls 0)
                (draw-map cells)

                (local dir (dir-events plr))
                (set plr (new-position plr dir cells))
                (draw-player plr)
                (if (is-gem? plr cells)
                    (_G.spr sp.gem.id
                            (* hex.col (+ plr.x map.dx))
                            (* hex.row (+ plr.y map.dy))
                            sp.gem.tp
                            1 0 0 2 2))

                ;; Highlight neighbours
                (each [i cell (ipairs (neighbours plr))]
                      (sp-draw sp.hl (+ (cx cell)
                                        (cx (hex-offset cell.y (not hex.even)))
                                        {:x map.dx :y map.dy} ;; TODO Change to x and y
                                        )))

                (hello)

                (set time (+ time 1))))

{: <<
 : >>
 : cx
 : irange
 : nan
 : nan?
 : sign}

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
;; 066:bbbbbbbbbbbbbbbbbbbbbbbbbbbbffffbbbfdccdbbfdcbbcbfdcbcfebfcdcfde
;; 067:bbbbbbbbbbbbbbbbbbbbbbbbf0fbbbbbccdfbbbbedbdfbbbdcdebfbbed00dfbb
;; 082:bbfcdefdbbbfcdfebbbbfcdcbbbbbfcbbbbbbbfcbbbbbbbfbbbbbbbbbbbbbbbb
;; 083:f00dfbbbcfefbbbbfefbbbbbdfbbbbbbfbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
;; </TILES>

;; <SPRITES>
;; 034:000000dd0000dd0000dd0000dd000000d0000000d0000000d0000000d0000000
;; 035:d00000000dd00000000dd00000000dd0000000d0000000d0000000d0000000d0
;; 050:d0000000d0000000d0000000d0000000dd00000000dd00000000dd00000000dd
;; 051:000000d0000000d0000000d0000000d000000dd0000dd0000dd00000d0000000
;; </SPRITES>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

