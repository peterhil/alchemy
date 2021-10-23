;; title:  Alchemy
;; author: peterhil
;; desc:   Alchemy of the soul and the holy sephiroth
;; script: fennel
;;
;; MIT Licensed: Copyright © 2021 Peter H.

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

(var level 6)

(local scr {:w 240 :h 136})
(local orientation ; of hexagons
       :flat
       ;; :pointy
       )
(local map
       {:w 7 :h 5
        :dx 4 :dy 0
        :thr 0.278
        :gems 0.12
        ;; :style :normal
        :style :starry
        })

;; Palette
(local transp 0)

;; Sprites
(local sp-offset
       (match [orientation map.style]
              [:flat :normal] 96
              [:pointy :starry] 96
              0))

(local sp {:green {:id 2 :tp transp}
           :blue {:id 4 :tp transp}
           :purple {:id 6 :tp transp}
           :dim {:id 34 :tp transp}
           :gem {:id 322 :tp 11}
           :moon {:id 324 :tp 11}
           :mercurius {:id 326 :tp 11}
           :venus {:id 328 :tp 11}
           :sun {:id 330 :tp 11}
           :hl {:id 290 :tp transp}})
(local air sp.blue)

(local things [sp.gem sp.moon sp.mercurius sp.venus sp.sun])

;; Grid settings
(local size 7)
(local hex {:w ; width
               (match orientation
                      :pointy (* size 2)
                      :flat (math.floor (* size sq3)))
            :h ; height
               (match orientation
                      :pointy (math.floor (* size sq3))
                      :flat (* size 2))
            :sp 3 ; spacing
            :even false})
(tset hex :col (+ hex.w hex.sp))
(tset hex :row (+ hex.h hex.sp))

;; Buttons
(local bt {:u 0 :d 1
           :l 2 :r 3
           :x 4 :z 5
           :a 6 :s 7})

(local directions
       {:r (/  0 24) :z (/ (match orientation :pointy 4 :flat 2) 24)
        :d (/  6 24) :x (/ (match orientation :pointy 8 :flat 10) 24)
        :l (/ 12 24) :a (/ (match orientation :pointy 16 :flat 14) 24)
        :u (/ 18 24) :s (/ (match orientation :pointy 20 :flat 22) 24)})


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
(fn incr [v ?a] (+ v (or ?a 1)))
(fn decr [v ?a] (- v (or ?a 1)))
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

(fn cx.mul [a b]
    (let [a (cx.from a)
          b (cx.from b)
          v {:x (- (* a.x b.x)
                   (* a.y b.y))
             :y (+ (* a.x b.y)
                   (* a.y b.x))}]
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
(tset cx :mul cx.mul)
(tset cx :equals cx.equals)
(tset cx :i (cx.new 0 1))

(tset cx-meta :__call (fn __call [_ x ?y] (cx.from x ?y)))
(tset cx-meta :abs cx.abs)
(tset cx-meta :angle cx.angle)
(tset cx-meta :__add cx.add)
(tset cx-meta :__eq cx.equals)
(tset cx-meta :__mod cx.mod)
(tset cx-meta :__mul cx.mul)
(setmetatable cx cx-meta)


;; Lib ----------------

(fn is [val]
    (fn [thing] (= val thing)))

(fn ifilter [pred seq]
    (icollect [_ v (ipairs seq)]
              (when (pred v) v)))

(fn filter [pred seq]
    (collect [k v (pairs seq)]
             (when (pred v k)
               (values k v))))

(fn find [pred tbl]
  (each [key val (pairs tbl)]
    (when (pred val)
      (lua "return key"))))

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

(fn odd-offset [v ?sub]
    "Alternate odd rows on grid"
    (let [f (if ?sub sub add)
          o (/ (math.abs (% v 2)) 2)]
      (f 0 o)))

(fn hex-offset [cell]
    "Adjust odd rows/cols on hexagonal map"
    (match orientation
           :pointy (cx.add cell (odd-offset cell.y hex.even))
           :flat   (cx.add cell (cx.new 0 (odd-offset cell.x hex.even)))))

(local odd-edge-directions
       {:r (cx  1 0) :z (match orientation :pointy (cx  1  1) :flat (cx  1  1))
        :d (cx  0 1) :x (match orientation :pointy (cx -1  1) :flat (cx -1  1))
        :l (cx -1 0) :a (match orientation :pointy (cx -1 -1) :flat (cx -1 -1))
        :u (cx 0 -1) :s (match orientation :pointy (cx  1 -1) :flat (cx  1 -1)) })

(fn odd-col? [plr]
    (~= hex.even (odd? plr.x)))

(fn odd-row? [plr]
    (~= hex.even (odd? plr.y)))

(fn random-piece []
    (let [dice (math.random)]
      (if (< dice map.gems) sp.gem
          (< dice map.thr) sp.dim
          air)))

(fn random-index [seq]
    (math.random (length seq)))

(fn random-sample [seq]
    (. seq (random-index seq)))

(fn gen-map [n]
    (icollect [_ _ (irange 0 n)]
              (random-piece)))


;; Map and movement ----------------

(local origin (cx.new map.dx map.dy))

(fn cell-index [pos cells]
    (let [fy (math.floor pos.y)
          fx (math.floor (incr pos.x))
          idx (+ (* fy map.w) fx)]
      idx))

(fn get-cell [pos cells]
    (. cells (cell-index pos cells)))

(fn can-move? [pos cells]
    (let [cell (get-cell pos cells)]
      (or (= air cell)
          (= sp.gem cell)
          (= sp.moon cell)
          (= sp.mercurius cell)
          (= sp.venus cell)
          (= sp.sun cell))))

(fn free-cell? [pos cells]
    (let [cell (get-cell pos cells)]
      (= air cell)))

(fn is-gem? [pos cells]
    (let [cell (get-cell pos cells)]
      (or (= sp.gem cell)
          (= sp.moon cell)
          (= sp.mercurius cell)
          (= sp.venus cell)
          (= sp.sun cell))))

(fn in-map? [pos]
    (and
     (iv? pos.x 0 map.w)
     (iv? pos.y 0 map.h)))

(fn odd-map? []
    (match orientation
           :flat (odd? map.w)
           :pointy (odd? map.h)
           false))

(fn on-smooth-edge? [plr]
    "Is the player on the smooth edge of the map?"
    (match orientation
           :flat (or (= plr.x 0)
                     (= plr.x (decr map.w)))
           :pointy (or (= plr.y 0)
                       (= plr.y (decr map.h)))
           false))

(fn collision? [pos cells]
    (or (not (in-map? pos))
        (not (can-move? pos cells))))

(fn allowed-directions [level]
    (if (<= level 9) ; Yesod
        directions
        (collect [key dir (pairs directions)]
                 (when (and (~= key :l)
                            (~= key :r))
                   (values key dir)))))

(fn wrap-map? [level]
    (<= level 6)) ; Tiphereth

(fn move [val dir]
    "Move complex value to some direction on map"
    (let [cxmap (cx.new map.w map.h)]
      (if (wrap-map? level)
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

(fn neighbours [pos]
    (let [phase (if (= orientation :flat) (/ 1 12) 0)]
      (icollect [_ phi (irange 0 1 (/ 1 6))]
                (+ (cx pos)
                   (cx (chexp (+ phi phase)))))))

(fn deviation [plr key]
    "Angle deviation for up/down (or left/right) movement to align with hex grid
on alternate rows (cols)"
    (match [orientation key]
           [:flat :l]   (if (odd-col? plr) (/ 1 12) (/ -1 12))
           [:flat :r]   (if (odd-col? plr) (/ -1 12) (/ 1 12))
           [:pointy :u] (if (odd-row? plr) (/ -1 12) (/ 1 12))
           [:pointy :d] (if (odd-row? plr) (/ 1 12) (/ -1 12))
           0))

(fn hex-move [plr angle key]
    "Get next position for the player based on angle of movement"
    (let [phi (+ angle (deviation plr key))
          next-pos (cx (chexp phi))]
      (if (and (odd-map?) (on-smooth-edge? plr))
          (if (in-map? (+ plr next-pos))
              next-pos
              (. odd-edge-directions key))
          next-pos)))

(fn dir-events [plr]
    "Get directions from button events.
Uses polar coordinates and converts to cartesian."
    (local moves [])
    (each [key angle (pairs (allowed-directions level))]
          (when (_G.btnp (. bt key))
            (do (btd key)
                (table.insert moves (hex-move plr angle key)))))
    (cx (add (table.unpack moves))))

(fn note-for [thing]
    (+ 56 (* 4 (find (is thing) things))))


;; Game logic -------

(local cheat true)
(local keys {:comma 45 :period 46})
(local hold 60)
(local period 60)

(local sephiroth
       [{:name :keter     :pos (cx  0 0.5) :sp sp.dim}
        {:name :chochmah  :pos (cx  1 1) :sp sp.dim}
        {:name :binah     :pos (cx -1 1) :sp sp.dim}
        {:name :chesed    :pos (cx  1 2) :sp sp.dim}
        {:name :gewurah   :pos (cx -1 2) :sp sp.dim}
        {:name :tiphereth :pos (cx  0 2.5) :sp sp.sun}
        {:name :nezach    :pos (cx  1 3) :sp sp.venus}
        {:name :hod       :pos (cx -1 3) :sp sp.mercurius}
        {:name :jesod     :pos (cx  0 3.5) :sp sp.moon}
        {:name :malkuth   :pos (cx  0 4.5)  :sp sp.gem}])

(local balance
       {:malkuth 0
        :jesod 0
        :hod 0
        :nezach 0
        :tiphereth 0
        :gewurah 0
        :chesed 0
        :binah 0
        :chochmah 0
        :keter 0})


;; Side effects --------

(fn hello []
    (printc "ALCHEMY" (half scr.w) (- scr.h 20) 12))

(fn sp-draw [sprite cell]
    "Draw sprite id on cell with x and y coordinates"
    (let [{: x : y} cell]
      (_G.spr (+ sprite.id sp-offset)
           (* x hex.col)
           (* y hex.row)
           sprite.tp 1 0 0 2 2)))

(fn draw-map [cells]
    "Draw hexagonal grid"
    (var i 0)
    ; TODO Calculate indices only once
    (for [y 0 (- map.h 1)]
         (for [x 0 (- map.w 1)]
              (set i (+ i 1))
              (let [cell (. cells i)
                    pos (cx {:y (+ y map.dy)
                             :x (+ x map.dx)})]
                (when (is-gem? {: x : y} cells)
                  (sp-draw air (hex-offset pos)))
                (sp-draw cell (hex-offset pos))))))

(fn draw-player [plr]
    "Draw player"
    (let [{: y : x} plr
          id 2 ;;(+ 2 (* (// (% time 60) 30) 2))
          ]
      ;; (printc (.. :player " x: " x " y: " y)
      ;;         (half scr.w) (- scr.h 10) 15)
      (_G.spr (+ id sp-offset)
           (* x hex.col)
           (* y hex.row)
           transp 1 0 0 2 2)))

(fn draw-neighbours [pos]
    "Highlight neighbours"
    (each [i cell (ipairs (neighbours pos))]
          (sp-draw sp.hl cell)))

(fn draw-sephiroth []
    (each [idx sephirah (ipairs sephiroth)]
          (let [origin (cx 1 0)
                colour (if (<= level idx) sp.green sp.purple)]
            (sp-draw colour (+ origin sephirah.pos)))))

(fn draw-balance []
    (each [idx sephirah (ipairs sephiroth)]
          (let [origin (cx 12 0)
                offset (cx hex.col (/ hex.row 2))
                pos (+ origin
                       (cx (* 2 (odd-offset idx hex.even))
                           (* 0.5 (decr idx))))
                x (+ offset.x (* hex.col (incr origin.x)) hex.sp)
                y (+ offset.y (* hex.row pos.y) (- hex.sp))
                name (. sephirah :name)
                count (or (. balance name) "?")
                abbrev (.. (: name :sub 1 1) (: name :sub 3 3))
                sprite sephirah.sp]
            (do
             (sp-draw sprite pos)
             (print (.. abbrev ": " count) x y 12)))))


;; Main ----------------

(var cells (gen-map (* map.w map.h)))
(var plr (cx {:y 0 :x 0}))
(var time 0)

(global TIC (fn tic []
                (_G.cls 0)

                (when cheat
                  (do
                   (when (_G.keyp keys.comma hold period)
                     (set level (math.min 10 (incr level))))
                   (when (_G.keyp keys.period hold period)
                     (set level (math.max 1 (decr level))))))

                (draw-map cells)
                (draw-sephiroth)
                (printc (. (. sephiroth level) :name) 24 100 5)

                (local dir (dir-events plr))
                (set plr (new-position plr dir cells))

                (draw-player (+ origin plr))

                (if (is-gem? plr cells)
                    (let [thing (get-cell plr cells)
                          idx (cell-index plr cells)]
                      (tset cells idx air)

                      ;; Play sound FX
                      (_G.sfx 1 (note-for thing) 15)

                      ;; Animate collecting for some frames
                      ;; (sp-draw sp.hl (+ origin plr))

                      ;; Add new gem to table
                      ;; TODO Use some distribution to select the level
                      (let [ether (filter (is air) cells)
                            space (icollect [k v (pairs ether)] k)
                            idx (random-sample space)
                            gem (random-sample things)]
                        (tset cells idx gem))))

                ;; Draw balance
                (draw-balance)

                ;; (draw-neighbours (+ origin plr))

                (hello)

                (set time (+ time 1))))

{: <<
 : >>
 : cx
 : filter
 : find
 : ifilter
 : irange
 : nan
 : nan?
 : odd?
 : random-index
 : random-sample
 : sign}

;; <TILES>
;; 002:0000006500006555006555556555555555555555555555555555555555555555
;; 003:6000000055600000555560005555556055555550555555505555555055555550
;; 004:000000ab0000abbb00abbbbbabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
;; 005:a0000000bba00000bbbba000bbbbbba0bbbbbbb0bbbbbbb0bbbbbbb0bbbbbbb0
;; 006:0000000000000811000811110081111100111111081111110111111101111111
;; 007:0000000011800000111180001111180011111100111111801111111011111110
;; 018:5555555555555555555555555555555565555555006555550000655500000065
;; 019:5555555055555550555555505555555055555560555560005560000060000000
;; 020:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbabbbbbbb00abbbbb0000abbb000000ab
;; 021:bbbbbbb0bbbbbbb0bbbbbbb0bbbbbbb0bbbbbba0bbbba000bba00000a0000000
;; 022:0111111101111111081111110011111100811111000811110000081100000000
;; 023:1111111011111110111111801111110011111800111180001180000000000000
;; 034:000000ff0000ff0000ff0000ff000000f0000000f0000000f0000000f0000000
;; 035:f00000000ff00000000ff00000000ff0000000f0000000f0000000f0000000f0
;; 050:f0000000f0000000f0000000f0000000ff00000000ff00000000ff00000000ff
;; 051:000000f0000000f0000000f0000000f000000ff0000ff0000ff00000f0000000
;; 098:0006555500055555006555550055555506555555055555556555555555555555
;; 099:5555600055555000555556005555550055555560555555505555555655555555
;; 100:000abbbb000bbbbb00abbbbb00bbbbbb0abbbbbb0bbbbbbbabbbbbbbbbbbbbbb
;; 101:bbbba000bbbbb000bbbbba00bbbbbb00bbbbbba0bbbbbbb0bbbbbbbabbbbbbbb
;; 102:0000000000000811000811110081111100111111081111110111111101111111
;; 103:0000000011800000111180001111180011111100111111801111111011111110
;; 114:6555555505555555065555550055555500655555000555550006555500000000
;; 115:5555555655555550555555605555550055555600555550005555600000000000
;; 116:abbbbbbb0bbbbbbb0abbbbbb00bbbbbb00abbbbb000bbbbb000abbbb00000000
;; 117:bbbbbbbabbbbbbb0bbbbbba0bbbbbb00bbbbba00bbbbb000bbbba00000000000
;; 118:0111111101111111081111110011111100811111000811110000081100000000
;; 119:1111111011111110111111801111110011111800111180001180000000000000
;; 130:000fffff000f000000f0000000f000000f0000000f000000f0000000f0000000
;; 131:fffff0000000f00000000f0000000f00000000f0000000f00000000f0000000f
;; 146:f00000000f0000000f00000000f0000000f00000000f0000000fffff00000000
;; 147:0000000f000000f0000000f000000f0000000f000000f000fffff00000000000
;; </TILES>

;; <SPRITES>
;; 034:000000dd0000dd0000dd0000dd000000d0000000d0000000d0000000d0000000
;; 035:d00000000dd00000000dd00000000dd0000000d0000000d0000000d0000000d0
;; 050:d0000000d0000000d0000000d0000000dd00000000dd00000000dd00000000dd
;; 051:000000d0000000d0000000d0000000d000000dd0000dd0000dd00000d0000000
;; 066:bbbbbbbbbbbbbbbbbbbbbbbbbbbbffffbbbfdccdbbfdcbbcbfdcbcfebfcdcfde
;; 067:bbbbbbbbbbbbbbbbbbbbbbbbf0fbbbbbccdfbbbbedbdfbbbdcdebfbbed00dfbb
;; 068:bbbbbbbbbbbbbbbbbbbb8884bbb81881bb818881bb188818b1811111b1888188
;; 069:bbbbbbbbbbbbbbbb449bbbbbcc49bbbbccc43bbb1cc49bbb84c493bb844c49bb
;; 070:bbbbbbbbbbbbbbbbbbbb8119bbb84999bb8c1444bb14c111b1941c44b1941444
;; 071:bbbbbbbbbbbbbbbb996bbbbb4496bbbb14996bbb44496bbb1c4996bb14c496bb
;; 072:bbbbbbbbbbbbbbbbbbbb000fbbb07666bb076555bb065666b0f65666b0766565
;; 073:bbbbbbbbbbbbbbbb7f0bbbbb6570bbbb66570bbb5665fbbb56657fbb66c677bb
;; 074:bbbbbb94b2bbb34c21282333b2129999b8294444b2944c49b394c444b3944c44
;; 075:9bbbbbbb43bbbabb3a29abab99921abb949929bb444932bb4c499abb44c493bb
;; 082:bbfcdefdbbbfcdfebbbbfcdcbbbbbfcbbbbbbbfcbbbbbbbfbbbbbbbbbbbbbbbb
;; 083:f00dfbbbcfefbbbbfefbbbbbdfbbbbbbfbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
;; 084:b1888888b1188888bb111811bb181118bbb18884bbbb1199bbbbbbbbbbbbbbbb
;; 085:844449bb4c4c43bbc4c49bbbcc493bbb4493bbbb933bbbbbbbbbbbbbbbbbbbbb
;; 086:b1944111b1944414bb144111bb894414bbb89999bbbb8199bbbbbbbbbbbbbbbb
;; 087:4c4996bbccc496bb4c493bbb44932bbb9932bbbb922bbbbbbbbbbbbbbbbbbbbb
;; 088:b0766656b0f66555bb066656bb876666bbb86676bbbb8717bbbbbbbbbbbbbbbb
;; 089:66456fbb64c470bb66461bbbc4610bbb6610bbbb110bbbbbbbbbbbbbbbbbbbbb
;; 090:b394c4c4b3944c4cb23444ccb2934c44b43939394c422333b4bbb921bbbbbb92
;; 091:4c4993bbc4c495bb444934bbc49325bb993216bb3545656b29bbb6bb9bbbbbbb
;; 100:bbbbbbbbbbbbbbbbbbbbffffbbbf3331bbf39499bb394c49bf34c444bf344c44
;; 101:bbbbbbbbbbbbbbbbfffbbbbb888fbbbb1188fbbb9818fbbb91188fbb91818fbb
;; 106:bbbbbb94b2bbb34c23212933b2839999b1394444b2944c49b994c444b3944c44
;; 107:9bbbbbbb43bbbabb3a29abab99921abb949929bb444932bb4c499abb44c493bb
;; 116:bf34c4c9bf394c49bbf94499bbf39991bbbf3318bbbbffffbbbbbbbbbbbbbbbb
;; 117:91188fbb11818fbb1818fbbb118ffbbb88ffbbbbfffbbbbbbbbbbbbbbbbbbbbb
;; 122:b394c4c4b3944c4cb43444ccb9234c44b43239394c494333b4bbb921bbbbbb92
;; 123:4c4993bbc4c495bb444934bbc49325bb993216bb3545656b29bbb6bb9bbbbbbb
;; 162:bbbbbbbbbbbbbbbbbbbbbbbbbbbbffffbbbfdccdbbfdcbbcbfdcbcfebfcdcfde
;; 163:bbbbbbbbbbbbbbbbbbbbbbbbf0fbbbbbccdfbbbbedbdfbbbdcdebfbbed00dfbb
;; 164:bbbbbbbbbbbbbbbbbbbb0003bbb08883bb081111bb811811b0818111b0811811
;; 165:bbbbbbbbbbbbbbbb333bbbbb9993bbbb44993bbb14493bbb1c4993bb14c493bb
;; 170:bbbbbbbbbbbbbbbbbbbb3333bb539999bb394444b5944c49b394c444b3944c44
;; 171:bbbbbbbbbbbbbbbb333bbbbb99935bbb94993bbb444935bb4c4993bb44c493bb
;; 178:bbfcdefdbbbfcdfebbbbfcdcbbbbbfcbbbbbbbfcbbbbbbbfbbbbbbbbbbbbbbbb
;; 179:f00dfbbbcfefbbbbfefbbbbbdfbbbbbbfbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
;; 180:b0818181b0811818bb011188bb081884bbb08839bbbb0033bbbbbbbbbbbbbbbb
;; 181:1c4993bbc4c493bb44493bbbc4933bbb9933bbbb333bbbbbbbbbbbbbbbbbbbbb
;; 186:b394c4c4b3944c4cb53444ccbb394c44bb539939bbbb3333bbbbbbbbbbbbbbbb
;; 187:4c4993bbc4c493bb444935bbc4933bbb99335bbb333bbbbbbbbbbbbbbbbbbbbb
;; </SPRITES>

;; <WAVES>
;; 000:0877655444445679aaa9999876543218
;; 001:f32788abd3f1777778dfaa6788d84887
;; 002:0770708095789bbb99abccbaaa000000
;; 005:007788808400040009000e0008553330
;; 009:00469df00c865555678aa76444404050
;; 015:88889accdeeecba98764322246789999
;; </WAVES>

;; <SFX>
;; 000:01f711d601e201f4119f212a41a2515461c271e0815e91da91df91d391c4a185a173a16591538151817e717b716f8154a156b136d136d125e123e11158200000000f
;; 001:0ff20fc00f6e0f9c0f5c0fac0f8d0f8f0f810f420f830f630f630f630f820f710f710f110fb00f700f800fa00f800f800fa00f800f100f310fc20f666c6000000000
;; </SFX>

;; <PALETTE>
;; 000:1a1c2c592465b13053ef9948ffea28a7f07038b764257179402048faba003065de73f6f7f4f4e694b0c2566c86333c57
;; </PALETTE>

