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

;; Sprite map
(local sp {:green 2
           :blue 4
           :bg 34})

;; Hexagon grid
(local size 7)
(local sq3 (math.sqrt 3))
(local hex {:kind :pointy
            :w (* size 2) ; width
            :h (math.floor (* size sq3)) ; height
            :sp 2 ; spacing
                  })

;; Hex algorithms from https://www.redblobgames.com/grids/hexagons/

(fn hex-offset [map key ?even]
    (let [f (if ?even add sub)
          v (. map key)
          o (half (band v 1))]
      (f v o)))

(lambda cube-y [x z]
  (- (- x) z))

(fn cube->axial [cube]
    (let [{:x q
           :z r} cube]
      {: q : r}))

(fn axial->cube [ax]
    (let [{:q x
           :r z} ax]
      {: x : z :y (cube-y x z)}))

(fn cube->hex [cube kind ?even]
    (let [{:x col
           :z row} cube]
      (match kind
             :flat   {:col (+ col (hex-offset cube :z ?even)) : row}
             :pointy {:row (+ row (hex-offset cube :x ?even)) : col})))

(fn hex->cube [hx kind ?even]
    (let [{:col col
           :row row} hx]
      (match kind
             :flat   (let [x (- col (hex-offset hx :row ?even))
                           z row]
                       {: x : z :y (cube-y x z)})
             :pointy (let [x col
                           z (- row (hex-offset hx :col ?even))]
                       {: x : z :y (cube-y x z)}))))

(lambda hex->axial [hx kind ?even]
  (-> (hex->cube hx kind ?even)
      (cube->axial)))

(lambda axial->hex [ax kind ?even]
  (-> (axial->cube ax)
      (cube->hex kind ?even)))

;; Game

(fn draw-grid [id cell]
    (let [{:row x :col y} cell]
      (spr id
           (* x (+ hex.w hex.sp))
           (* y (+ hex.h hex.sp))
           transp 1 0 0 2 2)))

(fn printc [msg x y]
    "Print message centered on coordinates"
    (local width (print msg 0 scr.h))
    (print msg (- x (half width)) y))

(fn hello []
    (printc "HEXAGONAL WORLD!" (half scr.w) 84))


(local plr {:x 96
            :y 28})

(var time 0)

(global
 TIC
 (fn tic []
     (when (btn 0) (tset plr :y (- plr.y 1)))
     (when (btn 1) (tset plr :y (+ plr.y 1)))
     (when (btn 2) (tset plr :x (- plr.x 1)))
     (when (btn 3) (tset plr :x (+ plr.x 1)))

     (cls 0)

     ;; Draw hexagonal grid

     (local cells
            [{:row 0 :col 0}
             {:row 1 :col 0}
             {:row 2 :col 0}
             {:row 0.5 :col 1}
             {:row 1.5 :col 1}
             {:row 1 :col 2}])

     (each [_ cell (ipairs cells)]
           (draw-grid sp.bg cell))

     (for [q 0 3]
          (for [r 3 6]
               (draw-grid sp.blue (axial->hex {: q : r} hex.kind))))

     (draw-grid sp.green (axial->hex {:q 1 :r 3} hex.kind))

     ;; Draw player
     (spr (+ 2 (* (// (% time 60) 30) 2))
          plr.x plr.y transp 1 0 0 2 2)

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

;; <MAP>
;; 000:00000000000000ffff00000000ffffffff0123456789ab00effe00ba987600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c22d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000500002000000100002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013
;; 002:000000000000203020300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 003:000000000000213121310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050
;; 004:0000000000000020302030203020300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ea
;; 005:000000000000002131213121312131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 006:000000000000000020302030203020302030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 007:000000000000000021312131213121312131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 008:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000095
;; 009:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003383000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000027
;; 010:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e23c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a5
;; 011:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007783000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000057
;; 012:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000344700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008300c100a80100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000095
;; 013:00000000000000000000000000000000000000000000000000000000000005b50000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ba8300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002c0000000047000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c4
;; 014:00000000000000000000000000000000000000000000000000000000000056c60000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008300b2009808000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b5
;; 015:0000000000000000000000000000000000000000000000000000000000000be5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ab0a6074011600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:000000000000000000000000000000000000000000000000000000000000f5f500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 017:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 018:0000000000000000000000000000000000000000000000000000000000008d130000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000044000000a600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 019:000000000000000000000000000000000000000000000000000000000000099500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 020:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 021:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000044000000e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 022:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000085c764018100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 030:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006fcf008f00000000000000000000000000000000000000000000016400
;; 031:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008100000000000000000000000000000000000000000000010000
;; 032:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000022953500440000000000000083c37401000000006400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008600000000000000000000000000000000000000000000016400
;; 033:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007bfcf00859f64012295350044000000000000000c4d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008100000000000000000000000000000000000000000000004400
;; 034:00000000000000000000000000000000000000000000000000000000000044000000000000000caa7401086554016400000007bfcf000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008a1c64018f5274016400000007bfcf000c9b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002c6963700000000000000000000000000000000000000000000471626
;; 035:c600000000000000000000000000000000000000000000000000000000000202020266f62702b6c2020716470296e602071696273700000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020296660282f5f566e6c6f576c6f62616c6f5f5370000000000000000000000000000000000000000000000000000000096e6768207164792929202478656e6a002020202000000000000000000000000000000000000000000000000000000000000000000000000000000000000004602275600000000000000000000000000000000000000000000f63796
;; 036:4700000000000000000000000000000000000000000000000000000000002696e64696e67637c202071647475627e6b582b602b2020000000000000000000000000000000000000000000000000000000000e637562747822696e64696e67637c202b7c6963747823797d682723756c65636477292c202b60000000000000000000000000000000000000000000000000000000057e6071636b67292c2023797d68272471626c656e257e6071636b6729292c2026716c692000000000000000000000000000000000000000000000000000000272202d3000000000000000000000000000000000000000000000247f6
;; 037:370000000000000000000000000000000000000000000000000000000000e6a0020202020202020202022756475727e602e696c6a0020202020202020256c63756a002020202020202020202c6f63616c6023757266716c602d302c6963747823797d68272e27292c2026716c6c202b692a00202020202020000000000000000000000000000000000000000000000000000000096e64696e6763702d302d616473686f5071647475627e682b73757266716c6d7c2020700000000000000000000000000000000000000000000000000000002471626c6000000000000000000000000000000000000000000002636f6
;; 038:e6000000000000000000000000000000000000000000000000000000000002960716962737823757262696e64696e67637920246f6a0020202020202020202020202471626c656e296e637562747822696e64696e67637c2022692000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0020202020202f573f58292a00202020256e646a0020202022756475727e60236f6e6000000000000000000000000000000000000000000000000000000005000880000000000000000000000000000000000000000000000000000
;; 039:000000000000000000000000000000000000000000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000008f73740171c63500640000008d6364018953640122000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007bfcf008300000000000000000000000000000000000000000000000000
;; 040:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004b7401830000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 041:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006401894374000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 043:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 045:050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003ef7000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 046:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001fffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 047:e40000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 050:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000440000008000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

