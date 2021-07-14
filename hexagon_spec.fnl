(local busted (require :busted))
(local run (require :busted.runner))

(local r math.random)

(local hex (require :hexagon))
(local cx hex.cx)

;; Initialise random number seed – otherwise the seed is constant.
;; Read more at http://lua-users.org/wiki/MathLibraryTutorial
(math.randomseed (os.time))

(run)

(macros
 {
  :desc
  (lambda [name body ...]
    `(busted.describe
      ,name
      (fn []
          (do ,body
              ,...))))
  :it
  (lambda [description body ...]
      `(busted.it
        ,description
        (fn []
            (do ,body
                ,...))))
  })


;; Quickcheck helpers --------------------------------

(fn rnd-bool []
    (= 1 (r 0 1)))

;; Integers

(fn rnd-uint []
    (r math.maxinteger))

(fn rnd-int [?scale]
    (let [abs (or ?scale (// math.maxinteger 2))]
      (r (- abs) abs)))

;; Floats

(local max-float-exp 1024)

(fn rnd-positive-float []
    (* (r) (^ 2.0 (r 0 max-float-exp))))

(fn rnd-float [?neg]
    (let [sign (if (rnd-bool) 1 -1)]
      (* sign (rnd-positive-float))))

(fn rnd-complex []
    (let [x (rnd-float)
          y (rnd-float)]
      (cx.new x y)))

(fn rnd-complex-int [?scale]
    (let [x (rnd-int ?scale)
          y (rnd-int ?scale)]
      (cx.new x y)))


;; Tests --------------------------------

;; Math

(desc "sign"
      (local num (rnd-positive-float))
      (local fixtures
             [{:name "zero" :exp 0 :val 0}
              {:name "positive floats" :exp 1 :val num}
              {:name "negative floats" :exp (- 1) :val (* -1 num)}])
      (each [_ f (ipairs fixtures)]
            (it (.. "works with " f.name)
                (assert.are.equal
                 f.exp
                 (hex.sign f.val))))

      (it "works with nan"
          (assert.true (hex.nan? (hex.sign hex.nan)))))

;; Complex numbers

(desc "cx.new"
      (it "throws error without arguments"
          (assert.has_error
           cx.new))

      (it "with one float argument"
          (let [x (rnd-float)]
            (assert.are.equal
             {: x :y 0}
             (cx.new x))))

      (it "with one integer argument"
          (let [x (rnd-uint)]
            (assert.are.equal
             {: x :y 0}
             (cx.new x))))

      (it "with two arguments"
          (let [x (rnd-float)
                y (rnd-float)]
            (assert.are.equal
             {: x : y}
             (cx.new x y)))))

(desc "cx.type"
      (local number (rnd-float))

      (it "returns complex type"
          (assert.are.equal
           (cx.type (cx number))
           :complex))

      (it "delegates to generic type"
          (assert.are.equal
           (cx.type number)
           :number)))

(desc "cx.from"
      (local x (rnd-float))
      (local y (rnd-float))
      (local expected (cx.new x y))

      (it "complex"
          (assert.are.equal
           expected
           (cx.from (cx x y))))

      (it "table"
          (assert.are.equal
           expected
           (cx.from {: x : y})))

      (it "number"
          (assert.are.equal
           (cx.new x)
           (cx.from x)))

      (it "throws with nil"
          (assert.has_error
           (fn [] (cx.from nil))
           "Nil given to cx.from"))

      (it "throws with unknown types"
          (assert.has_error
           (fn [] (cx.from :unknown))
           "Can’t make a complex number from: unknown")))

(desc "cx.abs"
      (it "works for simple pythagorean triple"
          (assert.are.equal
           5.0
           (cx.abs (cx 3 4))))

      (it "works for unsigned integers"
          (let [number (rnd-uint)]
            (assert.are.equal
             (math.abs (* 1.0 number))
             (cx.abs number)))))

(desc "cx.equals"
      (it "with a table"
          (let [x (rnd-float)
                y (rnd-float)]
            (assert.are.equal
             {: x : y}
             (cx x y)
             "Table with x and y should equal cx")))

      (it "equals with itself"
          (let [z (rnd-complex)]
            (assert.are.equal
             z
             z)))

      ;; These use cx.equals because Lua doesn’t call __eq for a table
      ;; and a number

      (it "equals with an integer"
          (let [x (rnd-uint)]
            (assert.is.true
             ;; Lua does not call __eq for table and number
             (cx.equals x (cx x))
             "equals with an integer")))

      (it "equals with a float"
          (let [x (rnd-float)]
            (assert.is.true
             (cx.equals x (cx x))
             "equals with a float")))

      (it "is not equal with a different integer"
          (let [x (rnd-uint)
                y (- 1 x)]
            (assert.not.true
             (cx.equals x (cx y)))))

      (it "is not equal with a different float"
          (let [x (rnd-float)
                y (- 1 x)]
            (assert.not.true
             (cx.equals x (cx y))))))

(desc "cx.add"
      (it "adds like vectors"
          (let [ax (rnd-float) bx (rnd-float)
                ay (rnd-float) by (rnd-float)
                x (+ ax bx)
                y (+ ay by)
                a (cx ax ay)
                b (cx bx by)]
            (assert.are.equal
             {: x : y }
             (+ a b)))))

(desc "cx.mod"
      (it "works for two complex integers"
          (let [scale 32
                a (rnd-complex-int scale)
                b (cx (rnd-int scale) (r 1 scale))]
            (assert.are.equal {:x (% a.x b.x)
                               :y (% a.y b.y)}
                              (% a b))))

      (it "throws when b.x is zero"
          (let [scale 32
                a (rnd-complex-int scale)
                b (cx 0 (r 1 scale))]
            (assert.has_error
             (fn [] (% a b))
             "attempt to perform 'n%0'")))

      (it "works for complex and real"
          (let [scale 32
                a (rnd-complex-int scale)
                b (rnd-int scale)]
            (assert.are.equal {:x (% a.x b)
                               :y (% a.y b)}
                              (% a b)))))


;; Lib --------------------------------

(desc "irange"
      (desc "using positive range"
            (it "works without step"
                (assert.same
                 [0 1 2 3]
                 (icollect [_ v (hex.irange 0 4)] v)))

            (it "works with step"
                (assert.same
                 [0 3 6 9]
                 (icollect [_ v (hex.irange 0 12 3)] v))))

      (desc "using negative range"
            (it "works without step"
                (assert.same
                 [(- 2) (- 1) (- 0) 1]
                 (icollect [_ v (hex.irange (- 2) 2)] v)))

            (it "works with step"
                (assert.same
                 [(- 16) (- 8) 0 8]
                 (icollect [_ v (hex.irange (- 16) 16 8)] v))))

      (desc "with invalid range"
            (local invalid-ranges [{:from 0 :to 3 :step (- 1)}
                                   {:from (- 2) :to 1 :step (- 1)}
                                   {:from 3 :to 0 :step 1}
                                   {:from 2 :to (- 2) :step 1}])
            (each [_ fx (ipairs invalid-ranges)]
                  (it "throws error"
                      (assert.has_error
                       (fn []
                           (icollect [_ v (hex.irange fx.from fx.to fx.step)] v)))))))
