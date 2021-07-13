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

;; Quickcheck helpers

(fn rnd-bool []
    (= 1 (r 0 1)))

;; Integers

(fn rnd-uint []
    (r math.maxinteger))

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

;; Tests

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

      (it "equals with an integer"
          (let [x (rnd-uint)]
            (assert.is.true
             ;; Lua does not call __eq for table and number
             (cx.equals x (cx x))
             "equals with an integer")))

      (it "equals with a float"
          (let [x (rnd-float)]
            (assert.is.true
             ;; Lua does not call __eq for table and number
             (cx.equals x (cx x))
             "equals with a float")))

      (it "is not equal with a different integer"
          (let [x (rnd-uint)
                y (- 1 x)]
            (assert.not.equal
             x
             (cx y))))

      (it "is not equal with a different float"
          (let [x (rnd-float)
                y (- 1 x)]
            (assert.not.equal
             x
             (cx y)))))

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
