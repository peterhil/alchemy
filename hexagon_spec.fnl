(local busted (require :busted))
(local run (require :busted.runner))

(local r math.random)

(local hex (require :hexagon))
(local cx hex.cx)

(run)

(macros
 {
  :desc
  (lambda [name body ...]
    `(let [name# ,name
           body# ,body]
       (busted.describe
        name#
        (fn []
            (do body#
                ,...)))))
  :it
  (fn [description body ...]
      `(let [desc# ,description
             body# ,body]
         (assert body# "Expected body for test")
         (busted.it
          desc#
          (fn []
              (do body#
                  ,...)))))
  })

;; Quickcheck helpers

(fn rnd-bool []
    (= 1 (r 0 1)))

;; Integers

(fn rnd-uint []
    (r math.maxinteger))

;; Floats

(local max-float-exp 1024)
(local inf (* 1 (^ 2.0 max-float-exp)))
(local neginf (* -1 (^ 2.0 max-float-exp)))
(local nan (/ inf inf))

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

      (it "throws with unknown types"
          (assert.has_error
           (fn [] (cx.from :unknown))
           "Can’t make a complex number from: unknown")))

(desc "cx.abs"
      (it "works for simple pythagorean triple"
          (assert.are.equal
           5.0
           (cx.abs (cx 3 4)))))

(desc "cx.equals"
      (it "with a table"
          (let [x (rnd-float)
                y (rnd-float)]
            (assert.are.equal
             {: x : y}
             (cx x y)
             "Table with x and y should equal cx"))))
