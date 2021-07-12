(local test (require :busted))
(local run (require :busted.runner))

(local r math.random)

(local hex (require :hexagon))
(local cx hex.cx)

(run)

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

;; Tests

(test.describe
 "complex"
 (fn []

     (test.describe
      "new"
      (fn []
          (test.it "throws error without arguments"
                   (fn []
                       (assert.has_error
                        cx.new)))

          (test.it "with one float argument"
                   (fn []
                       (let [x (rnd-float)]
                         (assert.are.equal
                          {: x :y 0}
                          (cx.new x)))))

          (test.it "with one integer argument"
                   (fn []
                       (let [x (rnd-uint)]
                         (assert.are.equal
                          {: x :y 0}
                          (cx.new x)))))

          (test.it "with two arguments"
                   (fn []
                       (let [x (rnd-float)
                             y (rnd-float)]
                         (assert.are.equal
                          {: x : y}
                          (cx.new x y)))))))

     (test.describe
      "type"
      (fn []
          (local number (rnd-float))

          (test.it "returns complex type"
           (fn []
               (assert.are.equal
                (cx.type (cx number))
                :complex)))

          (test.it "delegates to generic type"
           (fn []
               (assert.are.equal
                (cx.type number)
                :number)))))

     (test.it
      "equals"
      (fn []
          (assert.are.equal
           {:x 1 :y 0}
           (cx 1)
           "Tables are equal")))))
