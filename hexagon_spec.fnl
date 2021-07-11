(local test (require :busted))
(local run (require :busted.runner))

(local hex (require :hexagon))
(local cx hex.cx)

(run)

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

          (test.it "with one argument"
                   (fn []
                       (assert.are.equal
                        {:x 1 :y 0}
                        (cx.new 1))))

          (test.it "with two arguments"
                   (fn []
                       (assert.are.equal
                        {:x 2 :y 3}
                        (cx.new 2 3))))))

     (test.it
      "equals"
      (fn []
          (assert.are.equal
           {:x 1 :y 0}
           (cx 1)
           "Tables are equal")))))
