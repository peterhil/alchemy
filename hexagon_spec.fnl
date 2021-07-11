(local test (require :busted))
(local run (require :busted.runner))
(local hex (require :hexagon))

(run)

(test.describe
 "complex"
 (fn [] (test.it
         "equals"
         (fn [] (assert.are.equal
                 {:x 1 :y 0} (hex.cx 1)
                 "Tables can be used")))))
