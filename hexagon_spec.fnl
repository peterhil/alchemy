(local test (require :busted))
(local run (require :busted.runner))

(local hex (require :hexagon))
(local cx hex.cx)

(run)

(test.describe
 "complex"
 (fn []
     (test.it "new"
              (fn test-new
                  [] (assert.are.equal
                      {:x 1 :y 0} (cx.new 1)
                      "New works")))

     (test.it "equals"
              (fn test-equal
                  [] (assert.are.equal
                      {:x 1 :y 0} (cx 1)
                      "Tables can be used")))))
