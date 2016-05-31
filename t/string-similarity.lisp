
(in-package :analyzer.test)

(test string-similarity
  (let ((delta 0.01)
        (words '(("sealed" . 0.8)
                 ("healthy" . 0.55)
                 ("heard" . 0.44)
                 ("herded" . 0.4)
                 ("help" . 0.25)
                 ("sold" . 0))))
    (loop for (word . expected) in words
       do
         (is (< (abs (- (soerensen-dice-coefficient "Healed" word) expected)) delta)))))
