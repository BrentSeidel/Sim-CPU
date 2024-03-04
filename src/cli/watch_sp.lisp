(defun watch-sp ()
  (setq old-sp (reg-val 15))
  (dowhile (< old-sp #x300001)
    (dowhile (= old-sp (reg-val 15))
      (sim-step))
    (setq old-sp (reg-val 15))
    (print "SP changed at PC ")
    (print-hex (reg-val 17))
    (print ", task " (memw #x31b6) ", new SP ")
    (print-hex (reg-val 15)
    (terpri))))

