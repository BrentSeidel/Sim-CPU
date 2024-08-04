(defun watch (addr)
  (setq old-value (meml addr))
  (dowhile (= old-value (meml addr))
    (sim-step)))
