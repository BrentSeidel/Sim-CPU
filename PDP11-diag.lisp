;
;  Configuration for booting a RK11 or RK611 attached to PDP-11
;
(setq model "0")
(dowhile (= model "0")
  (print "PDP-11 model available") (terpri)
  (print "1. PDP-11/10 or PDP-11/05") (terpri)
  (print "2. PDP-11/20 or PDP-11/15") (terpri)
  (print "3. PDP-11/04") (terpri)
  (print "4. PDP-11/34") (terpri)
  (print "5. PDP-11/40 or PDP-11/35") (terpri)
  (print "Select model: ")
  (setq model (read-line))
  (if (= model "1") (sim-cpu "PDP-11/10")
  (if (= model "2") (sim-cpu "PDP-11/20")
  (if (= model "3") (sim-cpu "PDP-11/04")
  (if (= model "4") (sim-cpu "PDP-11/34")
  (if (= model "5") (sim-cpu "PDP-11/40")
      (progn (print "Unknown model: " model)
        (terpri)
        (setq model "0"))))))))
;
;  Attach required hardware
;
;  Attach BM792 boot ROM for PDP-11/04
;
(if (= model "3") (attach "BM792" #o773300 "MEM" 0))
;
;  Attach KT11 MMU for 11/34 and 11/40
;
(if (= model "4") (attach "KT11" 0 "MEM" #o250))
(if (= model "5") (attach "KT11" 0 "MEM" #o250))
;
;  KW11 always attaches to memory address 777546 and vector 100, BR6.
(attach "KW11" #o777546 "MEM" (+ #o100 #x060000))
;
; Rx vector is #o060, Tx vector is #o064, both at BR4.  Combined value is #o15000060
(attach "DL11" #o777560 "MEM" #o15000060 2171)
;
(print "Connect to the console on port 2171")
(print "Load the desired diagnostic routine, set desired tracing")
(print "For diagnostics, set the start address to 80 (hex)")
(print "and type RUN to start.")
(exit)
