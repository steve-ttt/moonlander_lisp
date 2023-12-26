;; compile with 
;; (sb-ext:save-lisp-and-die "moonlander.exe" :executable t :toplevel 'main)

;;10 print "moonlander"
;;20 let time = 0
;;30 let height = 500
;;40 let vel = 50
;;50 let fuel = 120
;;60 print "time ";time,"height ";height
;;70 print "vel ";vel,"fuel ";fuel
;;85 for i = 2 to height/500*40
;;86 print " ";
;;87 next i
;;88 print "*"
;;90 if fuel=0 goto 140
;;100 print "burn? (1-30)"
;;110 input b
;;120 if b<=0 then let b = 0 
;;130 if b>=30 then let b = 30 
;;140 if b > fuel then let b = fuel
;;150 let v1 = vel-b+5
;;160 let fuel = fuel - b
;;170 if (v1-vel)/2 >= height then goto 220
;;180 let height = height - (v1-vel)/2 
;;190 let time = time + 1
;;200 let vel = v1
;;210 goto 60
;;220 let v1 = vel + (5 - b) * height / vel 
;;230 if v1 > 5 then print "You crashed all dead"
;;240 if v1 > 1 and v1 <= 5 then print "Ok but some injuries"
;;250 if v1 < 1 then print "Good landing !"
;;260 stop

;; 1.625  m/s^2 luna grav
;; v = v_0 + at ; where v is the final velocity, v_0 is the initial velocity,
;;               a is the acceleration, and t is the time.
;;               A space craft falling at 5 m/s after 10 seconds on luna acceleration
;;               would be traveling at 21 m/s = 5 + (1.625 * 10)


(defvar *velocity* (+ 7  (random 18)) ) ;; random number between 7 and 25
(defvar *time* 0)
(defvar *start-height* (+ 400 (random 100)))
(defvar *fuel* (+ 80 (random 11)))
(defvar *term-column-width* 80)
(defvar *grav* 1.625)

(defun print-height-bar (column-width)
  (if (zerop column-width) '*
      (progn (format t "#")
	     (print-height-bar (1- column-width)))))

(defun print-stats (height)
    (format t "~%Time: ~A (sec)~tHeight: ~A (M)" *time* (floor height))
  (format t "~%Velocity: ~A (m/s)~tFuel: ~A (L)~%" *velocity* *fuel*)
  (if (< 500 height) (format t "* * * * * * * * * * * * * * TOO HIGH * * * * * * * * * * * * *")
    (print-height-bar (floor (* (/ height *start-height*) *term-column-width*)))))

(defun landed (vel)
  (cond ((> vel 2.5) (format t "~%You crashed all dead!~%~%"))
	((and (> vel 1) (< vel 2.5)) (format t "~%Ok but some injuries!~%~%"))
	(t (format t "~%Good landing commander!~%~%")))
  (exit))

(defun get-valid-number ()
  (let ((input nil))
    (loop
      (format t "~%[burn #] > " )
      (finish-output)
      (setf input (read-line))
      (if (numberp (parse-integer input :junk-allowed t))
	  (if (>= (parse-integer input) 0)
	      (return (parse-integer input))
	      (format t "Nice try but you can't give yourself fuel!~%"))
          (format t "Invalid input. Try again.~%")))))

(defun clear-screen ()
  (format t "~c[2J" #\Escape))

  
(defun game-loop (height)
  (let ((burn 0)
        (v_0 *velocity*))
    (if (<= height 0)
        (landed *velocity*) 
        (progn
	  (clear-screen)
          (print-stats height)
          ;(format t "~%[burn #] > ")
          (setf burn (get-valid-number))
          (if (and (not (null burn)) (numberp burn))
              'burn
              (setf burn 0))
	  
          (setf *fuel* (- *fuel* burn))
	  (setf v_0 (+ *velocity* (* -1 burn)))
          (setf *velocity* (+ v_0 (* *grav* 1))) ; Fix parentheses here
          (setf height (- height (* *velocity* 1))) ; Fix calculation here
          (setf *time* (1+ *time*))
          (game-loop height))))) ; Add closing parenthesis here

;;run game
(defun main ()
    (game-loop *start-height*))
