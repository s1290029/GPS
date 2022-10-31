(defvar *state* nil)

(defvar *ops* nil)

(defstruct op
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (*state* goals *ops*)
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  (or (member goal *state*)
      (some #'apply-op (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  (member goal (op-add-list op)))

(defun apply-op (op)
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op))) t))

(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))


(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
	    :preconds '(son-at-home car-works)
	    :add-list '(son-at-school)
	    :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop)
	    :add-list '(shop-knows-problem))
   (make-op :action 'telephoone-shop
	    :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
	    :preconds '(have-phone-book)
	    :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
	    :preconds '(have-money)
	    :add-list '(shop-has-money)
	    :del-list '(have-money))))
