(defvar *ops* nil)

(defvar *dbg-ids* nil)

(defstruct op
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun op (action &key preconds add-list del-list)
  (convert-op
   (make-op :action action
	    :preconds preconds
	    :add-list add-list
	    :del-list del-list)))

(defun GPS (state goals &optional (*ops* *ops*))
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(defun achieve (state goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
	((member-equal goal goal-stack) nil)
	(t (some #'(lambda (op) (apply-op state goal op goal-stack))
		 (find-all goal *ops* :test #'appropriate-p)))))

(defun achieve-all (state goals goal-stack)
  (let ((current-state state))
    (if (and (every #'(lambda (g)
			(setf current-state
			      (achieve current-state g goal-stack)))
		    goals)
	     (subsetp goals current-state :test #'equal))
	current-state)))

(defun appropriate-p (goal op)
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
			     (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
			     (member-equal x (op-del-list op)))
			 state2)
	      (op-add-list op)))))

(defun use (oplist)
  (length (setf *ops* oplist)))

(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
	     :test (complement test) keyword-args)))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun executing-p (x)
  (starts-with x 'executing))

(defun starts-with (list x)
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

;debug
(defun dbg (id format-string &rest args)
  (when (member id *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dodebug (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  (setf *dbg-ids* (if (null ids) nil
		      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

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
	    :del-list '(have-money))
   (make-op :action 'ask-phone-number
	    :preconds '(in-communivation-with-shop)
	    :add-list '(know-phone-number))))
