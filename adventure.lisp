;;; chapter 5

(defparameter *nodes* '((living-room (you are in the living room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room
			 (garden west door)
			 (attic upstairs ladder))
			(garden
			 (living-room east door))
			(attic
			 (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defparameter *location* 'living-room)

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (location objects object-locations)
  (labels ((at-loc-p (object)
	     (eq (cadr (assoc object object-locations)) location)))
    (remove-if-not #'at-loc-p objects)))

(defun describe-objects (location objects object-locations)
  (labels ((describe-obj (object)
	     `(you see a ,object on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at location objects object-locations)))))

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t `(you cannot get that.))))

(defun drop (object)
  (cond ((member object
		 (objects-at 'body *objects* *object-locations*))
	 (push (list object *location*) *object-locations*)
	 `(you are no longer carrying the ,object))
	(t `(you are not carrying that.))))

(defun inventory ()
  (cons 'items (objects-at 'body *objects* *object-locations*)))

;;; chapter 6

(defparameter *allowed-commands* '(look walk pickup drop inventory))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (progn (format t "~%")
	 (princ "> ")
	 (let ((cmd (read-from-string
		     (concatenate 'string "(" (read-line) ")"))))
	   (flet ((quote-it (x)
		    (list 'quote x)))
	     (cons (car cmd) (mapcar #'quote-it (cdr cmd)))))))

(defun game-eval (sexp)
  (format t "~%")
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))
