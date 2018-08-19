;; Digital EDA Tooling with Common Lisp
;;
(declaim (sb-ext:muffle-conditions cl:warning))
(declaim (optimize (debug 3)))

(ql:quickload :jsown)

(defmacro append! (x y)
  (list 'setq x (list 'append x y)))

(defmacro remove! (x y)
  (list 'setq y (list 'remove x y)))

(defun diff (x y)
  (set-difference x y))

(defun c+ (&rest strs)
  (apply 'concatenate
	 (append '(string)
		 (mapcar (lambda (x)
			   (if (symbolp x) (symbol-name x) x))
			 strs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *label-counter* 1)

(defun input-nodes (node)
  (mapcar (lambda (x) (car (get node x)))
	  (get node 'inputs)))

(defun output-nodes (node)
  (mapcar (lambda (x) (car (get node x)))
	  (get node 'outputs)))

(defun clear-node (node)
  (setf (symbol-plist node) '()))

(defun node (name function inputs outputs)
  (set name (intern (symbol-name name)))  
  (clear-node name)  
  (setf (get name 'inputs) inputs
	(get name 'outputs) outputs
	(get name 'function) function
	(get name 'label-ids) '())
  (loop for  p in (append inputs outputs) do
       (setf (get name 'label-ids)
	     (push (list p (incf *label-counter*))
		   (get name 'label-ids))))
  name)

(defun node-labels (node)
  (append (get node 'inputs) (get node 'outputs)))

(defun co (from from-out-label	   
	   to to-in-label)
  "connect output of node FROM to node TO"
  (setf (get from from-out-label) (cons to to-in-label)
	(get to to-in-label) (cons from from-out-label)))

(defun bit-print (bits)
  (format nil "~B" bits))

'("Bitwise Operators"
  (setq a #*011 b #*101)
  (arrayp a)
  (aref a 0)       
  (bit-not a)   
  (bit-ior a b)
  (bit-nor a b)
  (bit-xor a b) 
  (bit-and a b)    
  (bit-nand a b)
  (bit-eqv a b)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun label-value (label)
  (intern (c+ label "-" 'value)))


(defun set-node-value (node label value)
  (setf (get node (label-value label)) value))

(defun get-node-value (node label)
  (get node (label-value label)))

(defun node-inputs-signals (node)
  (mapcar (lambda (i)
	    (list i (get-node-value node i))) (get node 'inputs)))

(defun connected-output? (node)
  (setq connected nil)
  (loop for output in (get node 'outputs) do
       (if (get node output)
	   (setq connected t)))
  connected)

(defun get-node-outputs (node)
  (loop for out in (get node 'outputs) collect
       (list out (get node (label-value out)))))

(defun display-outputs (nodes)
  (terpri)
  (mapcar (lambda (node) (print (list node (get-node-outputs node))))
	  nodes))

(defun function-node (node node-function)
  (loop for out-label in (get node 'outputs) collect
       (list out-label
	     (set-node-value node out-label
			     (apply node-function input-signals)))))
		      
(defun compute-node (node)
  (case (setq node-inputs (node-inputs-signals node)
	      input-signals (mapcar 'cadr node-inputs)
	      node-function (get node 'function))	      
    ('wire (car input-signals))
    ('bit-not (function-node node node-function))
    ('bit-ior (function-node node node-function))
    ('bit-and (function-node node node-function))
    ('bit-xor (function-node node node-function))
    ('bit-nor (function-node node node-function))
    ('dff (function-node node 'bit-xor))
    (t (error (c+ "Uknown node function : " node-function)))
    ))

(defun clear-unknowns (unknowns)
  (loop for node in unknowns do
       (setf (get node 'unknown-inputs) (get node 'inputs))))

(defun assign-knowns (knowns)
  (loop for k in knowns do
       (setq node (car k)
	     signals (cdr k))
       (loop for s in signals do
	    (set-node-value node (car s) (cadr s)))))


(defun assign-signal (target-name target-input-label out-signal)
  (set-node-value target-name target-input-label
		  out-signal)	 
  (setf (get target-name 'unknown-inputs)
	(remove target-input-label target-unknowns)))

(defun solve (knowns unknowns)  
  (clear-unknowns unknowns)
  (assign-knowns knowns)  
  (do ((node-order '()))
      ((endp knowns)
       node-order)    
    (setq node-cell (pop knowns)
	  node (car node-cell)
	  outputs (get node 'outputs))
    (append! node-order (list node))
    (loop for out-label in outputs do
	 (setq out-signal (get-node-value node out-label)
	     target-node (get node out-label)
	     target-name (car target-node)
	     target-input-label (cdr target-node)
	     target-unknowns (get target-name 'unknown-inputs))
	 (if target-node
	     (progn
	       (assign-signal target-name target-input-label out-signal)
	       (if (null (get target-name 'unknown-inputs))
		   (progn	       
		     (setq new-outputs (compute-node target-name))
		     (if (connected-output? target-name)
			 (append! knowns (list
					  (append (list target-name)
						  new-outputs))))))
	       )))))

;;;;;;;;;;;;;;;;;
(defun generate-diagram ()
  (asdf:run-shell-command "node netlistsvg/bin/netlistsvg res/netlist.json -o circuit_out.svg --skin netlistsvg/lib/light_default.svg &"))

(defun write-netlist-file (netlist)
  (with-open-file (str "res/netlist.json"
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str (netlist-json netlist))))

(defun netlist-json-ports (ports)
  (append
   '("ports" :obj)
   (loop for p in ports collect
	`(,(car p) :obj
	   ("direction" . ,(cadr p))
	   ("bits" . (,(caddr p)))))))

(defun netlist-json-cells (cells)
  (append
   '("cells" :obj)
   (loop for c in cells collect
	`(,(car c) :obj
	   ("type" . ,(cadr c))
	   ,(append '("port_directions" :obj)
		    (caddr c))
	   ,(append '("connections" :obj)
		    (cadddr c)) ))))

(defun netlist-bits-field (node io-label)
  (if (member io-label (get node 'inputs))
      (destructuring-bind (input-node . input-label)
	  (get node io-label)
	(cadr (assoc input-label (get input-node 'label-ids))))
      (cadr (assoc io-label (get node 'label-ids)))))

(defun netlist-cell-type (node)
  "values are derived from the yosys netlist json format for cells, inputs to each cell with logic types (and, or, etc) in the json netlist format require A,B,C... inputs and Y output"
  (case (get node 'function)
    ('bit-ior "$or")
    ('bit-not "$not")
    ('bit-and "$and")
    ('bit-xor "$xor")
    ('bit-nor "$nor")
    ('add "$add")
    ('sub "$sub")
    ('equal"$eq")
    ('dff "$dff")
    ('mux "$mux")
    ('pmux "$pmux")
    (t (symbol-name n))))

(defun build-netlist (nodes port-nodes)
  (setq ports '()
	cells '())  
  (loop for n in nodes do
       (if (member n port-nodes)
	   (push `(,(symbol-name n)
		    "input"
		    ,(loop for i in (get n 'outputs)
			 collect (netlist-bits-field n i)))
		 ports)
	   (push
	    `(,(symbol-name n)
	       ,(netlist-cell-type n)
	       ,(append
		 (loop for i in (get n 'inputs)
		    collect `(,(symbol-name i) "input"))
		 (loop for i in (get n 'outputs)
		    collect `(,(symbol-name i) "output")))
	       ,(append
		 (loop for i in (append (get n 'inputs) (get n 'outputs))
		    collect `(,(symbol-name i)
			       (,(netlist-bits-field n i))))))
	        cells)))
  (list ports cells))

(defun netlist-json (netlist)
  (jsown:to-json
   `(:obj
     ("modules" :obj
      ("ModuleName" :obj
       ,(netlist-json-ports (car netlist))
       ,(netlist-json-cells (cadr netlist))
       )))))
;; Sample Simulation

(defun test-simulate ()
  (break)
  (setq *label-counter* 1
	nodes
	(list
	 (node 'A 'wire '() '(X Y))
	 (node 'B 'wire '() '(X Y))
	 (node 'I0 'wire '() '(X Y))
	 (node 'I1 'wire '() '(X))
	 
	 (node 'C 'bit-nor '(A B) '(Y))
	 (node 'D 'bit-ior '(A B C) '(Y))
	 (node 'E 'bit-not '(A) '(Y))
	 (node 'F 'bit-xor '(A B) '(Y))
	 (node 'G 'bit-and '(A B) '(Y))

	 ))
  (co A 'X C 'A)
  (co A 'Y D 'C)  
  (co B 'X C 'B)
  (co B 'Y E 'A)
  (co I0 'X D 'A)
  
  (co I0 'Y G 'B)
  (co I1 'X G 'A)
  
  (co C 'Y D 'B)
  (co D 'Y F 'A)
  (co E 'Y F 'B)  
    
  (setq knowns '((A (X #*0) (Y #*0))
		 (B (X #*0) (Y #*0))
		 (I0 (X #*1) (Y #*0))
		 (I1 (X #*1)))
	unknowns (diff nodes (mapcar 'car knowns)))
  
  (solve knowns unknowns)
  
  (display-outputs nodes)
  (write-netlist-file (build-netlist nodes '(A B I0 I1)))
  (generate-diagram)
  )
