
;; Documents are stored in a repository.

(defvar *document-repository* ())

;; They can be imported from
;; and exported to files.

;; The repository remembers the location of imported and exported
;; files, and can bulk import/export.

(defun import-document ()
  (values))

(defun export-document ()
  (values))

(defvar *document*)

(setq *document*  `(:html
                    (:head
                     (:title %title))
                    (:body
                     (:img (:src "logo.png"))
                     (:h1 %title)
                     %content)))

(defvar *point* *document*)

(defvar *buffer* nil)

(defun ins ()
  (let ((new (cons (read) *point*)))
    (setq *document* (subst new *point* *document*))
    (setq *point* new)))

(defun apd ()
  (let ((new (cons (read) (cdr *point*))))
    (setq *document* (subst new (cdr *point*) *document*))
    (setq *point* new)))

(defun del ()
  (if (cdr *point*)
      (setq *document* (subst (cdr *point*) *point* *document*)
            *point* (cdr *point*))))

(defun rep ()
  (let ((new (cons (read) (cdr *point*))))
    (setq *document* (subst new *point* *document*)
          *point* new)))

(defun yank ()
  (setq *buffer* *point*))

(defun pull ()
  (let ((new (cons *buffer* (cdr *point*))))
    (setq *document* (subst new (cdr *point*) *document*))
    (setq *point* new)))

(defun move-down ()
    (if (consp (car *point*))
        (setq *point* (car *point*))))

(defun move-next ()
    (if (cdr *point*)
        (setq *point* (cdr *point*))))

(defun locate-if (predicate tree)
  (cond ((funcall predicate tree) tree)
        ((atom tree) nil)
        (t (or (locate-if predicate (car tree))
               (locate-if predicate (cdr tree))))))

(defun pred (x)
  (lambda (y) (and (consp y) (eq x (cdr y)))))

(defun memc (item list)
  (if list (or (eq item list) (memc item (cdr list)))))

(defun contains (x)
  (lambda (y) (and (consp y) (consp (car y)) (memc x (car y)))))

(defun move-backward ()
  (setq *point* (or (locate-if (pred *point*) *document*) *point*)))

(defun move-up ()
  (setq *point*
        (or (locate-if (contains *point*) *document*)
            *point*)))

(defun edit ()
    (read-char))

(defun show-list (seq)
  (do ((x seq (cdr x)))
      ((null x))
    (if (eq x seq) (write-char #\())
    (show (car x))
    (write-char (if (cdr x) #\space #\)))))

(defun show (x i j)
  (if (atom x) (prin1 x)
    (progn
      (if (zerop i) (write-char #\())
      (if (eq x *point*) (write-char #\>))
      (show (car x) 0 (1+ j))
      (if (cdr x)
          (progn
            (write-char #\space)
            (show (cdr x) (1+ i) j))
        (write-char #\)))))
  (values))

(defun read-key ()
  (ext:with-keyboard
    (loop :for char = (read-char ext:*keyboard-input*)
          :for key = (or (ext:char-key char) (character char))
          :return key)))

(defun run ()
  (loop (show *document* 0 0)
        (princ #\newline)
        (let ((ch (read-key)))
          (case ch
            ((#\a) (apd))
            ((#\h) (move-backward))
            ((#\i) (ins))
            ((#\j) (move-down))
            ((#\k) (move-up))
            ((#\l) (move-next))
            ((#\p) (pull))
            ((#\q) (return))
            ((#\r) (rep))
            ((#\x) (del))
            ((#\y) (yank))))))

(run)

