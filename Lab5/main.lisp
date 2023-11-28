(defclass polynom ()
 ((var-symbol :initarg :var :reader var)
  ;; Разреженный список термов в порядке убывания степени
  (term-list :initarg :terms :reader terms)))

(defun make-term (&key order coeff)
  (list order coeff))

(defun order (term) (first term))   ; степень
(defun coeff (term) (second term))  ; коэффициент

; распечатка многочленов
(defgeneric zerop1 (arg)
 (:method ((n number))   ; (= n 0)
  (zerop n)))

(defgeneric minusp1 (arg)
 (:method ((n number))   ; (< n 0)
  (minusp n)))

(defmethod print-object ((p polynom) stream)
  (format stream "[МЧ (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
          (var p)
          (mapcar (lambda (term)
                    (list (zerop1 (coeff term))
                          (minusp1 (coeff term))
                          (if (minusp1 (coeff term))
                              (abs (coeff term))
                              (coeff term))
                          (order term)
                          (var p)
                          (order term)))
                  (terms p))))

; сложение многочленов
(defmethod add2 ((p1 polynom) (p2 polynom))
  (if (same-variable-p (var p1) (var p2))
      (make-instance 'polynom
                     :var (var p1)
                     :terms (add-terms (terms p1)
                                       (terms p2)))
      (error "Многочлены от разных переменных: ~s и ~s"
             p1 p2)))

(defun same-variable-p (v1 v2)
  ;; Переменные v1 и v2 - совпадающие символы
  (and (symbolp v1) (symbolp v2) (eq v1 v2)))

(defun add-terms (tl1 tl2)
  ;; Объединить списки термов tl1 и tl2,
  ;; упорядочивая по убыванию степеней
  (cond ((null tl1) tl2)
        ((null tl2) tl1)
        (t
         (let ((t1 (nth 0 tl1))
               (t2 (nth 0 tl2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1
                               (add-terms (rest tl1) tl2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2
                               (add-terms tl1 (rest tl2))))
                 (t
                  ;; степени совпадают - суммируем коэффициенты
                  (adjoin-term  
                   (make-term :coeff (+ (coeff t1) (coeff t2))
                              :order (order t1))
                   (add-terms (rest tl1)
                              (rest tl2)))))))))

(defun adjoin-term (term term-list)
  ;; Добавить term к списку term-list
  (if (zerop1 (coeff term))   ; если коэффициент нулевой,
      term-list               ; то отбрасываем терм,
      (cons term term-list))) ; иначе накапливаем

; умножение многочленов
(defmethod mul2 ((p1 polynom) (p2 polynom))
  (if (same-variable-p (var p1) (var p2))
      (make-instance 'polynom
                     :var (var p1)
                     :terms (mul-terms (terms p1)
                                       (terms p2)))
      (error "Многочлены от разных переменных: ~s и ~s"
             p1 p2)))

(defun mul-terms (tl1 tl2)
  ;; Скрестить каждый терм из списка tl1 с каждым из списка tl2
  (if (null tl1)
      ()
      (add-terms (mul-term-by-all-terms (first tl1) tl2)
                 (mul-terms (rest tl1) tl2))))

(defun mul-term-by-all-terms (t1 term-list)
  ;; Скрестить терм t1 с каждым из списка term-list
  (if (null term-list)
      ()
      (let ((t2 (first term-list)))
        ;; Коэффициенты перемножаем, а степени суммируем
        (adjoin-term (make-term :coeff (* (coeff t1) (coeff t2))
                                :order (+ (order t1) (order t2)))
                     (mul-term-by-all-terms t1 (rest term-list))))))

; мои функции
(defun make_polynom (list1 list2)
    (cond ((null list2) nil)
                ((null list1)
                        (make-instance 'polynom
                                        :var 'z
                                        :terms (list (make-term :order 0 :coeff (nth 0 list2)))))
        (t (let ((n (min (length list1) (1- (length list2))))
                (p1 (make-instance 'polynom :var 'x :terms (list (make-term :order 0 :coeff (nth 0 list2))))))
                (loop for i from 1 to n
                   do (setq p1 (add2 p1 (mul_n (nth i list2) (butlast list1 (- (length list1) i)))))) p1))))

(defun mul_n (d list2)
    (let ((p0 (make-instance 'polynom :var 'x :terms (list (make-term :order 0 :coeff d)))))
        (loop for a in list2
              do (setq p0 (mul2 p0 (make-instance 'polynom :var 'x :terms (list (make-term :order 1 :coeff 1)
                                                                                (make-term :order 0 :coeff (- a))))))) p0))