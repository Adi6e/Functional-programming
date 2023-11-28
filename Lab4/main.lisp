(defun russian-upper-case-p (char)
  (position char "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"))

(defun russian-char-downcase (char)
  (let ((i (russian-upper-case-p char)))
    (if i 
        (char "абвгдеёжзийклмнопрстуфхцчшщъыьэюя" i)
        (char-downcase char))))

(defun russian-char-equal (char1 char2)
  (char-equal (russian-char-downcase char1)
              (russian-char-downcase char2)))

(defun russian-string-downcase (string)
  (map 'string #'russian-char-downcase string)
)

(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline)))

(defun word-list (string)
  (loop with len = (length string)
        for left = 0 then (1+ right)
        for right = (or (position-if #'whitespace-char-p string
                                     :start left)
                        len)
        unless (= right left)
          collect (subseq string left right)
        while (< right len)))

(defun all-letters-string-downcase (string)
    (setq string (russian-string-downcase string))
    (string-downcase string)
)

(defun collect-words-with-char (ch text)
  (let ((word_list (list)))
    (dolist (sentence text)
      (dolist (word (word-list sentence))
        (let ((string (string-right-trim ",.;:?!" word)))
          (when (and (< 0 (length string)) (/= (length string) (length (string-trim (all-letters-string-downcase (string ch)) (all-letters-string-downcase string)))) (not (member string word_list :test #'string=)))
              (setq word_list (cons string word_list)))))
    )word_list)
)