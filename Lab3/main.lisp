(defun new_matrix (matrix row column i_row j_column)
    (let
        (
            (m (+ (array-dimension matrix 0) 1))
            (n (+ (array-dimension matrix 1) 1))
            (result_matrix (make-array (list (+ (array-dimension matrix 0) 1) (+ (array-dimension matrix 1) 1))))
            (i_row (- i_row 1))
            (j_column (- j_column 1))
        )
        (loop for i from 0 below (- m 1) do
                (if (<= i i_row)
                    (loop for j from 0 below (- n 1) do
                        (if (<= j j_column) 
                            (setf (aref result_matrix i j) (aref matrix i j))
                            (setf (aref result_matrix i (+ j 1)) (aref matrix i j))
                        )
                    )
                    (loop for j from 0 below (- n 1) do
                        (if (<= j j_column) 
                            (setf (aref result_matrix (+ i 1) j) (aref matrix i j)) 
                            (setf (aref result_matrix (+ i 1) (+ j 1)) (aref matrix i j))
                        )
                    )
                )
        )
        (loop for j from 0 below (- n 1) do
            (if (<= j j_column) 
                (setf (aref result_matrix (+ i_row 1) j) (svref row j)) 
                (setf (aref result_matrix (+ i_row 1) (+ j 1)) (svref row j))
            )
        )
        (loop for i from 0 below m do
            (setf (aref result_matrix i (+ j_column 1)) (svref column i))
        )
        (return-from new_matrix result_matrix)
    )
)