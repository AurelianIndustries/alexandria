(in-package :alexandria)

(defun copy-array (array &key (element-type (array-element-type array))
                              (fill-pointer (and (array-has-fill-pointer-p array)
                                                 (fill-pointer array)))
                              (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
 (let* ((dimensions (array-dimensions array))
        (new-array (make-array dimensions
                               :element-type element-type
                               :adjustable adjustable
                               :fill-pointer fill-pointer)))
   (dotimes (i (array-total-size array))
     (setf (row-major-aref new-array i)
           (row-major-aref array i)))
   new-array))

(defun lists-to-2d-array (l &key element-type width initial-element)
  "Takes a list of lists l and returns a 2-dimensional array whose
dimensions are (<the length (number of lists) of l> <the max length of
each of the lists in l>) and whose contents are the items contained in
the lists in l."
  (let ((rows (length l))
        (cols (or width (apply #'max (mapcar #'length l)))))
    (apply #'make-array (list rows cols)
           :initial-contents (loop for row in l
                                collect
                                  (let ((v (make-array (length row)
                                                       :initial-contents row
                                                       :adjustable t
                                                       :fill-pointer t)))
                                    (when (< (length row) cols)
                                      (adjust-array v
                                                    cols
                                                    :initial-element initial-element
                                                    :fill-pointer t))
                                    v))
           (when element-type
             `(:element-type ,element-type)))))
