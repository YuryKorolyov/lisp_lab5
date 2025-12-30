(defparameter *companies* (make-hash-table :test 'equal))
(defparameter *spacecrafts* (make-hash-table :test 'equal))

(defun split (row)
  "Розбиває рядок на частини."
  (let ((result '()) ; список для збереження
        (start 0))   ; початковий індекс
    ; пошук роздільника
    (loop for end = (position #\, row :start start)
          while end
          do (progn
               (push (subseq row start end) result)
               (setf start (1+ end))))
    (push (subseq row start) result)
    (nreverse result)))


(defun read-file (path)
  "Читає CSV-файл і повертає список рядків."
  (with-open-file (stream path :direction :input)
    ; читаємо кожен рядок файлу
    (loop for row = (read-line stream nil)
          while row
          ; розбиваємо рядок на частини
          collect (split row))))


(defun load-table (filepath table)
  "Завантажує таблицю з файлу CSV."
  (let* ((rows (read-file filepath)) ; читає CSV-файл
         (keys (mapcar (lambda (key) 
                         (intern (string-upcase key)))
                       (car rows))))
    (mapc (lambda (row) 
            (let ((record (loop for key in keys ; для кожного ключа
                                for value in row ; беремо відповідне значення з рядка
                                append (list key value)))) ; створюємо список пар ключ-значення
              (setf (gethash (getf record (car keys)) table) record))) ; зберігаємо запис у геш-таблицю, (getf record (car keys)) = id
          (cdr rows))))


(defun pretty-print-table (table)
  "Виводить вміст геш-таблиці."
  (maphash (lambda (key value) 
             (format t "~% [~a] " key) 
             (loop for (k v) on value by #'cddr 
                   do (format t "~a: ~a " k v)))
           table))


(defun write-csv (filepath table)
  "Записує геш-таблицю у CSV-файл."
  (with-open-file (stream filepath :direction :output :if-exists :supersede)
    (let* ((first-key (first (loop for key being the hash-keys of table collect key))) 
           (first-record (gethash first-key table))
           (keys (loop for (key _) on first-record by #'cddr collect key)))
      
      ; записуємо ключі в CSV
      (format stream "~{~a~^,~}~%" keys)
      
      ; проходимо по всіх записах хеш-таблиці і записуємо їх у CSV
      (maphash
       (lambda (_ value) ; для кожного запису в хеш-таблиці
         (format stream "~{~a~^,~}~%" ; форматуємо рядок як CSV
                 (mapcar (lambda (key) (getf value key)) keys)))
       table))))


(defun select (filepath)
  "Функція для вибірки записів із CSV-файлу."
  (let ((table (make-hash-table :test 'equal)))
    (load-table filepath table)
    (lambda (&rest filters)
      (let ((normalized-filters (loop for (key value) on filters by #'cddr
                                      collect (cons (intern (string-downcase (symbol-name key))) value))))
        (loop for record being the hash-values of table
              for normalized-record = (loop for (key value) on record by #'cddr
                                            append (list (intern (string-downcase (symbol-name key))) value))
              when (every (lambda (filter-pair)
                            (let ((key (car filter-pair))
                                  (value (cdr filter-pair)))
                              (equal (getf normalized-record key) value)))
                          normalized-filters)
              collect record)))))


(defun hash-table-to-alist (hash-table)
  "Перетворює геш-таблицю у асоціативний список."
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    (nreverse alist)))

(defun test-split ()
  "Тест функції split."
  (let ((string "a,b,c,d")
        (expected-result '("a" "b" "c" "d")))
    (format t "Testing split with input: ~a " string)
    (let ((actual-result (split string)))
      (format t "Expected result: ~a~%" expected-result)
      (format t "Actual result: ~a~%" actual-result)
      (if (equal actual-result expected-result)
          (format t "split test passed.~%")
          (format t "split test failed.~%")))))


(defun test-read-file ()
  "Тест функції read-file."
  (let ((test-csv "c:/KPI/FP/lab5/test.csv")
        (expected-header '("id" "name" "country"))
        (expected-row '("1" "SpaceX" "USA")))
    (with-open-file (stream test-csv :direction :output :if-exists :supersede)
      (format stream "id,name,country~%1,SpaceX,USA~%2,ESA,Europe~%"))
    (format t "Testing read-file with test file: ~a~%" test-csv)
    (let ((data (read-file test-csv)))
      (format t "Expected header: ~a~%" expected-header)
      (format t "Actual header: ~a~%" (car data))
      (if (equal (car data) expected-header)
          (format t "Header test passed.~%")
          (format t "Header test failed.~%"))
      (format t "Expected first row: ~a~%" expected-row)
      (format t "Actual first row: ~a~%" (cadr data))
      (if (equal (cadr data) expected-row)
          (format t "Row test passed.~%")
          (format t "Row test failed.~%")))
    (delete-file test-csv)))

(defun test-select ()
  "Тест функції select."
  (let* ((test-file "c:/KPI/FP/lab5/test.csv"))
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (format stream "id,name,country~%1,SpaceX,USA~%2,ESA,Europe~%"))

    (flet ((normalize-record (record)
             (loop for (key value) on record by #'cddr
                   append (list (intern (string-downcase (symbol-name key))) value))))

      (format t "Testing select with filters: ~a~%" '(:country "USA"))
      (let* ((selector (select test-file)) 
             (filters '(:country "USA")) 
             (expected-result '((:id "1" :name "SpaceX" :country "USA")))
             (normalized-expected (mapcar #'normalize-record expected-result))
             (actual-result (mapcar #'normalize-record (apply selector filters))))
        (format t "Expected result: ~a~%" normalized-expected)
        (format t "Actual result: ~a~%" actual-result)
        (if (equal actual-result normalized-expected)
            (format t "Select test with filters passed.~%")
            (format t "Select test with filters failed.~%")))

      
      (format t "Testing select without filters: ~a~%" nil)
      (let* ((selector (select test-file)) 
             (expected-result '((:id "1" :name "SpaceX" :country "USA")
                                (:id "2" :name "ESA" :country "Europe")))
             (normalized-expected (mapcar #'normalize-record expected-result))
             (actual-result (mapcar #'normalize-record (funcall selector))))
        (format t "Expected result: ~a~%" normalized-expected)
        (format t "Actual result: ~a~%" actual-result)
        (if (equal actual-result normalized-expected)
            (format t "Select test without filters passed.~%")
            (format t "Select test without filters failed.~%"))))

    (delete-file test-file)))


(defun run-all-tests ()
  "Запускає всі тестові функції."
  (test-split)
  (format t "~%")
  (test-read-file)
  (format t "~%")
  (test-select)
  (format t "~%"))
