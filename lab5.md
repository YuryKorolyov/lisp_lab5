<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Корольов Юрій КВ-23<p>
<p align="right">Рік: 2025<p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці

6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 11
База даних: Космічні апарати

Тип записів: Геш-таблиця

Таблиці: Компанії, Космічні апарати

  
## Лістинг реалізації завдання
```lisp
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

```
  
### Тестові набори та утиліти
```lisp
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
```

### Тестування
```lisp
CL-USER> (run-all-tests)
Testing split with input: a,b,c,d Expected result: (a b c d)
Actual result: (a b c d)
split test passed.

Testing read-file with test file: c:/KPI/FP/lab5/test.csv
Expected header: (id name country)
Actual header: (id name country)
Header test passed.
Expected first row: (1 SpaceX USA)
Actual first row: (1 SpaceX USA)
Row test passed.

Testing select with filters: (COUNTRY USA)
Expected result: ((id 1 name SpaceX country USA))
Actual result: ((id 1 name SpaceX country USA))
Select test with filters passed.
Testing select without filters: NIL
Expected result: ((id 1 name SpaceX country USA) (id 2 name ESA country Europe))
Actual result: ((id 1 name SpaceX country USA) (id 2 name ESA country Europe))
Select test without filters passed.

NIL
CL-USER> 
```
