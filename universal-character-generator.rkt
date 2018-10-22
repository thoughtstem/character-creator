#lang racket
; Resources: https://github.com/jrconway3/Universal-LPC-spritesheet

(provide create-random-character
         create-custom-character
         show-character-options
         show-hue-options
         hue)

(require 2htdp/image)
(require game-engine)
(require racket/runtime-path)
(define-runtime-path package-path "assets")

; generate random basic character: body + eyes + top + bottom + shoes
(define (random-character-basic gender)
  (define dir (list "shoes" "bottoms" "tops" "eyes" "body"))                      
  
  (define dir-path-list (map (lambda (s)
                               (build-path  package-path
                                            "Universal-LPC-spritesheets"
                                            "random-character"
                                            gender
                                            s))
                        dir))  
  (define body-parts-names (map (lambda (path)
                                  (first (shuffle (directory-list path))))
                                dir-path-list))
  (define body-parts-path (map (lambda (s1 s2)
                                 (build-path s1 s2))
                               dir-path-list body-parts-names))
  (define body-parts-images (map bitmap/file body-parts-path))
  (apply overlay body-parts-images))

; accessorize basic character, add: head-or-hat + belts + arms + random
(define (random-character-accessorised character
                                              gender)
  (define head-or-hair (if (> (modulo (current-seconds) 3) 0) "hair" "hats"))
  (define dir (append (list "misc" "arms" "belts") (list head-or-hair)))
  (define dir-path-list (map (lambda (s)
                               (build-path package-path
                                           "Universal-LPC-spritesheets"
                                           "random-character"
                                           gender
                                           s))
                        dir))  
  (define body-parts-names (map (lambda (path)
                                  (first (shuffle (directory-list path))))
                                dir-path-list))
  (define body-parts-path (map (lambda (s1 s2)
                                 (build-path s1 s2))
                               dir-path-list body-parts-names))
  (define body-parts-images (map bitmap/file body-parts-path))  
  (define acessorised-character (overlay (apply overlay (list-tail body-parts-images 1)) character)) 
  ; adds "rare" random feature to the character
  (if (eq? (modulo (current-seconds) 5) 0) (overlay  (first body-parts-images) acessorised-character)
                                           acessorised-character))

; get random-unifersal-lpc-character spritesheet, cut and reorder
(define (create-random-character #:gender [gender "r"])
  (define gender-dir (get-gender-dir gender))
  (define spritesheet-full (random-character-accessorised (random-character-basic gender-dir) gender-dir))
  (cut-rearrange-spritesheet spritesheet-full)
  )

; generate random basic character: body + eyes + top + bottom + shoes
(define (custom-character-basic gender
                                item-list
                                hue-list) 
  (define dir (list "shoes" "bottoms" "tops" "eyes" "body"))                      
  (define dir-path-list (map (lambda (s)
                               (build-path  package-path
                                            "Universal-LPC-spritesheets"
                                            "custom-character"
                                            gender
                                            s))
                        dir))
  
  (define body-parts-names (map (lambda (path item-num)
                                  (if (eq? item-num -1) "empty"
                                      (list-ref (directory-list path) item-num)))
                                dir-path-list item-list))

  (define body-parts-path (map (lambda (s1 s2)
                                                      (if (eq? s2 "empty") #f (build-path s1 s2)))
                                                    dir-path-list body-parts-names))
  (define body-parts-images (map cut-rearrange-spritesheet (map (lambda(path)
                                                                  (if (eq? path #f) empty-image (bitmap/file path))) body-parts-path)))
  (define body-parts-images-hue (map (lambda (hue img)
                                       (if (eq? hue 0) img (change-img-hue hue img)))
                                     hue-list body-parts-images)) 
  (apply overlay body-parts-images-hue))

; accessorize basic character, add: head-or-hat + belts + arms + random
(define (custom-character-accessorised character
                                       gender
                                       item-list
                                       hue-list)
  (define dir (list "misc" "arms" "belts" "hats" "hair"))
  (define dir-path-list (map (lambda (s)
                               (build-path package-path
                                           "Universal-LPC-spritesheets"
                                           "custom-character"
                                           gender
                                           s))
                        dir))       
  (define body-parts-names (map (lambda (path item-num)
                                  (if (eq? item-num -1) "empty"
                                      (list-ref (directory-list path) item-num)))
                                dir-path-list item-list))
  (define body-parts-path (map (lambda (s1 s2)
                                 (if (eq? s2 "empty") #f (build-path s1 s2)))
                               dir-path-list body-parts-names))
  (define body-parts-images (map cut-rearrange-spritesheet (map (lambda(path)
                                                                  (if (eq? path #f) empty-image (bitmap/file path))) body-parts-path)))
  (define body-parts-images-hue (map (lambda (hue img)
                                       (if (eq? hue 0) img (change-img-hue hue img)))
                                     hue-list body-parts-images))
  (cond
    [(eq? (length body-parts-images-hue) 0) character]
    [(eq? (length body-parts-images-hue) 1) (overlay (first body-parts-images-hue) character)]
    [else (overlay (apply overlay body-parts-images-hue) character)]))

;create custom character
(define/contract (create-custom-character    #:gender  gender
                                             #:body    [body   (hue 0 0)]
                                             #:eyes    [eyes   (hue 0 0)]
                                             #:top     [top    (hue 0 0)]
                                             #:bottom  [bottom (hue 0 0)]
                                             #:shoes   [shoes  (hue 0 0)]
                                             #:hat     [hat    (hue -1 0)]
                                             #:hair    [hair   (hue 0 0)]
                                             #:belt    [belt   (hue -1 0)]
                                             #:arms    [arms   (hue -1 0)]
                                             #:misc    [misc   (hue -1 0)])
  (->* (#:gender string?)
       (#:body   (or/c integer? pair?)
        #:eyes   (or/c integer? pair?)
        #:top    (or/c integer? pair?)
        #:bottom (or/c integer? pair?)
        #:shoes  (or/c integer? pair?)
        #:hat    (or/c integer? pair?)
        #:hair   (or/c integer? pair?)
        #:belt   (or/c integer? pair?)
        #:arms   (or/c integer? pair?)
        #:misc   (or/c integer? pair?))
      image?)

  (define input-list     (list shoes bottom top eyes body misc arms belt hat hair))
  (define input-to-pairs (map (lambda (arg)
                                (if (pair? arg) arg (cons arg 0))) input-list))

  ;(list shoes bottoms tops eyes body)
  (define basic-items            (map (lambda (arg)
                                        (car arg)) (take input-to-pairs 5)))
  (define basic-items-hue        (map (lambda (arg)
                                        (cdr arg)) (take input-to-pairs 5)))
  ;(list random arms belts hats hair)
  (define accessorised-items     (map (lambda (arg)
                                        (car arg)) (drop input-to-pairs 5)))
  (define accessorised-items-hue (map (lambda (arg)
                                        (cdr arg)) (drop input-to-pairs 5)))
  (define gender-dir (get-gender-dir gender))
  (custom-character-accessorised (custom-character-basic gender-dir basic-items basic-items-hue) gender-dir
                                                                                                 accessorised-items
                                                                                                 accessorised-items-hue))

(define (cut-rearrange-spritesheet spritesheet-full)
  ; universal lps character spritesheet rows, columns, row-number, columns-off
  (define r     21)
  (define c     13)
  (define n     9)
  (define r-on  4)
  (define c-off 4)

  ;converts to one row facing right
  #;(~> spritesheet-full
      (sheet->costume-list _ c r (* r c))
      (drop _ (* (- n 1) c))
      (take _ (- c c-off)))

  ;drop c-off number of rightmost col
  (define spritesheet-c-off
    (apply beside (~> spritesheet-full
                    (sheet->costume-list _ c 1 (* 1 c))
                    (take _ (- c c-off)))))
  
  ;take r-on rows of frames, get list of rows
  (define spritesheet-cut (~> spritesheet-c-off
                          (sheet->costume-list _ 1 r (* r 1))
                          (drop _ (* (- n 1) 1))
                          (take _ r-on )))

  ;rearrange rows as front/left/right/back
  (above (list-ref spritesheet-cut 2)
         (list-ref spritesheet-cut 1)
         (list-ref spritesheet-cut 3)
         (list-ref spritesheet-cut 0))
  )

; requires a full universal lpc character spritesheet (21 rows/ 13 columns)
(define (get-frame spritesheet)
  (define r     21)
  (define c     13)
  (define n     11)

  (first (~> spritesheet
           (sheet->costume-list _ c r (* r c))
           (drop _ (* (- n 1) c))
           (take _ 1)))
  )

; gets a correct dir name based on desired sprite gender
; if current seconds is even produce female sprite, male otherwise
(define (get-gender-dir gender)
  (define gender-dir (cond
                       [(eq? gender "m") "male"]
                       [(eq? gender "f") "female"]
                       [(eq? gender "r") (if (even? (current-seconds)) "female" "male")]))
  gender-dir
  )

(define (show-character-options #:gender gender)
  (define gender-dir (get-gender-dir gender))
  
  (define basic-dir        (list "shoes" "bottoms" "tops" "eyes" "body"))
  (define accessorises-dir (list "misc" "arms" "belts" "hair" "hats"))

  (above/align "left" (character-builder-layout basic-dir gender-dir)
                      (character-builder-layout accessorises-dir gender-dir)))

; puts list items in rows by n items each
(define (split-by lst n init-lst)
   (if (> (length lst) n)
       (above/align "left" (apply beside (add-numbers-img-list (take lst n) init-lst)) (split-by (drop lst n) n init-lst))
       (above/align "left" (cond
                             [(empty? lst)         empty-image]
                             [(eq? (length lst) 1) (first (add-numbers-img-list lst init-lst))]
                             [else                 (apply beside (add-numbers-img-list lst init-lst))])
                    empty-image)))

; adds item number under image
(define (add-numbers-img-list img-list img-list-full)
    (map (lambda (img)
           (above/align "center" img
                                 (text/font (number->string (index-of img-list-full img))
                                            12 "olive" "Gill Sans" 'swiss 'normal 'bold #f)))
       img-list))

(define (character-builder-layout dir gender-dir)
  (define dir-path-list (map (lambda (s)
                               (build-path  package-path
                                            "Universal-LPC-spritesheets"
                                            "custom-character"
                                            gender-dir
                                            s))
                        dir))
  (define character-options-names (map (lambda (path)
                                    (directory-list path))
                                  dir-path-list))
  (define character-options-path (map (lambda (dir list-files)
                                   (map (lambda (file)
                                          (build-path dir file))
                                   list-files))
                                   dir-path-list character-options-names))
  (define character-options-img (map (lambda (list-files)
                                       (foldl (lambda (file l)
                                              (cons (get-frame (bitmap/file file)) l)) '() list-files))
                                     character-options-path)) 
  (foldl (lambda (text parts whole)
           (above/align "left" (text/font text 24 "olive" "Gill Sans" 'swiss 'normal 'bold #f)
                               (split-by (reverse parts) 8 (reverse parts))
                               whole)) empty-image dir character-options-img))

(define (show-hue-options #:gender   gender
                          #:category dir
                          #:item     item-num)
  (define dir-path (build-path package-path "Universal-LPC-spritesheets"
                                            "custom-character"
                                            (get-gender-dir gender)
                                            dir))
  (define file-name (list-ref (directory-list dir-path) item-num))
  (define item-img (get-frame (bitmap/file (build-path dir-path file-name))))

  (define l (range 0 350 20))
  (define hue-variations (map (lambda (arg)
                                (change-img-hue arg item-img))
                              l))
  (split-by-numbered hue-variations 6 hue-variations l))

(define (split-by-numbered lst n init-lst numbers)
   (if (> (length lst) n)
       (above/align "left" (apply beside (add-numbers-img-list-numbered (take lst n) init-lst (take numbers n))) (split-by-numbered (drop lst n) n init-lst (drop numbers n)))
       (above/align "left" (cond
                             [(empty? lst)         empty-image]
                             [(eq? (length lst) 1) (first (add-numbers-img-list-numbered lst init-lst numbers))]
                             [else                 (apply beside (add-numbers-img-list-numbered lst init-lst numbers))])
                    empty-image)))

; adds item number under image
(define (add-numbers-img-list-numbered img-list img-list-full numbers)
    (map (lambda (img num)
           (above/align "center" img
                                 (text/font (number->string num)
                                            12 "olive" "Gill Sans" 'swiss 'normal 'bold #f)))
       img-list numbers))

(define/contract (hue item hue)
  (-> integer? integer? pair?)
  (cons item hue))
