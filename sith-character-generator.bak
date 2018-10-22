#lang racket
; Resources: http://untamed.wild-refuge.net/rmxpresources.php?characters

(provide sith-character)

(require racket/runtime-path)

(define-runtime-path package-path "assets")

; get random spritesheet from Sithjester sprite collection
(define (sith-character)
  (define dir-path (build-path  package-path "Sith-spritesheets"))
  
  (bitmap/file (build-path dir-path
                           (first (shuffle (directory-list dir-path))))))




