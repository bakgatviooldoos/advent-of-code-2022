#lang racket

#|--- Day 12: Hill Climbing Algorithm ---

You try contacting the Elves using your handheld device, but the river you're following must be too low to get a decent signal.

You ask the device for a heightmap of the surrounding area (your puzzle input). The heightmap shows the local area from above broken
into a grid; the elevation of each square of the grid is given by a single lowercase letter, where a is the lowest elevation, b is
the next-lowest, and so on up to the highest elevation, z.

Also included on the heightmap are marks for your current position (S) and the location that should get the best signal (E). Your
current position (S) has elevation a, and the location that should get the best signal (E) has elevation z.

You'd like to reach E, but to save energy, you should do it in as few steps as possible. During each step, you can move exactly
one square up, down, left, or right. To avoid needing to get out your climbing gear, the elevation of the destination square can
be at most one higher than the elevation of your current square; that is, if your current elevation is m, you could step to
elevation n, but not to elevation o. (This also means that the elevation of the destination square can be much lower than the elevation
of your current square.)

For example:

Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi

Here, you start in the top-left corner; your goal is near the middle. You could start by moving down or right, but eventually you'll
need to head toward the e at the bottom. From there, you can spiral around to the goal:

v..v<<<<
>v.vv<<^
.>vv>E^^
..v>>>^^
..>>>>>^

In the above diagram, the symbols indicate whether the path exits each square moving up (^), down (v), left (<), or right (>).
The location that should get the best signal is still E, and . marks unvisited squares.

This path reaches the goal in 31 steps, the fewest possible.

What is the fewest steps required to move from your current position to the location that should get the best signal?

--- Part Two ---

As you walk up the hill, you suspect that the Elves will want to turn this into a hiking trail. The beginning isn't very scenic,
though; perhaps you can find a better starting point.

To maximize exercise while hiking, the trail should start as low as possible: elevation a. The goal is still the square marked E.
However, the trail should still be direct, taking the fewest steps to reach its goal. So, you'll need to find the shortest path
from any square at elevation a to the square marked E.

Again consider the example from above:

Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi

Now, there are six choices for starting position (five marked a, plus the square marked S that counts as being at elevation a).
If you start at the bottom-left square, you can reach the goal most quickly:

...v<<<<
...vv<<^
...v>E^^
.>v>>>^^
>^>>>>>^

This path reaches the goal in only 29 steps, the fewest possible.

What is the fewest steps required to move starting from any square with elevation a to the location that should get the best
signal?|#

(define-values (topograph START END)
  (with-input-from-file "day-12-1.txt"
    (lambda ()
      (define start 'none)
      (define end   'none)
      
      (values (for/vector ([line (in-lines)]
                           [row  (in-naturals 0)])
                (for/vector ([height line]
                             [col    (in-naturals 0)])
                  (cond [(equal? #\S height)
                         (set! start (list row col))
                         #\a]
                        [(equal? #\E height)
                         (set! end (list row col))
                         #\z]
                        [else
                         height])))
              start
              end))))

(define ROWS (vector-length topograph))
(define COLS (vector-length (vector-ref topograph 0)))

(define (neighbours y x)
  (for/list ([dy '(-1  0 +1  0)]
             [dx '( 0 +1  0 -1)]
             #:when (and (<= 0 (+ y dy) (- ROWS 1))
                         (<= 0 (+ x dx) (- COLS 1))))
    (list (+ y dy) (+ x dx))))

(define (height-at y x)
  (vector-ref (vector-ref topograph y) x))

(define (find-path . start)
  (define visited
    (for/vector ([y (in-range 0 ROWS)])
      (for/vector ([x (in-range 0 COLS)])
        (if (member (list y x) start) 0 -inf.0))))
  
  (define (not-visited? y x)
    (negative? (vector-ref (vector-ref visited y) x)))

  (define (visit! step y x)
    (vector-set! (vector-ref visited y) x step))

  (define path-length
    (let loop ([visitors start]
               [step     1])
      (define next-visitors
        (for/fold ([next-visitors (list)])
                  ([visitor       visitors])
          (define height
            (apply height-at visitor))
          
          (values
           (append
            (for/list ([neighbour (apply neighbours visitor)]
                       #:do   [(define next-height
                                 (apply height-at neighbour))]
                       #:when (and (<= (char->integer next-height)
                                       (+ (char->integer height) 1))
                                   (apply not-visited? neighbour)))
              (apply visit! (cons step neighbour))
              neighbour)
            next-visitors))))

      (cond [(member END next-visitors)
             step]
            [(empty? next-visitors)
             +inf.0]
            [else
             (loop next-visitors (+ step 1))])))
  path-length)

(define (basin-edge? y x)
  (for/or ([neighbour (neighbours y x)])
    (< (char->integer #\a)
       (char->integer (apply height-at neighbour)))))

(define lowest-elevations
  (for*/list ([y (in-range 0 ROWS)]
              [x (in-range 0 COLS)]
              #:do   [(define height (height-at y x))]
              #:when (and (equal? #\a height)
                          (basin-edge? y x)))
    (list y x)))

(time
 (displayln
  (format "What is the fewest steps required to move from your current position to the location that should get the best signal?~n~a"
          (find-path START))))
(newline)

(time
 (displayln
  (format "What is the fewest steps required to move starting from any square with elevation a to the location that should get the best
signal?~n~a"
          (inexact->exact
           (apply find-path lowest-elevations)))))
