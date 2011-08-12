# vector-2d

Collection of 2D Vector Operations.

## Usage

     (ns vector-ops
       (:refer-clojure :exclude [+ - * =])
       (:use (clojure.contrib.generic [arithmetic :only [+ - *]]
                                      [comparison :only [=]]))
       (:use [vector-2d.core] :reload-all))

     (let [u (vector-2d 1 1)
           v (vector-2d 2 2)] 
       (+ u v))

     (let [guard-pos (vector-2d 1 3)
           guard-facing (vector-2d 1 1)
           hero-pos (vector-2d 3 2)] 
       (if (in-view-cone? guard-pos guard-facing 80 hero-pos)
         "Guard Can See!!"
         "Guard Can't See!!"))

## Contains

 -  distance-behind-line
 -  quadrant
 -  closest-point-on-line
 -  in-view-cone?
 -  dist
 -  point-in-circle?
 -  project
 -  parallel?
 -  bearing
 -  line-circle-collision
 -  line-intersection
 -  magnitude
 -  octant
 -  rectangle-circle-collision
 -  closest-point-on-circle
 -  normalize
 -  negative
 -  rotate
 -  vector-2d
 -  point-in-rectangle
 -  dot-product
 -  perpendicular?
 -  vector-2d-struct
 -  bisect-angle
 -  circle-circle-collision

## License

Beerware Revision 42
