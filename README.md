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

 -  closest-point-on-line  -  Calculate a point on the line AB that is closest to point C.
 -  in-view-cone?  -  Checks if the point is in viewing cone.
 -  dist  -  Return the distance between two vectors.
 -  project  -  See, http://en.wikipedia.org/wiki/Vector_projection
 -  parallel?  -  Returns true if vectors are parallel to each other.
 -  line-circle-collision  -  Given line segment AB and circle C with radius R, returns true if 
   circle collides with the line segmen.
 -  magnitude  -  Return the length of the vector.
 -  normalize  -  Returns the unit vector of the supplied vector.
 -  negative  -  Return a new vector in the opposite direction.
 -  rotate  -  See, http://en.wikipedia.org/wiki/Rotation_(mathematics)
 -  dot-product  -  See, http://en.wikipedia.org/wiki/Dot_product
 -  perpendicular?  -  Returns true if vectors are perpendicular to each other.
 -  vector-2d-struct  -  nil
 -  bisect-angle  -  Returns the vector that lies halfway between vectors.
 -  point-in-circle? - Test if point a falls within the circle c with radius r.
 -  bearing - Direction of u with respect to v.

## License

Beerware Revision 42
