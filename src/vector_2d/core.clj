(ns vector-2d.core
  (:refer-clojure :exclude [deftype])
  (:use [clojure.contrib.types :only (deftype)]
	[clojure.contrib.generic :only (root-type)]
        [clojure.contrib.seq-utils :only (find-first)])
  (:require [clojure.contrib.generic.arithmetic :as ga]
	    [clojure.contrib.generic.comparison :as gc]))

(defstruct vector-2d-struct :x :y)

(deftype ::vector-2d vector-2d
  (fn [x y] (struct vector-2d-struct x y))
  (fn [{x :x y :y}] (vector-2d x y)))

(derive ::vector-2d root-type)

(defn to-polar [{x :x y :y}]
  {:r (Math/sqrt (+ (* x x) (* y y))) :t (Math/atan2 y x)})

(defn to-cartesian [r t]
  (vector-2d (* r (Math/cos t)) (* r (Math/sin t))))

(defmethod ga/+ [::vector-2d ::vector-2d]
  [{ux :x uy :y} {vx :x vy :y}]
  (vector-2d (ga/+ ux vx) (ga/+ uy vy)))

(defmethod ga/- [::vector-2d ::vector-2d]
  [{ux :x uy :y} {vx :x vy :y}]
  (vector-2d (ga/- ux vx) (ga/- uy vy)))

(defmethod ga/* [::vector-2d Object]
  [{ux :x uy :y} s]
  (vector-2d (* ux s) (* uy s)))

(defmethod gc/= [::vector-2d ::vector-2d]
  [{ux :x uy :y} {vx :x vy :y}]
  (and (gc/= ux vx) (gc/= uy vy)))

(defn negative 
  "Return a new vector in the opposite direction."
  [{ux :x uy :y}]
  (vector-2d (- ux) (- uy)))

(defn magnitude 
  "Return the length of the vector."
  [{x :x y :y}]
  (Math/sqrt (+ (* x x) (* y y))))

(defn dist 
  "Return the distance between two vectors."
  [u v]
  (magnitude (ga/- u v)))

(defn dist-manhattan
  "Return the manhattan distance between two vectors."
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn normalize 
  "Returns the unit vector of the supplied vector."
  [v]
  (let [{:keys [x y]} v
	mag (magnitude v)] 
    (if-not (== mag 0)
      (vector-2d (/ x mag) (/ y mag)) (vector-2d 0 0))))

(defn dot-product 
  "See, http://en.wikipedia.org/wiki/Dot_product"
  [{ux :x uy :y} {vx :x vy :y}]
  (+ (* ux vx) (* uy vy)))

(defn project 
  "See, http://en.wikipedia.org/wiki/Vector_projection"
  [u v]
  (let [unit-v (normalize v)
	dot (dot-product u unit-v)]
    (ga/* unit-v dot)))

(defn rotate 
  "See, http://en.wikipedia.org/wiki/Rotation_(mathematics)"
  [{ux :x uy :y} angle]
  (let [s (Math/sin (Math/toRadians angle))
	c (Math/cos (Math/toRadians angle))]
    (vector-2d (- (ga/* c ux) (ga/* s uy)) 
	       (+ (ga/* s ux) (ga/* c uy)))))

(defn bisect-angle 
  "Returns the vector that lies halfway between vectors."
  [u v]
  (ga/+ (normalize u) (normalize v)))

(defn in-view-cone? 
  "Checks if the point is in viewing cone."
  [view-pos direction angle point]
  (let [vp (ga/- point view-pos)
	cos (/ (dot-product vp direction)
	       (ga/* (magnitude vp) (magnitude direction)))]
    (> cos (Math/cos (Math/toRadians angle)))))

(defn parallel? 
  "Returns true if vectors are parallel to each other."
  [u v]
  (or (gc/= (normalize u) (normalize v))
      (gc/= (normalize u) (negative (normalize v)))))

(defn perpendicular? 
  "Returns true if vectors are perpendicular to each other."
  [u v]
  (= 0 (Math/abs (dot-product u v))))

(defn closest-point-on-line 
  "Calculate a point on the line AB that is closest to point C."
  [a b c]
  (let [ac (ga/- c a)
	ab (ga/- b a)
	proj-mag (dot-product ac (normalize ab))]
    (cond (< proj-mag 0) a
	  (> proj-mag (magnitude ab)) b
	  :default (ga/+ (project ac ab) a))))

(defn closest-point-on-circle [p c r]
  (let [v (normalize (ga/- p c))]
    (ga/+ c (ga/* v r))))

(defn line-circle-collision 
  "Given line segment AB and circle C with radius R, returns true if 
   circle collides with the line segmen."
  [a b c r]
  (let [closest (closest-point-on-line a b c)
	distance (magnitude (ga/- c closest))]
    (if (<= distance r)
      true false)))

(defn point-in-circle? 
  "Test if point a falls within the circle c with radius r."
  [{ax :x ay :y} {cx :x cy :y} r]
  (gc/< (+ (Math/pow (- cx ax) 2) (Math/pow (- cy ay) 2)) (Math/pow r 2)))

(defn bearing 
  "Direction of u with respect to v."
  [{ux :x uy :y} {vx :x vy :y}]
  (let [ang (- (/ Math/PI 2) (Math/atan2 (- uy vy) (- ux vx)))] 
    (cond (> ang Math/PI) (- ang (* 2 Math/PI))
	  (< ang (- Math/PI)) (+ ang (* 2 Math/PI))
	  :else ang)))

(defn octant
  "Provides info on which octant (1-8) the vector lies in."
  [u]
  (let [{:keys [x y]} u
        {:keys [r t]} (to-polar u)
        angle (let [a (Math/toDegrees t)]
                (if (< a 0) (+ 360 a) a))
        bounds [[1 45] [2 90] [3 135] [4 180] [5 225] [6 270] [7 315] [8 360]]]
    (first (find-first #(< angle (second %)) bounds))))

(defn quadrant
  "Provides info on which quadrant (1-4) the vector lies in."
  [u]
  (let [{:keys [x y]} u
        {:keys [r t]} (to-polar u)
        angle (let [a (Math/toDegrees t)]
                (if (< a 0) (+ 360 a) a))
        bounds [[1 90] [2 180] [3 270] [4 360]]]
    (first (find-first #(< angle (second %)) bounds))))

(defn distance-behind-line
  "Given line ab calculate a point c, d distance behind a."
  [a b d]
  (let [b (ga/- b a)
        {:keys [r t]} (to-polar b)]
    (ga/+ a (to-cartesian (- d) t))))

(defn circle-circle-collision
  "Given two circles c1 with radius r1 and c2 with radius r2, return true if circles collide."
  [c1 r1 c2 r2]
  (<= (dist c1 c2) (Math/sqrt (Math/pow (+ r1 r2) 2))))

(defn line-intersection
  "Given two line segments ab and cd returns the intersection point if they intersect otherwise nil.
   http://paulbourke.net/geometry/lineline2d/"
  [{x1 :x y1 :y} {x2 :x y2 :y} {x3 :x y3 :y} {x4 :x y4 :y}]
  (when (and (not= (vector-2d x1 y1) (vector-2d x2 y2))
             (not= (vector-2d x3 y3) (vector-2d x4 y4)))
    
    (let [ua (/ (- (* (- x4 x3) (- y1 y3))
                   (* (- y4 y3) (- x1 x3)))
                
                (- (* (- y4 y3) (- x2 x1))
                   (* (- x4 x3) (- y2 y1))))
          
          ub (/ (- (* (- x2 x1) (- y1 y3))
                   (* (- y2 y1) (- x1 x3)))
                
                (- (* (- y4 y3) (- x2 x1))
                   (* (- x4 x3) (- y2 y1))))]
      
      (when (and (>= ua 0)
                 (<= ua 1)
                 (>= ub 0)
                 (<= ub 1))
        
        (vector-2d (+ x1 (* ua (- x2 x1)))
                   (+ y1 (* ua (- y2 y1))))))))

(defn- in-range? [x a b]
  (cond (< x a) false
        (> x b) false
        :else true))

(defn point-in-rectangle
  "Given corners of a rectangle a,b,c,d and a point p,
   returns true if p in the rectangle,"
  [a b c d p]
  (let [min-x (apply min (map #(:x %) [a b c d]))
        max-x (apply max (map #(:x %) [a b c d]))
        min-y (apply min (map #(:y %) [a b c d]))
        max-y (apply max (map #(:y %) [a b c d]))]
    (and (in-range? (:x p) min-x max-y)
         (in-range? (:y p) min-y max-y))))

(defn rectangle-circle-collision
  "Given corners of a rectangle a,b,c,d and a circle with
   center cc with radius r return true if circle collides with the
   rectangle."
  [[a b c d] cc r]
  (or (point-in-rectangle a b c d cc)
      (line-circle-collision a b cc r)
      (line-circle-collision b c cc r)
      (line-circle-collision c d cc r)
      (line-circle-collision d a cc r)))

(defn angle-between-points [u v]
  (let [{:keys [x y]} (ga/- u v)]
    (/ (* 180 (Math/atan2 y x)) Math/PI)))