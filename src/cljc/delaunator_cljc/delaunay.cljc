(ns delaunator-cljc.delaunay)


(def unchecked false)
(def +! (if unchecked unchecked-add +))
(def -! (if unchecked unchecked-subtract -))
(def *! (if unchecked unchecked-multiply *))

;; Implementing abs is comparable perfwise to using type hinting atm...
(defn abs [n]
  (if (neg? n) (-! 0 n) n))

(defn swap [arr a b]
  (let [av (nth arr a)
        bv (nth arr b)]
    (-> arr
        (assoc b av)
        (assoc a bv))))

(defn quicksort
  [idxs dists left right]
  (if (<= (- right left) 20)
    (loop [i (inc left)
           idxs idxs]
      (if (<= i right)
        (let [tmpidx (nth idxs i)
              tmpdist (nth dists tmpidx)
              j (dec i)
              [idxs j] (loop [idxs idxs j j]
                         (if
                           (and (>= j left) (> (nth dists (nth idxs j)) tmpdist))

                           (recur (assoc idxs (inc j) (nth idxs j)) (dec j))
                           [idxs j]))]
          (recur (inc i)
                 (assoc idxs (inc j) tmpidx)))
        idxs))
    (let [median (bit-shift-right (+ left right) 1)
          i$1 (inc left)
          j$1 right
          idxs (swap idxs median i$1)
          idxs (if (> (nth dists (nth idxs left)) (nth dists (nth idxs right)))
                 (swap idxs left right)
                 idxs)

          idxs (if (> (nth dists (nth idxs i$1)) (nth dists (nth idxs right)))
                 (swap idxs i$1 right)
                 idxs)

          idxs (if (> (nth dists (nth idxs left)) (nth dists (nth idxs i$1)))
                 (swap idxs left i$1)
                 idxs)

          tmp1  (nth idxs i$1)
          dist1 (nth dists tmp1)

          [idxs i$1 j$1] (loop [idxs idxs i$1 i$1 j$1 j$1]
                           (let [i$1 (loop [i$1 i$1]
                                       (let [i$1 (inc i$1)]
                                         (if (< (nth dists (nth idxs i$1)) dist1)
                                           (recur i$1)
                                           i$1)))
                                 j$1 (loop [j$1 j$1]
                                       (let [j$1 (dec j$1)]
                                         (if (> (nth dists (nth idxs j$1)) dist1)
                                           (recur j$1)
                                           j$1)))]
                             (if (< j$1 i$1)
                               [idxs i$1 j$1]
                               (recur (swap idxs i$1 j$1)
                                      i$1
                                      j$1))))
          idxs (-> idxs
                   (assoc (inc left) (nth idxs j$1))
                   (assoc j$1 tmp1))]
      (if (>= (inc (- right i$1)) (- j$1 left))
        (-> idxs
            (quicksort dists i$1 right)
            (quicksort dists left (dec j$1)))
        (-> idxs
            (quicksort dists left (dec j$1))
            (quicksort dists i$1 right))))))



(comment
  (def in  [0.5 2.5 0.5 0.5 2.5 6.5 12.5 2.5 6.5 30.5 20.5 12.5 42.5 20.5 30.5 42.5 56.5 72.5 90.5 56.5 72.5 0.5])
  (def out [0 3 21 2 4 7 1 5 8 6 11 13 10 9 14 12 15 16 19 17 20 18])

  (let [dists in
        n     (count dists)
        idxs  (vec (range n))]
    (quicksort idxs dists 0 (dec n))))



(def EPSILON (Math/pow 2 -52))

(defn vector-array
  ([n] (vector-array n 0))
  ([n x]
   (let [v #?(:clj  (vector-of :int)
              :cljs (vector))]
     (into v (repeat n x)))))



(defn pseudo-angle
  "monotonically increases with real angle, but doesn't need expensive trigonometry"
  [dx dy]
  (let [p (/ dx (+! (abs dx)
                    (abs dy)))]
    ;; values range between [0..1]
    (/ (if (> dy 0)
         (-! 3 p)
         (+! 1 p))
       4)))

(defn hash-key
  "An angle based hash function"
  [[cx cy] hash-size x y]
  (mod (Math/floor (*! (pseudo-angle (-! x cx) (-! y cy)) hash-size)) hash-size))


(defn dist [ax ay bx by]
  (let [dx (-! ax bx)
        dy (-! ay by)]
    (+! (*! dx dx) (*! dy dy))))

(defn orient-if-sure
  "return 2d orientation sign if we're confident in it through J. Shewchuk's error bound check"
  [px py rx ry qx qy]
  (let [l (*! (-! ry py) (-! qx px))
        r (*! (-! rx px) (-! qy py))]
    (if (>= (abs (-! l r)) (*! 3.3306690738754716e-16 (abs (+! l r))))
      (-! l r)
      0)))

(defn orient
  "a more robust orientation test that's stable in a given triangle (to fix robustness issues)"
  [rx ry qx qy px py]
  (let [fst (orient-if-sure px py rx ry qx qy)]
    (if-not (zero? fst)
      (< fst 0)
      (let [snd (orient-if-sure rx ry qx qy px py)]
        (if-not (zero? snd)
          (< snd 0)
          (let [lst (orient-if-sure qx qy px py rx ry)]
            (if-not (zero? lst)
              (< lst 0)
              false)))))))

(defn in-circle [[ax ay] [bx by] [cx cy] [px py]]
  (let [dx (-! ax px)
        dy (-! ay py)
        ex (-! bx px)
        ey (-! by py)
        fx (-! cx px)
        fy (-! cy py)

        ap (+! (*! dx dx) (*! dy dy))
        bp (+! (*! ex ex) (*! ey ey))
        cp (+! (*! fx fx) (*! fy fy))]
    (< (+! (-! (*! (-! (*! ey cp) (*! bp fy)) dx)
               (*! (-! (*! ex cp) (*! bp fx)) dy))
           (*! (-! (*! ex fy) (*! ey fx)) ap))
       0)))

(defn circumradius [ax ay bx by cx cy]
  (let [dx (-! bx ax)
        dy (-! by ay)
        ex (-! cx ax)
        ey (-! cy ay)

        bl (+! (*! dx dx) (*! dy dy))
        cl (+! (*! ex ex) (*! ey ey))
        d (/ 0.5 (-! (*! dx ey) (*! dy ex)))

        x (*! (-! (*! ey bl) (*! dy cl)) d)
        y (*! (-! (*! dx cl) (*! ex bl)) d)]
    (+! (*! x x) (*! y y))))

(defn circumcenter [ax ay bx by cx cy]
  (let [dx (-! bx ax)
        dy (-! by ay)
        ex (-! cx ax)
        ey (-! cy ay)

        bl (+! (*! dx dx) (*! dy dy))
        cl (+! (*! ex ex) (*! ey ey))
        d (/ 0.5 (-! (*! dx ey) (*! dy ex)))

        x (+! ax (*! (-! (*! ey bl) (*! dy cl)) d))
        y (+! ay (*! (-! (*! dx cl) (*! ex bl)) d))]
    [x y]))




(comment
  (let [coords (take 5 points)
        [[max-x min-x] [max-y min-y]] (mapv (partial apply (juxt max min)) (apply map vector coords))
        cx (/ (+! min-x max-x) 2)
        cy (/ (+! min-y max-y) 2)]
    (reduce (fn [[d seed] [x y]] (let [new-d (dist cx cy x y)] (if (< new-d d) [new-d [x y]] [d seed]))) [##Inf [##Inf ##Inf]] coords)))

(defn find-visible-edge [hull-hash hull-next center hash-size x y]
  (let [key (hash-key center hash-size x y)]
    (loop [idx 0]
      (let [lookup (mod (+! key idx) hash-size)
            start (nth hull-hash lookup -1)]
        (if (and (not= start -1) (not= start (hull-next start)))
          start
          (recur (inc idx)))))))


(defn assoc-if [m test k v]
  (if test
    (assoc-in m k v)
    m))

(defn add-link [state vertex edge]
  (-> state
      (assoc-in [:half-edges vertex] edge)
      (assoc-if (not= edge -1) [:half-edges edge] vertex)))


(defn add-triangle
  "add a new triangle given vertex indices and adjacent half-edge ids"
  [{:keys [triangles-len] :as state} seed-idx point-idx location-idx a b c]
  (-> state
      (update :triangles assoc triangles-len seed-idx
              (+! triangles-len 1) point-idx
              (+! triangles-len 2) location-idx)
      (add-link triangles-len a)
      (add-link (+! triangles-len 1) b)
      (add-link (+! triangles-len 2) c)
      (update :triangles-len +! 3)))


(defn fix-half-edge-reference [{:keys [points hull-start hull-prev hull-tri] :as state} bl a]
  (loop [e hull-start
         prior-e nil]
    (cond

      (= (get hull-tri e) bl)
      (assoc-in state [:hull-tri e] a)

      (and (= e hull-start) (not (nil? prior-e)))         ;; we've been around at least once
      state

      :else
      (recur (get hull-prev e) e))))

(defn legalise [state a]
  ;  if the pair of triangles doesn't satisfy the Delaunay condition
  ;  (p1 is inside the circumcircle of [p0, pl, pr]), flip them,
  ;  then do the same check/flip recursively for the new pair of triangles
  ;
  ;             pl                    pl
  ;            /||\                  /  \
  ;         al/ || \bl            al/    \a
  ;          /  ||  \              /      \
  ;         /  a||b  \    flip    /___ar___\
  ;       p0\   ||   /p1   =>   p0\---bl---/p1
  ;          \  ||  /              \      /
  ;         ar\ || /br             b\    /br
  ;            \||/                  \  /
  ;             pr                    pr
  (loop [{:keys [points triangles half-edges] :as state} state a a stack []]
    (let [b (nth half-edges a)

          a0 (-! a (mod a 3))
          ar (+! a0 (mod (+! a 2) 3))]
      (if (= b -1)

        (if (empty? stack)
          (assoc state :ar ar)
          (recur state (peek stack) (pop stack)))

        (let [b0 (-! b (mod b 3))
              al (+! a0 (mod (+! a 1) 3))
              bl (+! b0 (mod (+! b 2) 3))

              p0-idx (nth triangles ar)
              pr-idx (nth triangles a)
              pl-idx (nth triangles al)
              p1-idx (nth triangles bl)

              p0 (nth points p0-idx)
              pr (nth points pr-idx)
              pl (nth points pl-idx)
              p1 (nth points p1-idx)

              illegal? (in-circle p0 pr pl p1)]
          (cond
            illegal?
            (let [hbl (get-in state [:half-edges bl])
                  ;; edge swapped on the other side of the hull (rare); fix the half-edge reference
                  state (if (= hbl -1)
                          (fix-half-edge-reference state bl a)
                          state)
                  br (+! b0 (mod (+! b 1) 3))]
              (recur (-> state
                         (update :triangles assoc
                                 a p1-idx
                                 b p0-idx)
                         (add-link a hbl)
                         (add-link b (get-in state [:half-edges ar]))
                         (add-link ar bl))
                     a
                     (conj stack br)))

            (empty? stack)
            (assoc state :ar ar)

            :else
            (recur state (peek stack) (pop stack))))))))


(comment
  (legalise {:points     [[0 0] [0 1] [0 2] [1 2] [2 2] [2 1] [2 0] [1 0]]
             :triangles  [[0 1] [1 0] [0 0] [0 1] [0 2] [1 0]]
             :half-edges [5 -1 -1 -1 -1 0]
             :hull-tri   [2 0 0 0 0 0 0 1]
             :hull-next  [1 7 0 0 0 0 0 0]
             :hull-prev  [1 0 0 0 0 0 0 7]
             :hull-hash  [0 7 1]
             :hull-start [0 1]}
            5)


  (legalise {:points     [[0 0] [0 1] [0 2] [1 2] [2 2] [2 1] [2 0] [1 0]]
             :triangles  [[0 1] [1 0] [0 0] [0 1] [0 2] [1 0] [0 2] [1 2] [1 0]]
             :half-edges [5 -1 -1 -1 8 0 -1 -1 4]
             :hull-tri   [2 3 4 0 0 0 0 1]
             :hull-next  [1 2 7 0 0 0 0 0]
             :hull-prev  [7 0 1 0 0 0 0 7]
             :hull-hash  [0 7 1]
             :hull-start [0 1]}
            8))


;;; Generate Triangles from points
(defn calculate-center+bbox [{:keys [points] :as state}]
  (let [;: populate point indices; calculate input data bbox
        [[max-x min-x] [max-y min-y]] (mapv (partial apply (juxt max min)) (apply map vector points))
        cx (/ (+! min-x max-x) 2)
        cy (/ (+! min-y max-y) 2)]
    (assoc state
      :max-x max-x :min-x min-x
      :max-y max-y :min-y min-y
      :cx cx :cy cy)))

(defn pick-seed [{:keys [points cx cy] :as state}]
  (let [;; pick a seed point close to the center
        [_min-dist seed-idx seed] (reduce-kv (fn [[d idx seed] new-idx [x y]] (let [new-d (dist cx cy x y)] (if (< new-d d) [new-d new-idx [x y]] [d idx seed]))) [##Inf [##Inf ##Inf]] points)]
    (assoc state :seed seed :seed-idx seed-idx)))

(defn pick-point [{:keys [points seed] :as state}]
  (let [;; find the point closest to the seed
        [sx sy] seed
        [_min-dist point-idx point] (reduce-kv (fn [[d idx point] new-idx [x y]]
                                                 (let [new-d (dist sx sy x y)]
                                                   (cond
                                                     (= seed [x y]) [d idx point]
                                                     (< new-d d) [new-d new-idx [x y]]
                                                     :else [d idx point])))
                                               [##Inf [##Inf ##Inf]] points)]
    (assoc state :point point :point-idx point-idx)))

(defn pick-location [{:keys [points seed point] :as state}]
  (let [;; find the third point which forms the smallest circumcircle with the first two
        [sx sy] seed
        [px py] point
        [min-radius location-idx location] (reduce-kv (fn [[r idx location] new-idx [x y]]
                                                        (let [new-r (circumradius sx sy px py x y)]
                                                          (cond
                                                            (or (= seed [x y])
                                                                (= point [x y]))
                                                            [r idx location]

                                                            (< new-r r)
                                                            [new-r new-idx [x y]]

                                                            :else
                                                            [r idx location])))
                                                      [##Inf [##Inf ##Inf]] points)]
    (assoc state :location location :location-idx location-idx :min-radius min-radius)))


(defn good-point? [state e xy-idx]
  (if (nil? e)
    ;; likely a near-duplicate point; skip it
    state

    ;; add the first triangle from the point
    (let [{:keys [hull-next hull-tri]} state
          triangles-len (:triangles-len state)
          state (add-triangle state e xy-idx (hull-next e) -1 -1 (hull-tri e))
          {:keys [ar] :as state} (legalise state (+! triangles-len 2))]
      (-> state
          (update :hull-tri assoc
                            xy-idx ar
                            e triangles-len)  ;; keep track of boundary triangles on the hull
          (update :hull-size inc)))))

(defn walk-forward [{:keys [points hull-next] :as state} e x y xy-idx]
  (loop [nxt (get hull-next e)
         state state]
    (let [{:keys [hull-next]} state
          q (get hull-next nxt)
          [nxtx nxty] (nth points nxt)
          [qx qy] (nth points q)
          orientation (orient x y nxtx nxty qx qy)]

      (if (not orientation)                       ;; if orientation is false, stop
        [state nxt]
        (recur q                                  ;; new nxy
               (let [{:keys [triangles-len hull-tri]} state
                     state (add-triangle state nxt xy-idx q (nth hull-tri xy-idx) -1 (nth hull-tri nxt))
                     {:keys [ar] :as state} (legalise state (+! triangles-len 2))]
                 (-> state
                     (update :hull-tri assoc xy-idx ar)
                     (update :hull-next assoc nxt nxt)  ;; mark as removed
                     (update :hull-size dec))))))))

(defn walk-backward [{:keys [points] :as state} start e x y xy-idx]
  (if (= e start)
    (loop [e e
           state state]
      (let [{:keys [hull-prev]} state
            q (get hull-prev e)
            [ex ey] (nth points e)
            [qx qy] (nth points q)
            orientation (orient x y qx qy ex ey)]

        (if (not orientation)                     ;; if orientation is false, stop
          [state e]
          (recur q                                ;; new nxy
                 (let [{:keys [triangles-len hull-tri]} state
                       state (add-triangle state q xy-idx e -1 (nth hull-tri e) (nth hull-tri q))
                       state (legalise state (+! triangles-len 2))]
                   (-> state
                       (update :hull-tri assoc q triangles-len)
                       (update :hull-next assoc e e) ;; mark as removed
                       (update :hull-size dec)))))))
    [state e]))

(defn compute-hull [{:keys [points seed point location hull-hash hull-prev hull-next hull-tri hash-size cx cy xp yp] :as state} [_dist xy-idx [x y]]]
  (cond
    ;; skip near-duplicate points
    (and (or (not (nil? xp)) (not (nil? yp))) (<= (abs (-! x xp)) EPSILON) (<= (abs (-! y yp)) EPSILON))
    state

    ;; skip seed triangle points
    (or (= seed [x y])
        (= point [x y])
        (= location [x y]))
    (assoc state :xp x :yp y)

    :else
    (let [state (assoc state :xp x :yp y)

          ;; find a visible edge on the convex hull using edge hash
          visible-edge (find-visible-edge hull-hash hull-next [cx cy] hash-size x y)

          start (->> visible-edge
                     (get hull-prev))

          e (loop [e start
                   q (get hull-next e)
                   stop? false
                   prior-e nil]
              (let [[ex ey] (nth points e)
                    [qx qy] (nth points q)
                    orientation (orient x y ex ey qx qy)]
                (cond stop? nil

                      orientation                         ;; if orientation is true, stop
                      e

                      :else
                      (recur q                            ;; new e
                             (get hull-next q)            ;; new q (next of new e)

                             ;; Stop if we've gone through everything, except at the beginning
                             (and (= e start)
                                  (not (nil? prior-e)))
                             e))))

          state (good-point? state e xy-idx)

          ;; walk forward through the hull, adding more triangles and flipping recursively)
          [state nxt] (walk-forward state e x y xy-idx)

          ;; walk backward from the other side, adding more triangles and flipping
          [{:keys [cx cy] :as state} e] (walk-backward state start e x y xy-idx)

          [ex ey] (nth points e)]
      (-> state

          ;; update the hull indices
          (assoc :hull-start e)
          (assoc-in [:hull-prev xy-idx] e)
          (assoc-in [:hull-next e] xy-idx)
          (assoc-in [:hull-prev nxt] xy-idx)
          (assoc-in [:hull-next xy-idx] nxt)

          ;; save the two new edges in the hash table
          (assoc-in [:hull-hash (int (hash-key [cx cy] hash-size x y))] xy-idx)
          (assoc-in [:hull-hash (int (hash-key [cx cy] hash-size ex ey))] e)))))

(defn assemble-hull [{:keys [hull-size hull-start hull-next] :as state}]
  (let [[_element hull] (reduce (fn [[element v] idx]
                                  [(nth hull-next element) (assoc v idx element)])
                                [hull-start (vector-array hull-size)]
                                (range hull-size))]
    (assoc state :hull hull)))

(defn cleanup [{:keys [triangles-len] :as state}]
  (-> state
      (update :triangles  subvec 0 triangles-len)
      (update :half-edges subvec 0 triangles-len)

      (select-keys [:triangles-len :triangles :half-edges :hull])))


(defn build-non-collinear-hull [{:keys [points n seed point location seed-idx point-idx location-idx] :as state}]
  (let [hash-size (Math/ceil (Math/sqrt n))
        [sx sy] seed
        [px py] point
        [lx ly] location

        ;; swap the point and location (seed points) for counter-clockwise orientation
        {:keys [point location point-idx location-idx] :as state} (if (orient sx sy px py lx ly)
                                                                    (assoc state :point location :location point :point-idx location-idx :location-idx point-idx)
                                                                    state)
        [px py] point
        [lx ly] location

        center (circumcenter sx sy px py lx ly)
        [cx cy] center

        ;; sort the points by distance from the seed triangle circumcenter
        points+dist (map-indexed (fn [idx [x y]] [(dist x y cx cy) idx [x y]]) points)
        sorted-idx  (quicksort (mapv second points+dist) (mapv first points+dist) 0 (dec n))
        sorted-points (mapv (fn [idx] (nth points+dist idx)) sorted-idx)
        #_#_
        sorted-points (sort-by first points+dist)]
    (->
      state
      (assoc :cx cx :cy cy
             :hull-start seed-idx
             :hull-size 3
             :hash-size hash-size
             :hull-prev (-> (vector-array n)
                            (assoc location-idx point-idx)
                            (assoc seed-idx location-idx)
                            (assoc point-idx seed-idx))
             :hull-next (-> (vector-array n)
                            (assoc location-idx seed-idx)
                            (assoc seed-idx point-idx)
                            (assoc point-idx location-idx))

             :hull-tri (-> (vector-array n)
                           (assoc seed-idx 0)
                           (assoc point-idx 1)
                           (assoc location-idx 2))

             :hull-hash (-> (vector-array hash-size -1)
                            (assoc (int (hash-key center hash-size sx sy)) seed-idx)
                            (assoc (int (hash-key center hash-size px py)) point-idx)
                            (assoc (int (hash-key center hash-size lx ly)) location-idx))
             :hull []
             :triangles-len 0)
      (add-triangle seed-idx point-idx location-idx -1 -1 -1)
      ((partial reduce compute-hull) sorted-points)

      ;; build hull
      (assemble-hull)

      ;; cleanup output
      (cleanup))))

(defn build-collinear-hull [{:keys [points] :as state}]
  ;; order collinear points by dx (or dy if all x are identical)
  ;;   and return the list as a hull
  (let [[fx fy] (first points)
        coords+dist (map-indexed (fn [idx [x y]]
                                   (let [dx (-! x fx)]
                                     (if (zero? dx) [(-! y fy) idx [x y]] [dx idx [x y]]))) points)
        sorted-coords (sort-by first coords+dist)

        [_d hull] (reduce
                    (fn [[d hull] [dx idx [_x _y]]]
                      (if (> dx d)
                        [dx (conj hull idx)]
                        [d hull]))
                    [##-Inf []]
                    sorted-coords)]
    (assoc state :hull hull :triangles [] :half-edges [])))

(defn build-hull [{:keys [min-radius] :as state}]
  ;; check if there are collinear points
  (if (= min-radius ##Inf)
    (build-collinear-hull state)
    (build-non-collinear-hull state)))



(defn delaunator
  ([points] (delaunator points {}))
  ([points _opts] ;; opts like spitting out triangles?
   (let [n (count points)
         ;; arrays that will store the triangulation graph
         max-triangles (max (-! (*! 2 n) 5) 0)
         coords (into (vector-array (*! n 2)) cat points)
         triangles (vector-array (*! max-triangles 3))
         half-edges (vector-array (*! max-triangles 3))]
     (-> {:n          n :points points :coords coords
          :triangles  triangles
          :half-edges half-edges}
         (calculate-center+bbox)
         (pick-seed)
         (pick-point)
         (pick-location)
         (build-hull)))))

(comment
  (delaunator [[0 0] [0 1] [0 2] [1 2] [2 2] [2 1] [2 0] [1 0]]))
