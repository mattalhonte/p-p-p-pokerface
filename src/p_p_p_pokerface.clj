(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        spc-vals {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (contains? spc-vals fst)
      (spc-vals fst)
      (Integer/valueOf (str fst)))))

(defn suit [card]
  (let [[_ snd] card]
    snd))

(defn pair? [hand]
  (>
   (count
    (filter
     (fn [x] (== x 2))
     (vals (frequencies (map rank hand)))))
   0))

(defn three-of-a-kind? [hand]
  (>
   (count
    (filter
     (fn [x] (== x 3))
     (vals (frequencies (map rank hand)))))
   0))

(defn four-of-a-kind? [hand]
  (>
   (count
    (filter
     (fn [x] (== x 4))
     (vals (frequencies (map rank hand)))))
   0))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
     (>
   (count
    (filter
     (fn [x] (== x 2))
     (vals (frequencies (map rank hand)))))
   1)
     (four-of-a-kind? hand)))

(defn straight? [hand]
 (let  
          [sorted-rank  (sort (map rank hand))
           lowest-card (first (sort (map rank hand)))
           straight-final (+ lowest-card 5)
           my-straight    (range lowest-card straight-final)
           altern-ace (sort (replace {14 1} sorted-rank))]
   (or
        (= my-straight sorted-rank)
        (= altern-ace (range 1 6)))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
