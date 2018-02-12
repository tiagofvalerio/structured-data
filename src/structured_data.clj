(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow  xx xx)))

(defn spiff [v]
  (let [prim (get v 0)
        ter (get v 2)]
    (+ prim ter)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[v1 v2 v3]]
  (cond
    (and (not (= v1 nil)) (not (= v3 nil))) (+ v1 v3)
    :else nil))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right] )

(defn width
  [[[x1 y1][x2 y2]]]
  (- x2 x1))


(defn height
  [[[x1 y1][x2 y2]]]
  (- y2 y1))


(defn square? [rectangle]
  (if (= (width rectangle)(height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point?
  [rectangle point]
    (let [ [[x1 y1][x2 y2]] rectangle
      [z1 z2] point]
    (and(<= x1 z1 x2)(<= y1 z2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[x1 x2] inner]
    (and (contains-point? outer x1) (contains-point? outer x2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if(contains? author :death-year)
  false
  true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [v] (get v 1))]
  (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)))

 (defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if  (contains? (:authors book) author)
    true
    false))

(defn authors [books]
    (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map names books)))))

(defn author->string [author]
  (let [{name :name
        birth-year :birth-year
        death-year :death-year} author]
        (str name
          (if birth-year
            (str " ("birth-year " - " death-year ")" )))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (str (count books) " book" (if (> (count books) 1) "s") ". "
        (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
   (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
