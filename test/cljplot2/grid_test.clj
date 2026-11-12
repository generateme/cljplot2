(ns cljplot2.grid-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [fastmath.vector :as v]
            [cljplot2.grid :as grid]
            [cljplot2.measure :as measure]))

(defn- placeholder [w h] {:kind :placeholder :size [w h]})

(defn- layout
  ([g] (layout g {:x 0 :y 0 :w 100 :h 100}))
  ([g box] (let [p (grid/prepare g)]
             (grid/solve p box (grid/measure-items p {} nil)))))

(defn- sizes [l axis] (mapv :size (get l axis)))
(defn- offsets [l axis] (mapv :offset (get l axis)))

(defn- one-row [columns & items]
  {:columns columns :rows [{:size [:flexible]}] :items (vec items)})

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; 3.1 validation

(deftest validation-test
  (testing "malformed grid names the offending key"
    (are [g k] (= k (:key (error-data #(grid/prepare g))))
      nil :grid
      {:rows [{:size [:flexible]}]} :columns
      {:columns [] :rows [{:size [:flexible]}]} :columns
      {:columns '({:size [:flexible]}) :rows [{:size [:flexible]}]} :columns
      {:columns [{:size [:flexible]}]} :rows
      {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}] :items '()} :items
      {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}] :items [{:column 0 :row 0}]} :items
      {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}] :items [{:id :a :column 0 :row 0}
                                                                         {:id :a :column 0 :row 0}]} :items))
  (testing "invalid track sizes"
    (are [size] (= :columns (:key (error-data #(grid/prepare (one-row [{:size size}])))))
      nil [:unknown 1] [:fixed -1] [:fixed "1"] [:fraction -0.1] [:fraction 1.01]
      [:flexible 0] [:flexible -1] {:fixed 1}))
  (testing "boundary track sizes are valid"
    (are [size] (grid/prepare (one-row [{:size size}]))
      [:fixed 0] [:fraction 0] [:fraction 1] [:flexible] [:flexible 0.001] [:measured]))
  (testing "track ids"
    (is (= 0 (:id (error-data #(grid/prepare (one-row [{:id 0 :size [:flexible]}]))))))
    (is (= [:a] (:duplicates (error-data #(grid/prepare (one-row [{:id :a :size [:flexible]} {:id :b :size [:flexible]} {:id :a :size [:flexible]}])))))))
  (testing "item id duplicates are reported, at most 10"
    (let [items (vec (for [i (range 30)] {:id (quot i 2) :column 0 :row 0}))
          data (error-data #(grid/prepare (apply one-row [{:size [:flexible]}] items)))]
      (is (= :items (:key data)))
      (is (= 10 (count (:duplicates data))))))
  (testing "spans"
    (let [cols [{:id :a :size [:flexible]} {:id :b :size [:flexible]}]]
      (are [span] (= :column (:key (error-data #(grid/prepare (one-row cols {:id :i :column span :row 0})))))
        nil -1 2 :missing {:from :b :to :a} {:from :a} {:to :b} {:from 0 :to 2})
      (is (= :row (:key (error-data #(grid/prepare (one-row cols {:id :i :column 0}))))))
      (is (= 2 (:count (error-data #(grid/prepare (one-row cols {:id :i :column 2 :row 0})))))))))

;; 3.2 track kinds

(deftest fixed-test
  (testing "fixed tracks only: remaining space after the last track"
    (let [l (layout (one-row [{:size [:fixed 10]} {:size [:fixed 0]} {:size [:fixed 25.5]}]))]
      (is (= [10 0 26] (sizes l :columns)))
      (is (= [0 10 10] (offsets l :columns)))))
  (testing "fixed tracks filling the box exactly"
    (is (= [40 60] (sizes (layout (one-row [{:size [:fixed 40]} {:size [:fixed 60]}])) :columns)))))

(deftest fraction-test
  (is (= [25 75] (sizes (layout (one-row [{:size [:fraction 0.25]} {:size [:fraction 0.75]}])) :columns)))
  (testing "fractions summing to 1 with floating error don't overflow"
    (is (= 100 (reduce + (sizes (layout (one-row (vec (repeat 10 {:size [:fraction 0.1]})))) :columns)))))
  (testing "fraction of the grid's own box, not of the plot"
    (is (= [50] (sizes (layout (one-row [{:size [:fraction 0.5]}]) {:x 0 :y 0 :w 100 :h 10}) :columns)))
    (is (= [20] (sizes (layout (one-row [{:size [:fraction 0.5]}]) {:x 0 :y 0 :w 40 :h 10}) :columns)))))

(deftest measured-test
  (let [g (one-row [{:size [:measured]} {:size [:measured]} {:size [:flexible]}]
                   {:id :a :column 0 :row 0 :content (placeholder 12 5)}
                   {:id :b :column 0 :row 0 :content (placeholder 30 5)}
                   {:id :c :column 2 :row 0 :content (placeholder 999 5)})]
    (testing "largest natural size; empty measured track is 0"
      (is (= [30 0 70] (sizes (layout g) :columns)))))
  (testing "nil content measures 0"
    (is (= [0 100] (sizes (layout (one-row [{:size [:measured]} {:size [:flexible]}] {:id :a :column 0 :row 0})) :columns))))
  (testing "invalid placeholder"
    (are [size] (= size (:size (error-data #(layout (one-row [{:size [:measured]}] {:id :a :column 0 :row 0 :content {:kind :placeholder :size size}})))))
      nil [1] ["1" 1] '(1 1))
    (is (= {:key :content :item :a :kind :placeholder}
           (dissoc (error-data #(layout (one-row [{:size [:measured]}] {:id :a :column 0 :row 0 :content (placeholder -1 1)}))) :size))))
  (testing "content must have a known kind; only nil content is empty"
    (are [content] (contains? (error-data #(layout (one-row [{:size [:measured]}] {:id :a :column 0 :row 0 :content content}))) :kind)
      {:kind :unknown} {:knd :placeholder} {} "text")))

(defmethod measure/natural-size ::fixed-result [{:keys [result]} _] result)

(deftest natural-size-postcondition-test
  (testing "natural size must be a Vec2 with finite, non-negative coordinates"
    (are [result] (= {:key :content :item :a :kind ::fixed-result}
                     (dissoc (error-data #(layout (one-row [{:size [:measured]}] {:id :a :column 0 :row 0
                                                                                   :content {:kind ::fixed-result :result result}})))
                             :size))
      nil [1.0 1.0] (v/vec2 ##NaN 1.0) (v/vec2 1.0 ##Inf) (v/vec2 ##-Inf 1.0) (v/vec2 -0.5 1.0) (v/vec2 1.0 -1.0)))
  (testing "zero is a valid natural size"
    (is (= [0 100] (sizes (layout (one-row [{:size [:measured]} {:size [:flexible]}]
                                           {:id :a :column 0 :row 0 :content {:kind ::fixed-result :result (v/vec2 0.0 0.0)}}))
                          :columns)))))

(deftest flexible-test
  (testing "weights"
    (is (= [25 75] (sizes (layout (one-row [{:size [:flexible 1]} {:size [:flexible 3]}])) :columns))))
  (testing "single track takes the whole box"
    (is (= [100] (sizes (layout (one-row [{:size [:flexible]}])) :columns))))
  (testing "equal weights not dividing the length differ by at most 1 px"
    (let [s (sizes (layout (one-row (vec (repeat 7 {:size [:flexible]})))) :columns)]
      (is (= 100 (reduce + s)))
      (is (<= (- (apply max s) (apply min s)) 1))))
  (testing "zero-length box"
    (is (= [0 0] (sizes (layout (one-row [{:size [:flexible]} {:size [:flexible]}]) {:x 0 :y 0 :w 0 :h 0}) :columns)))))

(deftest mixed-test
  (let [l (layout (one-row [{:size [:fixed 10]} {:size [:fraction 0.2]} {:size [:measured]} {:size [:flexible]} {:size [:flexible 2]}]
                           {:id :m :column 2 :row 0 :content (placeholder 15 1)})
                  {:x 0 :y 0 :w 200 :h 10})]
    (is (= [10 40 15 45 90] (sizes l :columns)))
    (is (= [0 10 50 65 110] (offsets l :columns)))))

;; 3.3, 3.4 placement and spanning

(deftest placement-test
  (let [cols [{:id :a :size [:fixed 10]} {:id :gap :size [:fixed 5]} {:id :b :size [:flexible]}]
        by-id (layout (one-row cols {:id :i :column {:from :a :to :b} :row 0}))
        by-index (layout (one-row cols {:id :i :column {:from 0 :to 2} :row 0}))]
    (testing "id and index placement give identical boxes"
      (is (= (grid/item-box by-id :i) (grid/item-box by-index :i))))
    (testing "spanning box covers first start to last end, gap included"
      (is (= {:x 0 :y 0 :w 100 :h 100} (grid/item-box by-id :i))))
    (testing "single-track span as map"
      (is (= {:x 10 :y 0 :w 5 :h 100} (grid/item-box (layout (one-row cols {:id :i :column {:from :gap :to :gap} :row 0})) :i))))
    (testing "single-track span as map on rows"
      (is (= {:x 0 :y 0 :w 10 :h 100} (grid/item-box (layout (one-row cols {:id :i :column :a :row {:from 0 :to 0}})) :i))))
    (testing "vector ids are ids, not spans"
      (is (= {:x 0 :y 0 :w 100 :h 100}
             (grid/item-box (layout (one-row [{:id [:p 0] :size [:flexible]}] {:id :i :column [:p 0] :row 0})) :i))))
    (testing "unknown item id"
      (is (nil? (grid/item-box by-id :nope))))))

;; 4.4 overflow

(deftest overflow-test
  (testing "exactly fitting doesn't throw"
    (is (layout (one-row [{:size [:fixed 60]} {:size [:fraction 0.4]}]))))
  (testing "one pixel too much"
    (is (= {:axis :columns :available 100 :required 101.0}
           (error-data #(layout (one-row [{:size [:fixed 61]} {:size [:fraction 0.4]}]))))))
  (testing "measured content overflowing on rows"
    (is (= :rows (:axis (error-data #(layout {:columns [{:size [:flexible]}] :rows [{:size [:measured]}]
                                              :items [{:id :a :column 0 :row 0 :content (placeholder 1 101)}]})))))))

;; 4.5 measured contributions

(deftest measured-spanning-test
  (let [g {:columns [{:size [:measured]} {:size [:measured]} {:size [:measured]} {:size [:flexible]}]
           :rows [{:id :title :size [:measured]} {:size [:flexible]}]
           :items [{:id :title :column {:from 0 :to 2} :row :title :content (placeholder 90 24)}
                   {:id :a :column 0 :row 1 :content (placeholder 10 5)}]}
        l (layout g)]
    (testing "spanning title sets its row height"
      (is (= [24 76] (sizes l :rows))))
    (testing "and leaves column widths unchanged"
      (is (= [10 0 0 90] (sizes l :columns))))))

;; solve preconditions

(deftest solve-preconditions-test
  (let [p (grid/prepare (one-row [{:size [:flexible]}] {:id :a :column 0 :row 0} {:id :b :column 0 :row 0}))
        measurements (grid/measure-items p {} nil)
        box {:x 0 :y 0 :w 10 :h 10}]
    (testing "measurements must align with items"
      (are [ms n] (= {:key :measurements :items 2 :measurements n} (error-data #(grid/solve p box ms)))
        nil 0
        [] 0
        (subvec measurements 0 1) 1
        (conj measurements (first measurements)) 3
        {:a 1} 1))
    (testing "empty grid accepts nil measurements"
      (is (= [] (:items (grid/solve (grid/prepare (one-row [{:size [:flexible]}])) box nil)))))
    (testing "box must have integer coordinates and non-negative size"
      (are [b] (= :box (:key (error-data #(grid/solve p b measurements))))
        nil {} {:x 0 :y 0 :w 10} {:x 0 :y 0 :w 10.5 :h 10} {:x 0 :y 0 :w -1 :h 10} {:x "0" :y 0 :w 1 :h 1}))
    (testing "zero-size box is valid"
      (is (= {:x 3 :y 4 :w 0 :h 0} (grid/item-box (grid/solve p {:x 3 :y 4 :w 0 :h 0} measurements) :a))))))

;; 4.6 coordinates

(deftest coordinates-test
  (let [l (layout {:columns [{:size [:fixed 10]} {:size [:flexible]}]
                   :rows [{:size [:fixed 7]} {:size [:flexible]}]
                   :items [{:id :first :column 0 :row 0} {:id :last :column 1 :row 1}]}
                  {:x 10 :y 20 :w 50 :h 40})]
    (is (= {:x 10 :y 20 :w 10 :h 7} (grid/item-box l :first)))
    (is (= {:x 20 :y 27 :w 40 :h 33} (grid/item-box l :last)))))

;; 3.5 nesting

(deftest nested-test
  (let [child {:kind :grid
               :columns [{:size [:flexible]}]
               :rows [{:size [:flexible]} {:size [:flexible]}]
               :items [{:id :top :column 0 :row 0} {:id :bottom :column 0 :row 1}]}
        l (layout {:columns [{:size [:flexible]} {:size [:flexible]}]
                   :rows [{:size [:flexible]}]
                   :items [{:id :cell :column 0 :row 0 :content child}]}
                  {:x 0 :y 0 :w 101 :h 51})
        cell (grid/item-box l :cell)
        nested (:layout (first (:items l)))
        top (grid/item-box nested :top)
        bottom (grid/item-box nested :bottom)]
    (testing "child boxes tile the parent cell exactly"
      (is (= (:box nested) cell))
      (is (= (:x top) (:x bottom) (:x cell)))
      (is (= (:w top) (:w bottom) (:w cell)))
      (is (= (:y top) (:y cell)))
      (is (= (+ (:y top) (:h top)) (:y bottom)))
      (is (= (+ (:y bottom) (:h bottom)) (+ (:y cell) (:h cell))))))
  (testing "nested grid's natural size (fixed + measured, grown by fractions) drives a measured parent track"
    (let [child {:kind :grid
                 :columns [{:size [:fixed 30]} {:size [:measured]} {:size [:flexible]} {:size [:fraction 0.5]}]
                 :rows [{:size [:fixed 10]}]
                 :items [{:id :m :column 1 :row 0 :content (placeholder 12 3)}]}
          l (layout (one-row [{:size [:measured]} {:size [:flexible]}] {:id :cell :column 0 :row 0 :content child}))]
      (is (= [84 16] (sizes l :columns)))
      (is (= [30 12 0 42] (sizes (:layout (first (:items l))) :columns)))))
  (testing "nested grid with fractions summing to 1 can't fit a measured track"
    (is (= :columns (:axis (error-data #(layout (one-row [{:size [:measured]}]
                                                         {:id :cell :column 0 :row 0
                                                          :content {:kind :grid :columns [{:size [:fixed 1]} {:size [:fraction 1]}]
                                                                    :rows [{:size [:flexible]}]}})))))))
  (testing "nested overflow is reported"
    (is (= :columns (:axis (error-data #(layout (one-row [{:size [:fixed 10]}]
                                                         {:id :cell :column 0 :row 0
                                                          :content {:kind :grid :columns [{:size [:fixed 11]}] :rows [{:size [:flexible]}]}}))))))))

;; 4.3 generative

(def ^:private track-gen
  (gen/one-of [(gen/fmap (fn [px] {:size [:fixed px]}) (gen/choose 0 20))
               (gen/fmap (fn [n] {:size [:fraction (/ n 100.0)]}) (gen/choose 0 5))
               (gen/fmap (fn [w] {:size [:flexible w]}) (gen/choose 1 5))]))

(defspec cumulative-edges-tile-the-box 300
  (prop/for-all [tracks (gen/not-empty (gen/vector track-gen 1 12))
                 start (gen/choose -50 50)
                 extra (gen/choose 0 500)]
    (let [tracks (conj tracks {:size [:flexible 1]})
          fixed (reduce + (keep (fn [{[k v] :size}] (when (= k :fixed) v)) tracks))
          length (* 3 (+ fixed 60 extra))
          l (layout (one-row tracks) {:x start :y 0 :w length :h 10})
          cols (:columns l)
          equal-flex (->> (map vector tracks cols)
                          (filter (fn [[{[k w] :size}]] (and (= k :flexible) (= w 1))))
                          (map (comp :size second)))]
      (and (= start (:offset (first cols)))
           (= (+ start length) (+ (:offset (peek cols)) (:size (peek cols))))
           (every? true? (map (fn [a b] (= (+ (:offset a) (:size a)) (:offset b))) cols (rest cols)))
           (every? #(>= % 0) (map :size cols))
           (<= (- (apply max equal-flex) (apply min equal-flex)) 1)))))
