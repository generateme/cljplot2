(ns cljplot2.code-review-r3a-f3-test
  "Code review round 3, batch A, cells / 3D / part-of-whole charts: all-invalid data errors, the shared kept-value
  predicate, padding and widening helpers, and outlines through `draw/outline!`."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.field :as field]
            [cljplot2.data.space3d :as space]
            [cljplot2.test-util :refer [error]]))

(def ^:private size {:width 300 :height 200})

(defn- build [p] (plot/build p size))

(def ^:private nan-xy [{:x ##NaN :y ##NaN :v 1.0 :c "a"} {:x nil :y nil :v 2.0 :c "b"}])

(deftest all-invalid-data-test
  (testing "every chart with mapped positions or weights throws {:key :mapping} for data without one valid value"
    (doseq [[label p dim] [["bin2d" {:data nan-xy :mapping {:x :x :y :y} :charts [(chart/bin2d)]} :x]
                           ["contour" {:data nan-xy :mapping {:x :x :y :y} :charts [(chart/contour)]} :x]
                           ["field" {:data nan-xy :mapping {:x :x :y :y} :charts [(chart/field)]} :x]
                           ["contour gridded" {:data [{:x 0 :y 0 :v ##NaN} {:x 1 :y 1 :v nil}] :mapping {:x :x :y :y :value :v}
                                               :charts [(chart/contour)]} :value]
                           ["correlation" {:data nan-xy :mapping {:x :x :y :y} :charts [(chart/correlation)]} :x]
                           ["tile" {:data nan-xy :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]} :x]
                           ["surface" {:data nan-xy :mapping {:x :x :y :y :z :v} :charts [(chart/surface)]} :x]
                           ["treemap" {:data [{:a "a" :v ##NaN} {:a "b" :v nil}] :mapping {:value :v :hierarchy [:a]}
                                       :charts [(chart/treemap)]} :value]
                           ["waffle" {:data [{:c "a" :v ##NaN} {:c "b" :v nil}] :mapping {:color :c :value :v}
                                      :charts [(chart/waffle)]} :value]]]
      (is (= {:key :mapping :in [:mapping dim] :rows 2} (select-keys (error #(build p)) [:key :in :rows])) label)))
  (testing "empty data still builds"
    (doseq [[c mapping] [[(chart/bin2d) {:x :x :y :y}] [(chart/contour) {:x :x :y :y}] [(chart/correlation) {:x :x :y :y}]
                         [(chart/tile) {:x :x :y :y :color :v}]]]
      (is (map? (build {:data [] :mapping mapping :charts [c]})) (str (:kind c)))))
  (testing "a function field reads no rows"
    (is (nil? (field/check-mapped! :function {} {:panel nan-xy})))))

(deftest tile-kept-values-test
  (testing "tile drops invalid positions and colours outside an explicit domain, NA colours kept"
    (let [rows [{:x 0 :y 0 :c "a"} {:x ##NaN :y 0 :c "a"} {:x 1 :y 0 :c "z"} {:x 2 :y 0 :c nil}]
          b (build {:data rows :mapping {:x :x :y :y :color :c} :scales {:color {:domain ["a"]}} :charts [(chart/tile)]})]
      (is (= {:x 1 :y 0 :color 1 :duplicate 0} (get-in b [:stats 0 :dropped])))
      (is (= 2 (count (get-in b [:stats 0 :data :panel :cells])))))))

(deftest widened-domain-test
  (testing "a single 3D axis value widens by half a unit each side before the nice domain"
    (is (= [4.4 5.6] (space/data-domain [5 5] 0.0)))))

(deftest outlined-render-test
  (testing "charts outlining through draw/outline! render with visible and invisible strokes"
    (doseq [p [{:data [{:a "a" :v 2} {:a "b" :v 3}] :mapping {:value :v :hierarchy [:a]} :charts [(chart/pack {:stroke-width 0})]}
               {:data [{:c "a" :v 2} {:c "b" :v 3}] :mapping {:color :c :value :v} :charts [(chart/waffle {:stroke-color :none})]}
               {:data [{:x 0 :y 0 :c 1} {:x 1 :y 1 :c 2}] :mapping {:x :x :y :y :color :c} :charts [(chart/tile {:stroke-width 2})]}]]
      (is (some? (plot/render-image p size))))))
