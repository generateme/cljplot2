(ns cljplot2.cache-test
  "Bounded LRU caches (`cljplot2.cache`) and their users: SVG images, shape outlines."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.cache :as cache]
            [cljplot2.graphics.shape :as shape]))

(deftest lru-cache-test
  (testing "a value is computed once per key; nil values are cached"
    (let [c (cache/lru-cache 4)
          calls (atom 0)
          f (fn [k] (swap! calls inc) (when (pos? k) (* 10 k)))]
      (is (= 10 (cache/lookup-or-compute c 1 f)))
      (is (= 10 (cache/lookup-or-compute c 1 f)))
      (is (nil? (cache/lookup-or-compute c 0 f)))
      (is (nil? (cache/lookup-or-compute c 0 f)))
      (is (= 2 @calls))
      (is (= 2 (cache/entry-count c)))))
  (testing "boundary: threshold 1 keeps the last key; the least recently used goes first"
    (let [c (cache/lru-cache 1)]
      (cache/lookup-or-compute c :a name)
      (cache/lookup-or-compute c :b name)
      (is (= 1 (cache/entry-count c))))
    (let [c (cache/lru-cache 2)
          calls (atom [])
          f (fn [k] (swap! calls conj k) k)]
      (cache/lookup-or-compute c :a f)
      (cache/lookup-or-compute c :b f)
      (cache/lookup-or-compute c :a f)
      (cache/lookup-or-compute c :c f)
      (cache/lookup-or-compute c :a f)
      (cache/lookup-or-compute c :b f)
      (is (= [:a :b :c :b] @calls) ":b evicted (least recently used), :a kept")))
  (testing "boundary: a throwing computation caches nothing and propagates"
    (let [c (cache/lru-cache 2)]
      (is (thrown? ArithmeticException (cache/lookup-or-compute c 1 (fn [_] (/ 1 0)))))
      (is (zero? (cache/entry-count c)))
      (is (= 1 (cache/lookup-or-compute c 1 identity)))))
  (testing "boundary: threads missing one key together compute it once"
    (let [c (cache/lru-cache 2)
          calls (atom 0)
          f (fn [k] (swap! calls inc) (Thread/sleep 50) k)
          results (mapv deref (mapv (fn [_] (future (cache/lookup-or-compute c :k f))) (range 8)))]
      (is (= (repeat 8 :k) results))
      (is (= 1 @calls))))
  (testing "clear! empties and keeps the threshold"
    (let [c (cache/lru-cache 2)]
      (doseq [k (range 5)] (cache/lookup-or-compute c k identity))
      (cache/clear! c)
      (is (zero? (cache/entry-count c)))
      (doseq [k (range 5)] (cache/lookup-or-compute c k identity))
      (is (= 2 (cache/entry-count c))))))

(deftest shape-caches-test
  (testing "outlines are cached per shape map and bounded"
    (let [outlines @#'shape/outlines]
      (cache/clear! outlines)
      (doseq [i (range 300)] (is (nil? (shape/shape-problem {:path (str "M0 0 L" (inc i) " 0 L0 1 Z")}))))
      (is (= 256 (cache/entry-count outlines)))))
  (testing "SVG images: a failing source caches nothing"
    (let [svgs @#'shape/svg-images]
      (cache/clear! svgs)
      (with-redefs [clojure2d.core/transcode-svg (fn [_ _ _] (throw (Exception. "broken")))
                    clojure2d.core/load-svg identity]
        (is (thrown? clojure.lang.ExceptionInfo (shape/svg-image "notebooks/data/none.svg" 10))))
      (is (zero? (cache/entry-count svgs))))))
