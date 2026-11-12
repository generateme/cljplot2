(ns cljplot2.canvas-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [cljplot2.canvas :as canvas]
            [cljplot2.core :as plot]))

(deftest call-with-graphics-test
  (let [flush-graphics c2d/flush-graphics
        released (atom [])
        received (atom nil)
        released? (fn [] (some #(identical? @received %) @released))]
    (with-redefs [c2d/flush-graphics (fn [c] (swap! released conj c) (flush-graphics c))]
      (testing "graphics released when f throws"
        (is (thrown? clojure.lang.ExceptionInfo
                     (canvas/call-with-graphics (c2d/canvas 2 2) (fn [c] (reset! received c) (throw (ex-info "boom" {}))))))
        (is (released?)))
      (testing "graphics released when f returns, result passed through"
        (is (= 42 (canvas/call-with-graphics (c2d/canvas 2 2) (fn [c] (reset! received c) 42))))
        (is (released?)))
      (testing "render releases the target canvas"
        (reset! released [])
        (let [result (plot/render {} {:width 5 :height 5})]
          (is (some #(identical? result %) @released)))))))
