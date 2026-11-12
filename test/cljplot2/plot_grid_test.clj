(ns cljplot2.plot-grid-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [cljplot2.core :as plot]
            [cljplot2.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- layout [plot] (:layout (plot/build plot {:width 600 :height 400})))
(defn- track-ids [g axis] (mapv :id (get g axis)))
(defn- track-size [l axis id] (:size (first (filter #(= id (:id %)) (get l axis)))))

(defn- pixel [canvas x y]
  (let [rgb (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right rgb 16) 0xff) (bit-and (bit-shift-right rgb 8) 0xff) (bit-and rgb 0xff)]))

(defn- rgb [color] (let [v (c/to-color color)] [(long (c/red v)) (long (c/green v)) (long (c/blue v))]))

(def ^:private all-labels {:title "Title" :x "x title" :y "y title"})

;; 1.x plot -> grid

(deftest generated-grid-test
  (let [g (:grid (plot/build {}))]
    (is (= [:margin-left :y-title :y-axis :panel :x-overhang :margin-right] (track-ids g :columns)))
    (is (= [:margin-top :title :y-overhang :panel :x-axis :x-title :margin-bottom] (track-ids g :rows))))
  (testing "the :grid stage produces the grid"
    (is (not (contains? (plot/build {} {:until :scale-specs}) :grid)))
    (is (contains? (plot/build {} {:until :grid}) :grid))))

(deftest custom-grid-test
  (let [custom {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}] :items [{:id :only :column 0 :row 0}]}
        built (plot/build {:custom-grid custom})]
    (is (= custom (:grid built)) "custom grid replaces generation")
    (is (= custom (:custom-grid (plot/build built))) "kept across rebuilds")
    (testing "labels can't be combined with a custom grid"
      (is (= {:key :labels} (error-data #(plot/build {:custom-grid custom :labels all-labels})))))))

(deftest reserved-axis-tracks-test
  (let [l (layout {:labels all-labels})]
    (is (= 0 (track-size l :columns :y-axis)))
    (is (= 0 (track-size l :rows :x-axis)))))

(deftest empty-plot-test
  (testing "no labels: panel fills the area inside the margins"
    (is (= {:x 10 :y 10 :w 580 :h 380} (grid/item-box (layout {}) :panel))))
  (testing "nil labels add no items"
    (is (= (grid/item-box (layout {}) :panel)
           (grid/item-box (layout {:labels {:title nil :x nil :y nil}}) :panel)))))

;; 2.x labels

(deftest labels-test
  (testing "string and options map lay out identically"
    (is (= (layout {:labels {:title "abc"}}) (layout {:labels {:title {:text "abc"}}}))))
  (testing "title shrinks the panel by its measured height"
    (let [with-title (layout {:labels {:title "abc"}})
          title-h (track-size with-title :rows :title)]
      (is (pos? title-h))
      (is (= (- 380 title-h) (:h (grid/item-box with-title :panel))))))
  (testing "axis titles"
    (let [l (layout {:labels {:x "x" :y "y"}})]
      (is (pos? (track-size l :rows :x-title)))
      (is (pos? (track-size l :columns :y-title)))))
  (testing "y title rotated 270 by default, explicit rotation wins"
    (let [w-default (track-size (layout {:labels {:y "Percent of Total"}}) :columns :y-title)
          w-flat (track-size (layout {:labels {:y {:text "Percent of Total" :rotation 0}}}) :columns :y-title)]
      (is (< w-default w-flat))))
  (testing "item ids and fixed item order, independent of the labels map's key order"
    (let [ids (fn [labels] (mapv :id (:items (:grid (plot/build {:labels labels})))))]
      (is (= [:panel :title :x-title :y-title] (ids (array-map :y "y" :x "x" :title "t"))))
      (is (= (layout {:labels (array-map :y "y" :x "x" :title "t")})
             (layout {:labels (array-map :title "t" :x "x" :y "y")})))))
  (testing "label :kind can't be overridden"
    (is (= :label (get-in (plot/build {:labels {:title {:text "t" :kind :panel}}}) [:grid :items 1 :content :kind]))))
  (testing "1 layout round"
    (is (= 1 (:rounds (layout {:labels all-labels})))))
  (testing "invalid labels"
    (is (= {:key :labels :value :xlab :allowed #{:title :x :y}}
           (error-data #(plot/build {:labels {:xlab "x"}}))))
    (is (= :labels (:key (error-data #(plot/build {:labels "title"})))))))

;; 3.x panel, 4.x theme

(deftest panel-drawing-test
  (let [box (grid/item-box (layout {}) :panel)
        {:keys [x y w h]} box
        cx (+ x (quot w 2)) cy (+ y (quot h 2))]
    (testing "default: no fill, no border - plot background shows"
      (let [c (plot/render {} {:width 600 :height 400})]
        (is (= (rgb 0xe8e8f0) (pixel c cx cy) (pixel c x y)))))
    (testing "fill"
      (is (= (rgb :red) (pixel (plot/render {:theme {:panel {:fill :red}}} {:width 600 :height 400}) cx cy))))
    (testing "border inside the box"
      (let [c (plot/render {:theme {:panel {:border-color :blue :border-width 3}}} {:width 600 :height 400})]
        (is (= (rgb :blue) (pixel c x y) (pixel c (+ x 2) cy) (pixel c (+ x w -3) cy) (pixel c cx (+ y h -1))))
        (is (= (rgb 0xe8e8f0) (pixel c (+ x 3) cy) (pixel c (dec x) cy) (pixel c cx (+ y h))))))
    (testing "border wider than half the box fills it"
      (let [c (plot/render {:theme {:panel {:border-color :blue :border-width 1000}}} {:width 600 :height 400})]
        (is (= (rgb :blue) (pixel c cx cy)))))
    (testing "border width rounded to whole pixels"
      (let [c (plot/render {:theme {:panel {:border-color :blue :border-width 1.6}}} {:width 600 :height 400})]
        (is (= (rgb :blue) (pixel c (inc x) cy)))
        (is (= (rgb 0xe8e8f0) (pixel c (+ x 2) cy)))))
    (testing ":none draws nothing, nil in an override keeps the default"
      (let [c (plot/render {:theme {:panel {:fill :none :border-color nil}}} {:width 600 :height 400})]
        (is (= (rgb 0xe8e8f0) (pixel c cx cy) (pixel c x y)))))
    (testing "invalid border width fails at build time"
      (is (= :border-width (:key (error-data #(plot/build {:theme {:panel {:border-color :blue :border-width -1}}}))))))))

(deftest plot-theme-test
  (testing "background fills the canvas"
    (is (= (rgb :yellow) (pixel (plot/render {:theme {:plot {:background :yellow}}} {:width 50 :height 50}) 0 0))))
  (testing "margin sizes the margin tracks"
    (let [l (layout {:theme {:plot {:margin 25}}})]
      (is (= [25 25] [(track-size l :columns :margin-left) (track-size l :rows :margin-bottom)]))
      (is (= {:x 25 :y 25 :w 550 :h 350} (grid/item-box l :panel)))))
  (testing ":none background leaves the canvas transparent"
    (let [argb (.getRGB ^BufferedImage (c2d/get-image (plot/render {:theme {:plot {:background :none}}} {:width 50 :height 50})) 0 0)]
      (is (zero? (bit-and (bit-shift-right argb 24) 0xff)))))
  (testing "theme validation at build time"
    (are [t k] (= k (:key (error-data #(plot/build {:theme t}))))
      {:text {:title {:size 0}}} :size
      {:text {:strip {:padding -2}}} :padding
      {:text {:legend-label {:align :left}}} :align
      {:text {:tick-label {:font "Liberaton Mono"}}} :font
      {:text {:extra 12}} :role))
  (testing "invalid margin"
    (are [m] (= {:key :margin :value m} (error-data #(plot/build {:theme {:plot {:margin m}}})))
      -1 "10")))

(defn- title-ink-center-x
  "Horizontal center of non-background pixels in the title row."
  [plot]
  (let [built (plot/build plot {:width 600 :height 400})
        {:keys [x y w h]} (grid/item-box (:layout built) :title)
        c (plot/render plot {:width 600 :height 400})
        bg (rgb 0xe8e8f0)
        xs (for [px (range x (+ x w)) py (range y (+ y h)) :when (not= bg (pixel c px py))] px)]
    (/ (+ (apply min xs) (apply max xs)) 2.0)))

(deftest title-align-test
  (testing "centered by default"
    (is (< 250 (title-ink-center-x {:labels {:title "abc"}}) 350)))
  (testing "theme alignment"
    (is (< (title-ink-center-x {:labels {:title "abc"} :theme {:text {:title {:align [:left :middle]}}}}) 200)))
  (testing "inline alignment wins over the theme"
    (is (> (title-ink-center-x {:labels {:title {:text "abc" :align [:right :middle]}}
                                :theme {:text {:title {:align [:left :middle]}}}})
           400)))
  (testing "invalid theme alignment"
    (is (= :align (:key (error-data #(plot/build {:labels {:title "abc"} :theme {:text {:title {:align :left}}}})))))))
