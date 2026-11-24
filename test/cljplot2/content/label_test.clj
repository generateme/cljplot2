(ns cljplot2.content.label-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error-data error error-ins]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.core :as plot]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.font :as font]
            [cljplot2.layout.grid :as grid]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme]
            [cljplot2.examples :as examples])
  (:import [java.awt.image BufferedImage]))

(defn- label [text & {:as options}] (merge {:kind :label :role :axis-title :text text} options))

(defn- size
  "Natural size [w h] of `content` measured with `hint` and the default theme."
  ([content] (size content :high))
  ([content hint]
   (measure/call-with-scratch-canvas
    hint (fn [c] (let [s (measure/natural-size content {:canvas c :theme theme/default-theme})] [(.x s) (.y s)])))))

(def ^:private white-background
  "Ink tests treat white as empty."
  {:plot {:background :white}})

(defn- prepare-error
  "ex-data of preparing a one-cell grid holding `content`."
  [content]
  (error #(grid/prepare {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}]
                              :items [{:id :l :column 0 :row 0 :content content}]})))

(defn- oracle-size
  "Independent measurement: text block of `lines` in `style`, rounded up, plus padding."
  [lines style hint]
  (c2d/with-canvas [c (c2d/canvas 1 1 hint)]
    (c2d/set-font c (font/resolve-font (:font style) (:font-style style) (:size style)))
    (let [boxes (map #(c2d/text-bounding-box c %) lines)
          p (* 2.0 (:inset style))]
      [(+ p (m/ceil (apply max (map #(nth % 2) boxes))))
       (+ p (m/ceil (* (count lines) (nth (first boxes) 3))))])))

(def ^:private axis-style (get-in theme/default-theme [:text :axis-title]))

;; single-label rendering and ink

(defn- render-label
  [content w h & {:as options}]
  (plot/render {:theme white-background :custom-grid {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}]
                       :items [{:id :l :column 0 :row 0 :content content}]}}
               (merge {:width w :height h} options)))

(defn- ink-box
  "Bounding box [x0 y0 x1 y1] of non-white pixels of `canvas` with x in `[from-x to-x)`
  (optionally only those matching `pred` on [r g b]), or nil."
  ([canvas] (ink-box canvas (constantly true)))
  ([canvas pred] (ink-box canvas pred 0 (.getWidth ^BufferedImage (c2d/get-image canvas))))
  ([canvas pred from-x to-x]
   (let [^BufferedImage img (c2d/get-image canvas)
         pts (for [x (range from-x to-x) y (range (.getHeight img))
                   :let [rgb (.getRGB img x y)
                         r (bit-and (bit-shift-right rgb 16) 0xff)
                         g (bit-and (bit-shift-right rgb 8) 0xff)
                         b (bit-and rgb 0xff)]
                   :when (and (not= [r g b] [255 255 255]) (pred [r g b]))]
               [x y])]
     (when (seq pts)
       [(apply min (map first pts)) (apply min (map second pts))
        (apply max (map first pts)) (apply max (map second pts))]))))

;; 1.x label content

(deftest empty-text-test
  (are [text] (= [0.0 0.0] (size (label text)))
    nil ""))

(deftest text-conversion-test
  (is (= (size (label "12")) (size (label 12)))))

(deftest multi-line-test
  (let [[w2 h2] (size (label "ab\nabcd"))
        [w1 h1] (size (label "abcd"))
        p2 (* 2.0 (:inset axis-style))]
    (testing "width of the widest line, height of two lines"
      (is (= w1 w2))
      (is (< (- (* 2.0 (- h1 p2)) 1.0) (- h2 p2) (+ (* 2.0 (- h1 p2)) 1.0))))
    (testing "CRLF line breaks"
      (is (= (size (label "ab\nabcd")) (size (label "ab\r\nabcd")))))
    (testing "trailing and inner empty lines count"
      (is (= h2 (second (size (label "a\n")))))
      (is (= (second (size (label "a\n\nb"))) (second (size (label "a\nb\nc"))))))
    (testing "oracle"
      (is (= (oracle-size ["ab" "abcd"] axis-style :high) (size (label "ab\nabcd")))))))

(deftest rotation-test
  (let [[w h] (size (label "Percent of Total"))]
    (is (= [h w] (size (label "Percent of Total" :rotation 90))))
    (is (= [h w] (size (label "Percent of Total" :rotation 270))))
    (are [r] (= {:key :rotation :in [:content :rotation] :value r} (prepare-error (label "a" :rotation r)))
      45 180 -90 "90" 90.0)
    (is (nil? (prepare-error (label "a" :rotation 270))))))

(deftest align-validation-test
  (are [a] (= :align (:key (prepare-error (label "a" :align a))))
    [:center] [:middle :center] '(:left :top) :left [:left :top :x])
  (testing "nested grids validate their content too"
    (is (= :align (:key (prepare-error {:kind :grid :columns [{:size [:flexible]}] :rows [{:size [:flexible]}]
                                        :items [{:id :n :column 0 :row 0 :content (label "a" :align :left)}]}))))))

(deftest padding-test
  (let [[w0 h0] (size (label "abc" :inset 0))]
    (is (= [(+ w0 10.0) (+ h0 10.0)] (size (label "abc" :inset 5))))
    (is (= :padding (:key (prepare-error (label "abc" :padding -1)))))))

(deftest style-override-test
  (is (> (second (size (label "abc" :role :title :size 20))) (second (size (label "abc" :role :title))))))

(deftest role-test
  (are [role] (= {:key :role :value role} (error #(size (label "a" :role role))))
    nil :unknown)
  (is (= {:key :role :value :broken}
         (error #(measure/call-with-scratch-canvas
                       :high (fn [c] (measure/natural-size (label "a" :role :broken)
                                                           {:canvas c :theme (theme/plot-theme {:text {:broken 12}})})))))))

;; 2.x fonts

(deftest bundled-files-test
  (is (= #{"DejaVu Sans Mono" "Noto Emoji"} (set (keys font/bundled-fonts))))
  (doseq [path (concat (mapcat vals (vals font/bundled-fonts)) ["fonts/LICENSE-DejaVu.txt" "fonts/LICENSE-NotoEmoji.txt"])]
    (is (some? (clojure.java.io/resource path)) path)))

(deftest default-font-test
  (testing "every default text role uses the bundled DejaVu Sans Mono"
    (is (= #{"DejaVu Sans Mono"} (set (map :font (vals (:text theme/default-theme)))))))
  (testing "its faces are the bundled files, also with DejaVu Sans Mono installed"
    (doseq [[style path] (font/bundled-fonts "DejaVu Sans Mono")]
      (is (= (.getFontName (c2d/load-font (clojure.java.io/resource path)))
             (.getFontName (font/resolve-font "DejaVu Sans Mono" style 12))) (str style))))
  (testing "it displays power labels (superscript digits and minus) in every style"
    (doseq [style (keys font/font-styles)]
      (is (= -1 (.canDisplayUpTo (font/resolve-font "DejaVu Sans Mono" style 10) "2⁰¹²³⁴⁵⁶⁷⁸⁹⁻")) (str style)))))

(deftest font-resolution-test
  (testing "bundled styles are real faces from their files"
    (doseq [[style path] (font/bundled-fonts "DejaVu Sans Mono")]
      (is (= (.getFontName (c2d/load-font (clojure.java.io/resource path)))
             (.getFontName (font/resolve-font "DejaVu Sans Mono" style 12))) (str style))))
  (testing "size"
    (is (= 17.0 (double (.getSize2D (font/resolve-font "DejaVu Sans Mono" :plain 17))))))
  (testing "system and logical family names, case-insensitive"
    (is (= 13.0 (double (.getSize2D (font/resolve-font "SansSerif" :bold 13)))))
    (is (.isBold (font/resolve-font "sansserif" :bold 13)))
    (is (= 9.0 (double (.getSize2D (font/resolve-font "DejaVu Sans" :plain 9))))))
  (testing "Font object: size and style replaced"
    (let [f (font/resolve-font (java.awt.Font. "SansSerif" java.awt.Font/BOLD 10) :plain 13)]
      (is (not (.isBold f)))
      (is (= 13.0 (double (.getSize2D f))))))
  (testing "invalid"
    (are [f s z k] (= k (:key (error #(font/resolve-font f s z))))
      "DejaVu Sans Mono" :heavy 12 :font-style
      "DejaVu Sans Mono" :plain 0 :size
      "DejaVu Sans Mono" :plain nil :size
      nil :plain 12 :font
      42 :plain 12 :font
      "Liberaton Mono" :plain 12 :font)))

(deftest font-loaded-once-test
  (let [loads (atom {})
        load-font c2d/load-font]
    (with-redefs-fn {#'c2d/load-font (fn [url] (swap! loads update (str url) (fnil inc 0)) (load-font url))}
      (fn []
        (with-redefs-fn {#'font/bundled-font-files (#'font/bundled-font-delays)}
          (fn []
            (let [builds (doall (repeatedly 4 #(future (plot/build {:custom-grid examples/fig-1-1-labelled-grid}))))]
              (run! deref builds))
            (plot/build {:custom-grid examples/fig-1-1-labelled-grid})
            (is (seq @loads))
            (is (every? #(= 1 %) (vals @loads)) "each file loaded once, also under concurrent first use")))))))

(deftest bundled-metrics-pinned-test
  (testing "bundled font metrics are environment independent (regression value)"
    (is (= [217.0 14.0] (size {:kind :label :role :axis-title :text "0.123456789 Score: Golden Rain" :inset 0})))))

;; 3.x theme

(deftest theme-test
  (testing "reserved roles resolve"
    (doseq [role [:title :strip :axis-title :tick-label :legend-title :legend-label]
            :let [style (theme/text-style theme/default-theme {:role role})]]
      (is (font/resolve-font (:font style) (:font-style style) (:size style)) (str role))))
  (testing "nil in the plot theme doesn't remove a default"
    (is (= (get-in theme/default-theme [:text :title]) (get-in (theme/plot-theme {:text {:title nil}}) [:text :title])))
    (is (= theme/default-theme (theme/plot-theme nil))))
  (testing "deep merge keeps sibling keys"
    (let [t (theme/plot-theme {:text {:title {:size 30}}})]
      (is (= 30 (get-in t [:text :title :size])))
      (is (= (dissoc (get-in theme/default-theme [:text :title]) :size) (dissoc (get-in t [:text :title]) :size)))))
  (testing "build stores the merged theme"
    (is (= theme/default-theme (:theme (plot/build {}))))
    (is (= 30 (get-in (plot/build {:theme {:text {:title {:size 30}}}}) [:theme :text :title :size])))))

;; 4.x measurement

(deftest oracle-test
  (doseq [hint [:low :high]]
    (is (= (oracle-size ["Percent of Total"] axis-style hint) (size (label "Percent of Total") hint)) (str hint))))

(deftest glyph-independent-height-test
  (is (= (second (size (label "a"))) (second (size (label "g"))))))

(deftest no-canvas-test
  (is (= {:key :canvas} (error #(grid/measure-items
                                      (grid/prepare {:columns [{:size [:measured]}] :rows [{:size [:measured]}]
                                                     :items [{:id :l :column 0 :row 0 :content (label "a")}]})
                                      {:theme theme/default-theme} nil)))))

(deftest no-state-leak-test
  (let [b (label "bbb" :role :strip)]
    (is (= (size b)
           (measure/call-with-scratch-canvas
            :high (fn [c]
                    (measure/natural-size (label "AAA" :role :title :size 30) {:canvas c :theme theme/default-theme})
                    (let [s (measure/natural-size b {:canvas c :theme theme/default-theme})] [(.x s) (.y s)])))))))

(deftest theme-reaches-measurement-test
  (let [track (fn [t] (get-in (plot/build {:theme t :custom-grid {:columns [{:size [:measured]} {:size [:flexible]}]
                                                           :rows [{:size [:flexible]}]
                                                           :items [{:id :l :column 0 :row 0 :content (label "abc")}]}})
                              [:layout :columns 0 :size]))]
    (is (< (track nil) (track {:text {:axis-title {:size 30}}})))))

;; 5.x drawing

(deftest no-debug-draws-only-content-test
  (let [c (plot/render {:theme white-background :custom-grid {:columns [{:size [:flexible]} {:size [:flexible]}] :rows [{:size [:flexible]}]
                               :items [{:id :l :column 0 :row 0 :content (label "abc")}
                                       {:id :p :column 1 :row 0 :content {:kind :placeholder :size [10 10]}}]}}
                       {:width 200 :height 50})
        [_ _ x1 _] (ink-box c)]
    (is (< x1 100) "placeholder box stays white")))

(deftest clip-test
  (let [c (plot/render {:theme white-background :custom-grid {:columns [{:size [:fixed 20]} {:size [:flexible]}] :rows [{:size [:flexible]}]
                               :items [{:id :l :column 0 :row 0 :content (label "a very long text" :align [:left :middle])}
                                       {:id :m :column 1 :row 0 :content (label "x" :align [:right :middle])}]}}
                       {:width 200 :height 40})
        any-ink (constantly true)]
    (testing "clipped label leaves ink in its box only"
      (is (some? (ink-box c any-ink 0 20)))
      (is (nil? (ink-box c any-ink 20 150))))
    (testing "next item is not clipped"
      (is (some? (ink-box c any-ink 150 200))))))

(deftest color-test
  (let [c (render-label (label "XXXX" :color :red :size 30) 200 60)]
    (is (some? (ink-box c (fn [[r g b]] (and (> r g) (> r b))))))
    (is (nil? (ink-box c (fn [[r g b]] (or (> g r) (> b r))))))))

(deftest align-ink-test
  (let [center-x (fn [align] (let [[x0 _ x1 _] (ink-box (render-label (label "ab" :align align) 300 40))]
                               (/ (+ x0 x1) 2.0)))]
    (is (< (center-x [:left :middle]) 100))
    (is (> (center-x [:right :middle]) 200))
    (is (< 100 (center-x [:center :middle]) 200)))
  (let [center-y (fn [align] (let [[_ y0 _ y1] (ink-box (render-label (label "ab" :align align) 40 300))]
                               (/ (+ y0 y1) 2.0)))]
    (is (< (center-y [:center :top]) 100))
    (is (> (center-y [:center :bottom]) 200))))

(deftest rotation-ink-test
  (let [[x0 y0 x1 y1] (ink-box (render-label (label "Percent of Total" :rotation 270) 60 300))]
    (is (> (- y1 y0) (* 3 (- x1 x0))))))

(deftest transforms-restored-test
  (let [c (plot/render {:theme white-background :custom-grid {:columns [{:size [:fixed 60]} {:size [:flexible]}] :rows [{:size [:flexible]}]
                               :items [{:id :r :column 0 :row 0 :content (label "Percent of Total" :rotation 270)}
                                       {:id :h :column 1 :row 0 :content (label "Percent of Total")}]}}
                       {:width 400 :height 300})
        [x0 y0 x1 y1] (ink-box c (constantly true) 60 400)]
    (is (> (- x1 x0) (* 3 (- y1 y0))))))

(defn- in-graphics [w h f]
  (c2d/with-canvas [c (c2d/canvas w h)] (c2d/set-background c :white) (f c) c))

(deftest outer-clip-test
  (let [outer-clip (atom nil)
        c (in-graphics 200 60
                       (fn [c]
                         (.setClip ^java.awt.Graphics2D (:graphics c) 0 0 100 60)
                         (c2d/with-isolated-canvas [ic c]
                           (draw/draw-content (label "XXXXXXXXXXXXXXXXXXXX" :size 20) ic {:x 0 :y 0 :w 200 :h 60}
                                              {:theme theme/default-theme}))
                         (reset! outer-clip (.getBounds (.getClip ^java.awt.Graphics2D (:graphics c))))))]
    (testing "label clip intersects the outer clip"
      (is (some? (ink-box c (constantly true) 0 100)))
      (is (nil? (ink-box c (constantly true) 100 200))))
    (testing "outer clip is unchanged afterwards"
      (is (= (java.awt.Rectangle. 0 0 100 60) @outer-clip)))))

(deftest isolated-state-test
  (let [state (atom nil)]
    (in-graphics 100 60
                 (fn [c]
                   (let [g ^java.awt.Graphics2D (:graphics c)
                         before [(.getFont g) (.getColor g) (.getTransform g) (.getClip g)]]
                     (c2d/with-isolated-canvas [ic c]
                       (draw/draw-content (label "abc" :role :title :color :red :rotation 270) ic {:x 10 :y 5 :w 40 :h 50}
                                          {:theme theme/default-theme}))
                     (reset! state [before [(.getFont g) (.getColor g) (.getTransform g) (.getClip g)]]))))
    (is (= (first @state) (second @state)))))

(deftest isolated-on-exception-test
  (let [g-state (atom nil)]
    (in-graphics 10 10
                 (fn [c]
                   (let [g ^java.awt.Graphics2D (:graphics c)
                         transform (.getTransform g)]
                     (is (thrown? clojure.lang.ExceptionInfo
                                  (c2d/with-isolated-canvas [ic c] (c2d/translate ic 5 5) (throw (ex-info "boom" {})))))
                     (reset! g-state [transform (.getTransform g)]))))
    (is (= (first @g-state) (second @g-state)))))

;; 6.x layout with labels

(deftest fig-1-1-labelled-layout-test
  (let [built (plot/build {:custom-grid examples/fig-1-1-labelled-grid} {:width 800 :height 600})
        layout (:layout built)
        row-size (fn [id] (:size (first (filter #(= id (:id %)) (:rows layout)))))
        col-size (fn [id] (:size (first (filter #(= id (:id %)) (:columns layout)))))
        strip-h (second (size {:kind :label :role :strip :text "10"}))
        y-title-w (first (size (label "Percent of Total" :rotation 270)))]
    (is (= 1 (:rounds layout)))
    (is (= (long strip-h) (row-size [:strip 0]) (row-size [:strip 1])))
    (is (= (long y-title-w) (col-size :label)))))
