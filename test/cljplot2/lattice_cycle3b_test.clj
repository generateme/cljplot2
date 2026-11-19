(ns cljplot2.lattice-cycle3b-test
  "Lattice cycle 3b (3D): `chart/surface` (gridded, function, `[z c]`, density, parametric sources; styles, colors,
  facets, explicit domains) and the cloud additions (side projections, `:bins`)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.chart.cloud :as cloud]
            [cljplot2.chart.surface :as surface]
            [cljplot2.core :as plot]
            [cljplot2.data.field :as field]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows]
            [cljplot2.data.vector-field]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.space3d :as space-draw]
            [cljplot2.theme]
            [cljplot2.test-util :refer [error error-ins]])
  (:import [java.awt.image BufferedImage]))

(defn- near? [a b] (< (m/abs (- (double a) (double b))) 1e-9))
(defn- built [p] (plot/build p {:width 500 :height 400}))
(defn- image ^BufferedImage [p] (c2d/get-image (plot/render p {:width 400 :height 300})))
(defn- panel [p] (get-in (built p) [:stats 0 :data :panel]))

(def ^:private lattice
  "z = x + 10 y on x 0..2, y 0..1."
  (vec (for [x [0 1 2] y [0 1]] {:x x :y y :z (+ x (* 10 y))})))

(def ^:private plane {:f (fn [[x y]] (+ x y)) :domain {:x [0 1] :y [0 1]}})

(defn- primitives
  "Primitives of one panel's computed data of `chart` in a 400 x 300 panel (default theme)."
  [chart cp]
  (let [proj (space/projector (assoc (:frame cp) :domains (:domains cp)) {:w 400 :h 300 :margin 20})
        b (built {:charts [chart]})]
    ((:primitives chart) chart cp (:point proj) (:finalized-scales b) {:theme (:theme b)})))

;; 5 surface

(deftest surface-sources-test
  (testing "5.1 gridded: a node per lattice point, row by row"
    (let [{:keys [nu nv nodes]} (:mesh (panel {:data lattice :mapping {:x :x :y :y :z :z} :charts [(chart/surface)]}))]
      (is (= [3 2] [nu nv]))
      (is (= [[0.0 0.0 0.0] [1.0 0.0 1.0] [2.0 0.0 2.0] [0.0 1.0 10.0] [1.0 1.0 11.0] [2.0 1.0 12.0]] nodes))))
  (testing "5.1 gridded: a missing or repeated lattice point is keyed :z; no :z mapping - :mapping"
    (is (= :z (:key (error #(built {:data (pop lattice) :mapping {:x :x :y :y :z :z} :charts [(chart/surface)]})))))
    (is (= :z (:key (error #(built {:data (conj lattice (first lattice)) :mapping {:x :x :y :y :z :z} :charts [(chart/surface)]})))))
    (is (= :mapping (:key (error #(built {:data lattice :mapping {:x :x :y :y} :charts [(chart/surface)]}))))))
  (testing "5.1 a function: :n points per axis end to end (default 30), z = f(x, y)"
    (let [{:keys [nu nv nodes]} (:mesh (panel {:charts [(chart/surface (assoc plane :n [4 3]))]}))]
      (is (= [4 3] [nu nv]))
      (is (every? (fn [[x y z]] (near? z (+ x y))) nodes))
      (is (= [0.0 0.0 0.0] (first nodes)))
      (is (= [1.0 1.0 2.0] (peek nodes))))
    (is (= [30 30] ((juxt :nu :nv) (:mesh (panel {:charts [(chart/surface plane)]}))))))
  (testing "5.1 boundary: n = 2 - one quad"
    (is (= 1 (count (let [c (chart/surface (assoc plane :n 2))] (primitives c (panel {:charts [c]})))))))
  (testing "5.1 a function needs :x and :y domains"
    (is (= :domain (:key (error #(built {:charts [(chart/surface {:f + :domain {:x [0 1]}})]}))))))
  (testing "5.1 [z c]: c is the color value"
    (let [cp (panel {:charts [(chart/surface {:f (fn [[x y]] [(+ x y) (* 100 x)]) :domain {:x [0 1] :y [0 1]} :n 2})]})]
      (is (= [0.0 0.0 0.0] (first (:nodes (:mesh cp)))))
      (is (= [0.0 100.0 0.0 100.0] (:tones (:mesh cp))))))
  (testing "5.1 density: an n x n grid of non-negative heights over the points, z titled Density"
    (let [rows (for [i (range 30)] {:x (m/sin i) :y (m/cos (* 3 i))})
          cp (panel {:data rows :mapping {:x :x :y :y} :charts [(chart/surface {:density true :n 12})]})]
      (is (= [12 12] ((juxt :nu :nv) (:mesh cp))))
      (is (every? #(>= (double (% 2)) 0.0) (:nodes (:mesh cp))))
      (is (= "Density" (get-in cp [:frame :titles :z])))
      (is (= :mapping (:key (error #(built {:data rows :mapping {:x :x} :charts [(chart/surface {:density true})]})))))))
  (testing "5.1 one source per chart"
    (is (seq (error-ins #(built {:data lattice :mapping {:x :x :y :y} :charts [(chart/surface (assoc plane :density true))]}))))))

(deftest surface-styles-test
  (let [c (fn [opts] (chart/surface (merge plane {:n 4} opts)))
        prims (fn [opts] (let [ch (c opts)] (primitives ch (panel {:charts [ch]}))))]
    (testing "5.2 a polygon per quad in every style"
      (doseq [s [:surface :wireframe :both]]
        (is (= {:polygon 9} (frequencies (map :kind (prims {:style s})))) (str s))))
    (testing "5.2 surface: outlined in its fill; both: by :stroke-color; wireframe: background fill, colored outline"
      (is (every? #(= (:fill %) (:stroke %)) (prims {})))
      (is (every? #(= [1 2 3] (:stroke %)) (prims {:style :both :stroke-color [1 2 3]})))
      (let [ps (prims {:style :wireframe :color [9 9 9]})]
        (is (every? #(= [9 9 9] (:stroke %)) ps))
        (is (every? #(= 0xececf3 (:fill %)) ps) "the plot background")))
    (testing "5.2 shading on by default; :shade false - the color itself"
      (is (every? #(= [9 9 9] (:fill %)) (prims {:color [9 9 9] :shade false})))
      (is (not-every? #(= [9 9 9] (:fill %)) (prims {:color [9 9 9]}))))
    (testing "5.2 colored by height: a color bar; a fixed :color - none"
      (let [legends (fn [p] (filter #(= :legends (get-in % [:content :kind])) (:items (:grid (built p)))))]
        (is (= 1 (count (legends {:charts [(c {})]}))))
        (is (empty? (legends {:charts [(c {:color [9 9 9]})]})))
        (is (= {:color "z"} (surface/computed-aesthetics {} {:z :z})))
        (is (= {} (surface/computed-aesthetics {:color :red} {:z :z})))))
    (testing "5.2 a node left out takes its quads: a NaN interior node of a 4 x 4 grid leaves 5 of 9"
      (let [ch (chart/surface {:f (fn [[x y]] (if (and (near? x (/ 1.0 3)) (near? y (/ 1.0 3))) ##NaN 0.0)) :domain {:x [0 1] :y [0 1]} :n 4})]
        (is (= 5 (count (primitives ch (panel {:charts [ch]})))))))))

(deftest parametric-test
  (testing "5.3 the unit sphere spans ±1 on every axis"
    (let [cp (panel {:charts [(chart/surface {:fu (fn [[u v]] [(* (m/cos u) (m/sin v)) (* (m/sin u) (m/sin v)) (m/cos v)])
                                               :u [0 m/TWO_PI] :v [0 m/PI] :n 13})]})]
      (doseq [k [:x :y :z]]
        (let [[lo hi] (get-in cp [:domains k])] (is (near? -1.0 lo) (str k)) (is (near? 1.0 hi) (str k))))))
  (testing "5.3 :fu needs :u and :v; :u / :v need :fu"
    (is (seq (error-ins #(built {:charts [(chart/surface {:fu vector :u [0 1]})]}))))
    (is (seq (error-ins #(built {:charts [(chart/surface (assoc plane :u [0 1]))]})))))
  (testing "5.3 renders"
    (is (instance? BufferedImage (image {:charts [(chart/surface {:fu (fn [[u v]] [u v (* u v)]) :u [0 1] :v [0 1] :n 5})]})))))

(deftest surface-facets-and-domains-test
  (testing "5.4 a mesh per panel, one domain"
    (let [rows (for [g ["a" "b" "c" "d"] r lattice] (assoc r :g g :z (* (double (:z r)) (inc (int (first g))))))
          ds (map :domains (vals (get-in (built {:data rows :mapping {:x :x :y :y :z :z :facet :g} :charts [(chart/surface)]})
                                         [:stats 0 :data])))]
      (is (= 4 (count ds)))
      (is (apply = ds))))
  (testing "5.5 a cloud and a surface with explicit domains share the box"
    (let [domain {:x [0 2] :y [0 1] :z [0 12]}
          b (built {:data lattice :mapping {:x :x :y :y :z :z}
                    :charts [(chart/surface {:domain domain}) (chart/cloud {:domain domain})]})]
      (is (= (get-in b [:stats 0 :data :panel :domains]) (get-in b [:stats 1 :data :panel :domains])))
      (is (instance? BufferedImage (image {:data lattice :mapping {:x :x :y :y :z :z}
                                           :charts [(chart/surface {:domain domain}) (chart/cloud {:domain domain})]})))))
  (testing "5.5 nodes outside an explicit domain are left out"
    (let [cp (panel {:data lattice :mapping {:x :x :y :y :z :z} :charts [(chart/surface {:domain {:z [0 10.5]}})]})]
      (is (= 4 (count (remove nil? (:nodes (:mesh cp))))))
      (is (= [0 10.5] (:z (:domains cp)))))))

;; 6 cloud additions

(def ^:private cloud-rows
  (vec (for [i (range 40)] {:x (m/sin i) :y (m/cos (* 2 i)) :z (* 0.1 i)})))

(defn- wall-coords
  "Per projection primitive of `computed` its points in data units (the projection is the identity)."
  [computed]
  (let [identity-point (fn [p] [p 0.0])]
    (for [p (cloud/projection-primitives computed identity-point (constantly (:style computed)))]
      [(:kind p) (case (:kind p) :point [(:at p)] :path (:points p) :region (apply concat (:rings p)))])))

(deftest projections-test
  (let [cp (fn [view] (panel {:data cloud-rows :mapping {:x :x :y :y :z :z}
                              :charts [(chart/cloud {:view view :projections {:xy :density :xz :contour :yz :points}})]}))
        on-wall? (fn [{:keys [domains]} view]
                   (fn [[kind pts]]
                     (let [[i axis] (case kind :region [2 :z] :path [1 :y] :point [0 :x])
                           wall (nth (domains axis) (space/far-side view [1 1 1] axis))]
                       (every? #(near? wall (% i)) pts))))]
    (testing "6.1 every projection in its wall's plane, on the far wall; all three kinds"
      (doseq [view [{:azimuth 40 :elevation 30} {:azimuth 220 :elevation 30}]]
        (let [c (cp view) ws (wall-coords c)]
          (is (= #{:region :path :point} (set (map first ws))))
          (is (every? (on-wall? c view) ws)))))
    (testing "6.1 the x / y walls switch under azimuth + 180"
      (is (not= (space/far-side {:azimuth 40 :elevation 30} [1 1 1] :x) (space/far-side {:azimuth 220 :elevation 30} [1 1 1] :x)))
      (is (not= (space/far-side {:azimuth 40 :elevation 30} [1 1 1] :y) (space/far-side {:azimuth 220 :elevation 30} [1 1 1] :y))))
    (testing "6.1 projections drawn before the cloud (depth +Inf), inside the box"
      (let [c (cp {:azimuth 40 :elevation 30})
            ws (wall-coords c)]
        (is (every? #(= ##Inf (:depth %)) (cloud/projection-primitives c (fn [p] [p 0.0]) (constantly (:style c)))))
        (is (every? (fn [[_ pts]] (every? (fn [p] (every? (fn [i] (space/in-domain? ((:domains c) (space/axes i)) (p i))) [0 1 2])) pts)) ws))))
    (testing "6.1 edge-on wall (elevation 0): no floor projection"
      (is (empty? (filter #(= :region (first %)) (wall-coords (cp {:azimuth 40 :elevation 0}))))))
    (testing "6.1 boundary: one point gives no density, only its point projection"
      (is (= [:point] (map first (wall-coords (panel {:data [{:x 0 :y 0 :z 0}] :mapping {:x :x :y :y :z :z}
                                                       :charts [(chart/cloud {:projections {:xy :density :yz :points}})]}))))))
    (testing "6.1 renders"
      (is (instance? BufferedImage (image {:data cloud-rows :mapping {:x :x :y :y :z :z}
                                           :charts [(chart/cloud {:projections {:xy :density :xz :contour :yz :points}})]}))))))

(deftest bins-test
  (let [rows (conj cloud-rows {:x 1.0 :y 1.0 :z 0})
        cp (fn [opts] (panel {:data rows :mapping {:x :x :y :y} :charts [(chart/cloud (merge {:type :bars :bins [4 3]} opts))]}))]
    (testing "6.2 counts sum to the rows; a point at the far end counts in the last cell"
      (let [{:keys [points]} (cp {})]
        (is (= (count rows) (reduce + (:z points))))
        (is (= 41 (reduce + (:z (cloud/bin-counts (map :x rows) (map :y rows) [[-1 1] [-1 1]] [1 1])))))
        (is (= {:x [0.75] :y [0.5] :z [1]} (cloud/bin-counts [1.0] [1.0] [[-1 1] [-1 1]] [4 2])))))
    (testing "6.2 bars sized to cells, the domain the binned range, base 0, z titled Count"
      (let [{:keys [half domains base frame]} (cp {})
            [x0 x1] (:x domains)]
        (is (near? (* 0.5 0.8 (/ (- x1 x0) 4.0)) (first half)))
        (is (= 0.0 base))
        (is (= 0.0 (first (:z domains))))
        (is (= "Count" (get-in frame [:titles :z])))))
    (testing "6.2 spikes too; an explicit domain bins its range"
      (is (= [-2 2] (:x (:domains (cp {:type :spikes :domain {:x [-2 2]}}))))))
    (testing "6.2 rules: :bins needs bars / spikes, no aesthetic mapping, no projections; no :z without :bins"
      (is (seq (error-ins #(built {:data rows :mapping {:x :x :y :y} :charts [(chart/cloud {:bins [2 2]})]}))))
      (is (seq (error-ins #(built {:data rows :mapping {:x :x :y :y} :charts [(chart/cloud {:bins [2 2] :type :bars :projections {:xy :points}})]}))))
      (is (= :bins (:key (error #(built {:data (map (fn [r] (assoc r :g "a")) rows) :mapping {:x :x :y :y :color :g}
                                         :charts [(chart/cloud {:bins [2 2] :type :bars})]})))))
      (is (= :mapping (:key (error #(built {:data rows :mapping {:x :x :y :y} :charts [(chart/cloud)]}))))))
    (testing "6.2 renders"
      (is (instance? BufferedImage (image {:data rows :mapping {:x :x :y :y} :charts [(chart/cloud {:type :bars :bins [4 3]})]}))))))

;; review fixes

(deftest nice-domain-test
  (testing "R1 data-derived domains made nice; explicit ones kept"
    (is (= [-0.5 1.5] (space/data-domain [-0.4953 1.2551] 0.0)))
    (is (= [165.0 190.0] (space/data-domain [165.67 188.13] 0.0)))
    (is (= [0.0 1.0] (space/data-domain [] 0.0)) "no values")
    (is (= [4.4 5.6] (space/data-domain [5 5] 0.0)) "one value: widened to [4.5 5.5], then nice")
    (is (= [690 40] (:z (:domains (panel {:data cloud-rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:domain {:z [690 40]}})]}))))))
  (testing "R1 axis ticks include both ends of a nice domain; else n ticks"
    (let [{:keys [ticks]} (space-draw/edge-ticks :z [-0.5 1.5] nil 6)]
      (is (= -0.5 (double (first ticks))))
      (is (= 1.5 (double (peek ticks)))))
    (is (= (space-draw/axis-ticks :z [690 40] 5) (space-draw/edge-ticks :z [690 40] nil 5)) "no count reaches both ends")))

(deftest categorical-axes-test
  (let [rows [{:x "b" :y 1 :z 1} {:x "a" :y 2 :z 2} {:x "b" :y 3 :z 3} {:x nil :y 4 :z 4}]
        cp (panel {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :bars})]})]
    (testing "R2 text x: levels in first-appearance order at 1, 2, ..; domain [0.5 n + 0.5]; nil dropped"
      (is (= ["b" "a"] (get-in cp [:frame :levels :x])))
      (is (= [1.0 2.0 1.0] (get-in cp [:points :x])))
      (is (= [0.5 2.5] (get-in cp [:domains :x])))
      (is (near? 0.4 (first (:half cp))) "bars 0.8 of a level")
      (is (nil? (get-in cp [:frame :levels :y]))))
    (testing "R2 a categorical marker sorts numbers; intervals sort"
      (is (= [0 2 10] (get-in (panel {:data [{:x 10 :y 1 :z 1} {:x 0 :y 1 :z 1} {:x 2 :y 1 :z 1}]
                                      :mapping {:x (levels/categorical :x) :y :y :z :z} :charts [(chart/cloud)]})
                              [:frame :levels :x])))
      (let [binned (levels/bin (map (fn [i] {:v i :z i}) (range 20)) :v {:as :bin :bins 4})]
        (is (= (sort (distinct (map :bin binned)))
               (get-in (panel {:data binned :mapping {:x :bin :y :z :z :z} :charts [(chart/cloud)]}) [:frame :levels :x])))))
    (testing "R2 rules: numbers mixed with text - :mapping; explicit domain - :domain; :bins - :bins"
      (is (= :mapping (:key (error #(built {:data (conj rows {:x 1 :y 1 :z 1}) :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})))))
      (is (= :domain (:key (error #(built {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:domain {:x [0 3]}})]})))))
      (is (= :bins (:key (error #(built {:data rows :mapping {:x :x :y :y} :charts [(chart/cloud {:bins [2 2] :type :bars})]}))))))
    (testing "R2 ticks: a tick per level; labels thinned to every ceil(levels / n)-th"
      (let [ls (mapv str (range 10))]
        (is (= ["0" nil nil nil "4" nil nil nil "8" nil] (:labels (space-draw/edge-ticks :y [0.5 10.5] ls 3))))
        (is (= ls (:labels (space-draw/edge-ticks :y [0.5 10.5] ls 10))))
        (is (= ["a"] (:labels (space-draw/edge-ticks :y [0.5 1.5] [:a] 3))) "one level, a keyword by name")
        (is (= (mapv double (range 1 11)) (:ticks (space-draw/edge-ticks :y [0.5 10.5] ls 3))))))
    (testing "R2 a positional categorical marker in a 3D plot declares no position scale; renders"
      (is (instance? BufferedImage (image {:data rows :mapping {:x (levels/categorical :y) :y :x :z :z} :charts [(chart/cloud {:type :bars})]}))))))

(deftest projections-only-test
  (testing "R3 :type :none - no marks, projections kept"
    (let [cp (panel {:data cloud-rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :none :projections {:xy :points}})]})]
      (is (empty? (cloud/primitives cp (fn [p] [p 0.0]) (constantly (:style cp)))))
      (is (= #{:point} (set (map first (wall-coords cp))))))))

(deftest label-overlap-test
  (testing "R4 an axis leaves out labels overlapping earlier axes' labels; returns the boxes it drew"
    (let [proj (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view space/default-view} {:w 400 :h 300 :margin 30})
          frame {:domains {:x [0 1] :y [0 1] :z [0 1]} :titles {}}
          env {:theme cljplot2.theme/default-theme}
          edge (:x (space/axis-edges space/default-view [1 1 1] (:corner proj)))
          draw (fn [taken] (c2d/with-canvas [c (c2d/canvas 400 300)] (#'space-draw/draw-axis! c :x edge proj frame taken (assoc env :canvas c))))
          drawn (draw [])]
      (is (seq drawn))
      (is (empty? (draw [[-1e6 -1e6 1e6 1e6]])) "everything taken")
      (is (= drawn (draw [[-10 -10 -5 -5]])) "a box elsewhere"))))

(deftest density-cut-test
  (let [rows (for [i (range 30)] {:x (m/sin i) :y (m/cos (* 3 i))})
        xs (fn [opts] (map first (remove nil? (:nodes (:mesh (panel {:data rows :mapping {:x :x :y :y} :charts [(chart/surface (merge {:density true :n 5} opts))]}))))))]
    (testing "R5 :cut 0 - the points' range; default 3 bandwidths wider"
      (is (near? (reduce min (map :x rows)) (reduce min (xs {:cut 0}))))
      (is (< (reduce min (xs {})) (reduce min (map :x rows)))))
    (testing "R5 :cut only with :density"
      (is (seq (error-ins #(built {:charts [(chart/surface (assoc plane :cut 1))]})))))))

(deftest scene-frame-test
  (let [first-frame {:view {:azimuth 10 :elevation 20} :aspect [1 1 1] :box? true :axes? true :titles {:x "a" :y "b"}}
        scene (fn [& overrides] (space-draw/scene-frame (cons {:frame first-frame :domains {:x [0 1]}}
                                                              (map (fn [o] {:frame-overrides o}) overrides))))]
    (testing "R7 later charts' explicit frame options merged over the first's, in order"
      (is (= (assoc first-frame :domains {:x [0 1]}) (scene)))
      (is (= {:azimuth 10 :elevation 50} (:view (scene {:view {:elevation 50}}))) "view key by key")
      (is (= {:x "a" :y "B" :z "c"} (:titles (scene {:titles {:y "B"}} {:titles {:z "c"}}))))
      (is (nil? (:titles (scene {:titles nil}))) "nil clears the titles")
      (is (= [2 1 1] (:aspect (scene {:aspect [3 1 1]} {:aspect [2 1 1]}))) "the last wins")
      (is (false? (:box? (scene {:box? false})))))
    (testing "R7 overrides hold only the options set; :axis-titles nil - no titles"
      (is (= {} (cloud/frame-overrides {:box nil :size 3})))
      (is (= {:view {:azimuth 5} :box? false :titles nil} (cloud/frame-overrides {:view {:azimuth 5} :box false :axis-titles nil})))
      (is (nil? (:titles (cloud/frame-settings {:axis-titles nil} {:x :x})))))
    (testing "R7 a cloud's view over a surface's frame"
      (let [domain {:x [0 2] :y [0 1] :z [0 12]}
            b (built {:data lattice :mapping {:x :x :y :y :z :z}
                      :charts [(chart/surface {:domain domain}) (chart/cloud {:domain domain :view {:azimuth 99}})]})]
        (is (= 99 (get-in (space-draw/scene-frame [(get-in b [:stats 0 :data :panel]) (get-in b [:stats 1 :data :panel])])
                          [:view :azimuth])))))))

(deftest projection-density-precheck-test
  (testing "R8 no density without 2 distinct values on each axis; a spread gives one"
    (let [cp (fn [rows] (panel {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:projections {:xy :density}})]}))]
      (is (nil? (get-in (cp [{:x 1 :y 0 :z 0} {:x 1 :y 1 :z 1} {:x 1 :y 2 :z 2}]) [:projections :xy :grid])))
      (is (some? (get-in (cp [{:x 0 :y 0 :z 0} {:x 1 :y 1 :z 1} {:x 2 :y 3 :z 2}]) [:projections :xy :grid]))))))

(deftest vector-argument-test
  (testing "surface :f and :fu take one vector argument"
    (let [seen (atom nil)]
      (panel {:charts [(chart/surface {:f (fn [p] (reset! seen p) 0.0) :domain {:x [0 1] :y [0 1]} :n 2})]})
      (is (= [1.0 1.0] @seen)))
    (is (thrown? clojure.lang.ArityException
                 (panel {:charts [(chart/surface {:f (fn [_x _y] 0.0) :domain {:x [0 1] :y [0 1]} :n 2})]})))))

;; review 2 fixes

(deftest review2-fixes-test
  (testing "V1 the frame margin measures every level label, not the thinned ones"
    (let [frame (fn [ls] {:domains {:x [0.5 (+ 0.5 (count ls))] :y [0 1] :z [0 1]} :levels {:x ls} :titles {} :axes? true})
          margin (fn [ls] (c2d/with-canvas [c (c2d/canvas 10 10)]
                            (space-draw/frame-margin (frame ls) [400 300] {:theme cljplot2.theme/default-theme :canvas c})))
          short (mapv str (range 10))
          long-second (assoc short 1 "a very long level label")]
      (is (< (double (margin short)) (double (margin long-second))) "level 1 is never a thinned label, yet counts")))
  (testing "V2 NaN under a categorical marker is no level; its row dropped"
    (let [cp (panel {:data [{:x 1 :y 0 :z 0} {:x ##NaN :y 1 :z 1} {:x 2 :y 2 :z 2}]
                     :mapping {:x (levels/categorical :x) :y :y :z :z} :charts [(chart/cloud)]})]
      (is (= [1 2] (get-in cp [:frame :levels :x])))
      (is (= 2 (count (get-in cp [:points :x]))))))
  (testing "V3 a categorical marker on :z throws {:key :mapping}"
    (is (= :mapping (:key (error #(built {:data cloud-rows :mapping {:x :x :y :y :z (levels/categorical :z)} :charts [(chart/cloud)]}))))))
  (testing "vector contract: field / contour :f, :pixel-color, domain-color and field/sample take a point [x y]"
    (let [seen (atom #{})
          f (fn [p] (swap! seen conj (count p)) 0.5)]
      (built {:charts [(chart/field {:f f :domain {:x [0 1] :y [0 1]} :n 3})]})
      (built {:charts [(chart/contour {:f f :domain {:x [0 1] :y [0 1]} :n 3})]})
      (is (= #{2} @seen)))
    (is (= [[0.0 0.0] [1.0 0.0] [0.0 1.0] [1.0 1.0]]
           (let [ps (atom [])] (field/sample (fn [p] (swap! ps conj p) 0.0) [[0 1] [0 1]] [2 2]) @ps)))
    (is (some? ((field/domain-color identity) [0.5 0.5])))
    (is (thrown? clojure.lang.ArityException ((field/domain-color identity) 0.5 0.5)))
    (is (instance? BufferedImage (image {:charts [(chart/field {:pixel-color (fn [[x _]] (if (< (double x) 0.5) :black :white))
                                                                :domain {:x [0 1] :y [0 1]}})]})))))

;; review 3 fixes

(deftest review3-fixes-test
  (testing "W3 every 2D point function receives a Vec2"
    (let [types (atom #{})
          f (fn [p] (swap! types conj (type p)) 0.5)]
      (built {:charts [(chart/field {:f f :domain {:x [0 1] :y [0 1]} :n 2})]})
      (built {:charts [(chart/contour {:f f :domain {:x [0 1] :y [0 1]} :n 2})]})
      (panel {:charts [(chart/surface {:f f :domain {:x [0 1] :y [0 1]} :n 2})]})
      (panel {:charts [(chart/surface {:fu (fn [p] (swap! types conj (type p)) [0 0 0]) :u [0 1] :v [0 1] :n 2})]})
      (image {:charts [(chart/field {:pixel-color (fn [p] (swap! types conj (type p)) :red) :domain {:x [0 1] :y [0 1]}})]})
      (is (= #{fastmath.vector.Vec2} @types))))
  (testing "W3 point-valued results: any sequential value or array"
    (doseq [r [[1.0 2.0] '(1.0 2.0) (fastmath.vector/vec2 1.0 2.0) (double-array [1.0 2.0])]]
      (is (= [1.0 2.0] (mapv double (cljplot2.data.rows/as-coordinates r))) (str (type r))))
    (doseq [r [nil 1.0 {:a 1} "ab"]]
      (is (nil? (cljplot2.data.rows/as-coordinates r)) (pr-str r)))
    (let [{:keys [nodes tones]} (:mesh (panel {:charts [(chart/surface {:f (fn [_] (fastmath.vector/vec2 1.0 2.0)) :domain {:x [0 1] :y [0 1]} :n 2})]}))]
      (is (= [0.0 0.0 1.0] (first nodes)) "[z c] as a Vec2 - z 1")
      (is (= [2.0 2.0 2.0 2.0] tones) "c 2"))
    (is (= [2.0 2.0 2.0 2.0] (:tones (:mesh (panel {:charts [(chart/surface {:f (fn [_] (list 1.0 2.0)) :domain {:x [0 1] :y [0 1]} :n 2})]}))))
        "[z c] as a list")
    (is (= [1.0 2.0 3.0] (first (:nodes (:mesh (panel {:charts [(chart/surface {:fu (fn [_] (fastmath.vector/vec3 1 2 3)) :u [0 1] :v [0 1] :n 2})]}))))))
    (is (= [1.0 2.0 3.0] (first (:nodes (:mesh (panel {:charts [(chart/surface {:fu (fn [_] (double-array [1 2 3])) :u [0 1] :v [0 1] :n 2})]}))))))
    (is (every? nil? (:nodes (:mesh (panel {:charts [(chart/surface {:fu (fn [_] {:x 1}) :u [0 1] :v [0 1] :n 2})]})))) "a map is no point")
    (is (= 1 (count (cljplot2.data.vector-field/mapped-points (fn [[x y]] (double-array [y x])) [[1 2]]))) "an array pair")
    (is (= 1 (count (cljplot2.data.vector-field/mapped-points (fn [[x y]] [y x]) [(double-array [1 2])]))) "an array seed")
    (is (= :f (:key (error #(cljplot2.data.vector-field/mapped-points (fn [_] [1 2 3]) [[1 2]])))) "3 values - no pair"))
  (testing "W1 a translucent :surface fill has no seam outline; an opaque one keeps it"
    (let [prims (fn [color] (let [ch (chart/surface (merge plane {:n 3 :color color :shade false}))] (primitives ch (panel {:charts [ch]}))))]
      (is (every? #(= :none (:stroke %)) (prims [10 20 30 254])))
      (is (every? #(= (:fill %) (:stroke %)) (prims [10 20 30 255])))
      (is (every? #(= (:fill %) (:stroke %)) (prims [10 20 30]))))))
