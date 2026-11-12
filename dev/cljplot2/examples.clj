(ns cljplot2.examples
  "Example grids shared by demos and tests.")

(defn- placeholder [w h] {:kind :placeholder :size [w h]})

(def fig-1-1-grid
  "LMDVR Fig 1.1 layout (2x3 facets) with placeholder content:
  columns `label | axis | panel | gap | panel | gap | panel`,
  rows `title | strip | panel | gap | strip | panel | axis | label`."
  {:columns [{:id :label :size [:measured]}
             {:id :axis :size [:measured]}
             {:id [:panel 0] :size [:flexible]}
             {:id [:gap 0] :size [:fixed 8]}
             {:id [:panel 1] :size [:flexible]}
             {:id [:gap 1] :size [:fixed 8]}
             {:id [:panel 2] :size [:flexible]}]
   :rows [{:id :title :size [:measured]}
          {:id [:strip 0] :size [:measured]}
          {:id [:panel 0] :size [:flexible]}
          {:id [:gap 0] :size [:fixed 8]}
          {:id [:strip 1] :size [:measured]}
          {:id [:panel 1] :size [:flexible]}
          {:id :axis :size [:measured]}
          {:id :label :size [:measured]}]
   :items (vec (concat
                [{:id :title :column {:from :label :to [:panel 2]} :row :title :content (placeholder 200 24)}
                 {:id :y-label :column :label :row {:from [:strip 0] :to [:panel 1]} :content (placeholder 16 100)}
                 {:id :x-label :column {:from [:panel 0] :to [:panel 2]} :row :label :content (placeholder 120 16)}]
                (for [r (range 2)]
                  {:id [:y-axis r] :column :axis :row [:panel r] :content (placeholder 36 50)})
                (for [c (range 3)]
                  {:id [:x-axis c] :column [:panel c] :row :axis :content (placeholder 50 24)})
                (for [r (range 2) c (range 3)]
                  {:id [:strip r c] :column [:panel c] :row [:strip r] :content (placeholder 40 16)})
                (for [r (range 2) c (range 3)]
                  {:id [:panel r c] :column [:panel c] :row [:panel r]})))})

(def nested-grid
  "Multiplot preview: 1x2 grid, left cell holds [[fig-1-1-grid]], right cell a 2x1 grid of panels."
  {:columns [{:id :left :size [:flexible 2]} {:id :right :size [:flexible 1]}]
   :rows [{:id :main :size [:flexible]}]
   :items [{:id :facets :column :left :row :main :content (assoc fig-1-1-grid :kind :grid)}
           {:id :stack :column :right :row :main
            :content {:kind :grid
                      :columns [{:size [:flexible]}]
                      :rows [{:size [:flexible]} {:size [:flexible]}]
                      :items [{:id :top :column 0 :row 0}
                              {:id :bottom :column 0 :row 1}]}}]})
