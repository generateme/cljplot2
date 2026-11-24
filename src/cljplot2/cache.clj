(ns cljplot2.cache
  "Bounded caches of values that are costly to compute and asked for again: transcoded SVG images, malli explainers,
  shape outlines.

  A cache keeps at most its threshold of entries and evicts the least recently used one when full. A value is
  computed once per key, also when threads miss the same key together; a computation that throws caches nothing.
  Caches are internal: their thresholds aren't options. A hit updates the entry's recency (an atom `swap!`, about 0.5 µs against a few ns for a plain map): fine per plot or per drawn mark (a shape outline costs more to draw), not in a tight loop that draws nothing.

  Functions: [[lru-cache]] makes a cache, [[lookup-or-compute]] reads through it, [[entry-count]] and [[clear!]]
  serve tests."
  (:require [clojure.core.cache.wrapped :as cw]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn lru-cache
  "Gives a new empty least-recently-used cache.

  Parameters:

  - `threshold` (positive long): the most entries the cache keeps.

  Returns the cache (an atom; read it only through this namespace). See [[lookup-or-compute]]."
  [^long threshold]
  (cw/lru-cache-factory {} :threshold threshold))

(defn lookup-or-compute
  "Gives the value cached under `k`, computing and caching it on a miss.

  Parameters:

  - `cache`: a cache of [[lru-cache]].
  - `k`: the key (compared by value).
  - `compute` (fn of `k`): computes the value; called at most once per miss, also when threads miss `k` together.

  Returns the value, which may be `nil`. A hit makes `k` the most recently used entry; a miss on a full cache evicts
  the least recently used one. When `compute` throws, nothing is cached and the exception propagates."
  [cache k compute]
  (cw/lookup-or-miss cache k compute))

(defn entry-count
  "Gives the number of entries a cache holds.

  Parameters:

  - `cache`: a cache of [[lru-cache]].

  Returns the count as a long, at most the cache's threshold."
  ^long [cache]
  (cw/size cache))

(defn clear!
  "Empties a cache, keeping its threshold; for tests.

  Parameters:

  - `cache`: a cache of [[lru-cache]].

  Returns `nil`."
  [cache]
  (cw/seed cache {})
  nil)
