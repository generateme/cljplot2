(ns cljplot2.data.rng
  "Random generators of charts drawing random numbers (jitter, bootstrap): the `:rng` option and the generator one
  build draws from."
  (:require [fastmath.random :as r]
            [cljplot2.schema :as schema])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream NotSerializableException ObjectInputStream
            ObjectOutputStream Serializable]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schema
  "Schema of `:rng`: a fastmath random generator (see `fastmath.random/rng`)."
  (schema/named-type ::rng #(satisfies? fastmath.protocols/RNGProto %) "should be a fastmath random generator"))

(defn- copy
  "A copy of the serializable generator `g` in its current state."
  [^Serializable g]
  (let [bytes (ByteArrayOutputStream.)]
    (with-open [out (ObjectOutputStream. bytes)] (.writeObject out g))
    (with-open [in (ObjectInputStream. (ByteArrayInputStream. (.toByteArray bytes)))] (.readObject in))))

(defn for-build
  "The generator a build draws from for the `:rng` option `rng`: a copy of it in its current state (`rng` itself
  isn't advanced, so every build of a plot draws the same numbers); a new unseeded `:mersenne` generator when `rng`
  is `nil`. A generator that can't be copied (`fastmath.random/synced-rng`) is used itself - it advances, and builds
  differ."
  [rng]
  (cond
    (nil? rng) (r/rng :mersenne)
    (instance? Serializable rng) (try (copy rng) (catch NotSerializableException _ rng))
    :else rng))
