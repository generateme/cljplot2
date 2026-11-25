(ns cljplot2.data.rng
  "Random generators of charts drawing random numbers (jitter, bootstrap).

  A chart's `:rng` option takes a fastmath random generator ([[schema]]); a build draws from a copy of it ([[for-build]]), so every build of a plot draws the same numbers.

  Functions: [[for-build]] gives the generator a build draws from."
  (:require [fastmath.random :as r]
            [cljplot2.schema :as schema])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream NotSerializableException ObjectInputStream
            ObjectOutputStream Serializable]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schema
  "Schema of `:rng`: a fastmath random generator (see `fastmath.random/rng`)."
  (schema/named-type ::rng #(satisfies? fastmath.protocols/RNGProto %) "should be a fastmath random generator"
                     {:cljplot2/type-name "random generator"
                      :cljplot2/type-meaning "A fastmath random generator, e.g. `(r/rng :mersenne 7)`; a seeded one draws the same values on every build."}))

(defn- copy
  "Copies a serializable random generator in its current state.

  Parameters:

  - `g` (`java.io.Serializable`): the generator.

  Returns a new generator in the same state; `g` isn't advanced.

  Throws `java.io.NotSerializableException` when a part of `g` can't be serialized."
  [^Serializable g]
  (let [buffer (ByteArrayOutputStream.)]
    (with-open [out (ObjectOutputStream. buffer)] (.writeObject out g))
    (with-open [in (ObjectInputStream. (ByteArrayInputStream. (.toByteArray buffer)))] (.readObject in))))

(defn for-build
  "Gives the random generator a build draws from for a chart's `:rng` option.

  Parameters:

  - `rng`: a fastmath random generator, or `nil`.

  Returns a copy of `rng` in its current state (`rng` itself isn't advanced, so every build of a plot draws the same numbers); a new unseeded `:mersenne` generator when `rng` is `nil`. A generator that can't be copied (`fastmath.random/synced-rng`) is returned itself - it advances, and builds differ."
  [rng]
  (cond
    (nil? rng) (r/rng :mersenne)
    (instance? Serializable rng) (try (copy rng) (catch NotSerializableException _ rng))
    :else rng))
