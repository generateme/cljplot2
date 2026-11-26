(ns cljplot2.data.text
  "Label texts made of runs: plain strings and LaTeX formulas.

  A label text is any value. A formula value (made by [[math]]) is one formula; a runs value (made by [[runs]]) is
  pieces following one another on one baseline - strings, formula values and other values (converted with `str`).
  Any other value, a vector too, is converted with `str`. Line breaks (LF or CRLF) in plain pieces split lines; a
  formula is never split.

  Dollar math: with it on, `$...$` in a plain piece is a formula when the opening dollar is followed by a non-space
  and the closing one follows a non-space and isn't followed by a digit (so `$5 and $10` stays literal); `$$...$$` is
  one formula too. A dollar after a backslash is a literal dollar, two backslashes before a dollar a literal
  backslash; a pair never spans a line break. Text roles and labels switch it with `:dollar-math` (default on, see
  [[cljplot2.theme/text-style]]).

  Functions: [[math]], [[runs]], [[formula?]], [[runs?]], [[formula-text?]], [[label-text]], [[label-text-schema]], [[source-text]], [[dollar-runs]],
  [[text-lines]], [[formulas?]], [[plain-lines]]."
  (:require [clojure.string :as str]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn math
  "Gives a formula value: a LaTeX formula a label draws as typeset math.

  Parameters:

  - `tex` (any value): the formula source in LaTeX math mode (converted with `str`), e.g. `x_i^2`.

  Returns `{:cljplot2/math tex}` - plain data, so plots holding it print and compare as data. A formula that doesn't
  parse is an error keyed `:text` (see [[cljplot2.graphics.latex/typeset]]).

  See also [[runs]], [[formula?]]."
  [tex]
  {:cljplot2/math (str tex)})

(defn runs
  "Gives a runs value: pieces of text drawn one after another on one baseline.

  Parameters:

  - `pieces` (values): strings (their `$...$` pairs are formulas with dollar math on), formula values of [[math]],
    runs values (their pieces taken in), other values (converted with `str`); `nil` is empty.

  Returns `{:cljplot2/runs [piece ..]}` - plain data. Line breaks in string pieces split lines.

  See also [[math]], [[runs?]]."
  [& pieces]
  {:cljplot2/runs (vec (mapcat (fn [p] (if (and (map? p) (contains? p :cljplot2/runs)) (:cljplot2/runs p) [p])) pieces))})

(defn formula?
  "Tests whether a value is a formula value of [[math]].

  Parameters:

  - `x`: any value.

  Returns truthy for a map with `:cljplot2/math`, else `nil` or false."
  [x]
  (and (map? x) (contains? x :cljplot2/math)))

(defn runs?
  "Tests whether a value is a runs value of [[runs]].

  Parameters:

  - `x`: any value.

  Returns truthy for a map with `:cljplot2/runs`, else `nil` or false."
  [x]
  (and (map? x) (contains? x :cljplot2/runs)))

(defn formula-text?
  "Tests whether a value is a text value of this namespace: a formula value or a runs value.

  Parameters:

  - `x`: any value.

  Returns truthy for [[formula?]] or [[runs?]] values, else `nil` or false. Such texts are passed on as they are,
  never converted with `str` (see [[label-text]])."
  [x]
  (or (formula? x) (runs? x)))

(defn label-text
  "Gives the text of a value as labels take it: a formula or runs value kept, any other value as a string.

  Unlike [[cljplot2.content.label/name-text]] a keyword keeps its colon (`str`).

  Parameters:

  - `x`: any value (e.g. what a `:formatter` returns).

  Returns `x` when [[formula-text?]], else `(str x)`."
  [x]
  (if (formula-text? x) x (str x)))

(def label-text-schema
  "Schema of a text given as a string or as a formula or runs value of this namespace (legend titles, pairs variable names)."
  [:or {:cljplot2/type-doc "a string, plot/math or plot/runs"}
   :string
   [:fn {:error/message "should be a string, a plot/math or a plot/runs value" :cljplot2/type-name "formula value"} formula-text?]])

(defn source-text
  "Gives the source of a text as one string, for messages.

  Parameters:

  - `t`: any value - a formula value (written between dollars), a runs value (its pieces' sources joined) or another
    value (converted with `str`).

  Returns a string."
  [t]
  (cond
    (formula? t) (str "$" (:cljplot2/math t) "$")
    (runs? t) (apply str (map source-text (:cljplot2/runs t)))
    :else (str t)))

(defn- space?
  "Tests whether a character is white space, a no-break space included.

  Parameters:

  - `c` (char): the character.

  Returns true or false."
  [^Character c]
  (or (Character/isWhitespace (.charValue c)) (Character/isSpaceChar (.charValue c))))

(defn- char-at
  "Gives the character of a string at an index.

  Parameters:

  - `s` (string): the text.
  - `i` (long): the index.

  Returns the character, `nil` outside the string."
  [^String s ^long i]
  (when (and (m/>= i 0) (m/< i (.length s))) (.charAt s i)))

(defn- escaped?
  "Tests whether the dollar at an index follows a backslash that isn't itself escaped.

  Parameters:

  - `s` (string): the text.
  - `i` (long): the dollar's index.

  Returns true when an odd number of backslashes stands right before it."
  [^String s ^long i]
  (loop [j (m/long-dec i) n 0]
    (if (= \\ (char-at s j)) (recur (m/long-dec j) (m/long-inc n)) (m/odd? n))))

(defn- closing-dollar
  "Gives the index of the dollar closing an inline formula opened before an index.

  Parameters:

  - `s` (string): the text.
  - `from` (long): the first index searched (just after the opening dollar).

  Returns the index of the first unescaped `$` following a non-space and not followed by a digit or another `$`,
  else `nil`."
  [^String s ^long from]
  (loop [i (.indexOf s "$" (int from))]
    (cond
      (m/neg? i) nil
      (or (escaped? s i)
          (not (char-at s (m/long-dec i)))
          (space? (char-at s (m/long-dec i)))
          (when-let [^Character c (char-at s (m/long-inc i))] (Character/isDigit (.charValue c))))
      (recur (.indexOf s "$" (int (m/long-inc i))))
      :else i)))

(defn- closing-double-dollar
  "Gives the index of the `$$` closing a display formula.

  Parameters:

  - `s` (string): the text.
  - `from` (long): the first index searched (just after the opening `$$`).

  Returns the index of the first unescaped `$$`, else `nil`."
  [^String s ^long from]
  (loop [i (.indexOf s "$$" (int from))]
    (cond
      (m/neg? i) nil
      (escaped? s i) (recur (.indexOf s "$$" (int (m/long-inc i))))
      :else i)))

(defn dollar-runs
  "Gives the runs of one text line with its dollar pairs turned into formulas (see the namespace doc).

  Parameters:

  - `line` (string): one line of text (no line breaks).

  Returns a vector of runs - non-empty strings and formula values of [[math]], in order; empty for an empty line. Outside a pair a backslash before a dollar is dropped (the dollar is literal) and two backslashes before a dollar give one backslash (the dollar may open a pair); a pair's source is kept as it is for TeX. A dollar opening no pair stays literal."
  [^String line]
  (if-not (str/includes? line "$")
    (if (= "" line) [] [line])
    (let [n (.length line)
          with-plain (fn [acc ^StringBuilder plain] (cond-> acc (m/pos? (.length plain)) (conj (str plain))))]
      (loop [i 0 plain (StringBuilder.) found []]
        (if (m/>= i n)
          (with-plain found plain)
          (let [ch (.charAt line i)
                next-ch (char-at line (m/long-inc i))]
            (cond
              (and (= \\ ch) (= \\ next-ch) (= \$ (char-at line (m/long-add i 2))))
              (recur (m/long-add i 2) (.append plain \\) found)

              (and (= \\ ch) (= \$ next-ch))
              (recur (m/long-add i 2) (.append plain \$) found)

              (and (= \$ ch) (= \$ next-ch))
              (let [close (closing-double-dollar line (m/long-add i 2))
                    tex (when close (subs line (m/long-add i 2) close))]
                (if (and tex (not (str/blank? tex)))
                  (recur (m/long-add (long close) 2) (StringBuilder.) (conj (with-plain found plain) (math tex)))
                  (recur (m/long-add i 2) (.append plain "$$") found)))

              (and (= \$ ch) next-ch (not (space? next-ch)))
              (if-let [close (closing-dollar line (m/long-inc i))]
                (recur (m/long-inc (long close)) (StringBuilder.) (conj (with-plain found plain) (math (subs line (m/long-inc i) close))))
                (recur (m/long-inc i) (.append plain \$) found))

              :else (recur (m/long-inc i) (.append plain ch) found))))))))

(defn- plain-runs
  "Gives the lines of a plain piece of text as runs.

  Parameters:

  - `s` (string): the text, possibly with line breaks.
  - `dollar-math` (boolean): true parses dollar pairs (see [[dollar-runs]]).

  Returns a vector of lines, each a vector of runs; an empty string gives one empty line."
  [s dollar-math]
  (mapv (fn [line] (if dollar-math (dollar-runs line) (if (= "" line) [] [line])))
        (str/split s #"\r?\n" -1)))

(defn- joined-lines
  "Joins the line lists of consecutive pieces: the last line of one continues with the first line of the next.

  Parameters:

  - `pieces` (sequence): line vectors (each a vector of runs).

  Returns one vector of lines."
  [pieces]
  (reduce (fn [lines piece]
            (if (empty? lines)
              piece
              (into (conj (pop lines) (into (peek lines) (first piece))) (rest piece))))
          [] pieces))

(defn text-lines
  "Gives the lines of a label text as runs.

  Parameters:

  - `t`: any value - a formula value, a runs value or another value (converted with `str`), see the namespace doc.
  - `dollar-math` (boolean): true turns dollar pairs of plain pieces into formulas (see [[dollar-runs]]).

  Returns a vector of lines, each a vector of runs (non-empty strings and formula values); empty text (`nil`, the empty string, a runs value without pieces) has no lines; empty inner or trailing lines are kept (as empty vectors)."
  [t dollar-math]
  (cond
    (formula? t) [[(math (:cljplot2/math t))]]
    (runs? t) (joined-lines (keep (fn [x]
                                    (cond (formula? x) [[(math (:cljplot2/math x))]]
                                          (nil? x) nil
                                          :else (let [s (str x)] (when-not (= "" s) (plain-runs s dollar-math)))))
                                  (:cljplot2/runs t)))
    :else (let [s (str t)] (if (= "" s) [] (plain-runs s dollar-math)))))

(defn formulas?
  "Tests whether lines of runs hold a formula.

  Parameters:

  - `lines` (sequence): lines of runs, see [[text-lines]].

  Returns truthy when some run is a formula value, else `nil`."
  [lines]
  (some #(some formula? %) lines))

(defn plain-lines
  "Gives lines of runs without formulas as strings.

  Parameters:

  - `lines` (sequence): lines of runs holding no formula (see [[formulas?]]).

  Returns a vector of strings, a line's runs joined."
  [lines]
  (mapv #(apply str %) lines))
