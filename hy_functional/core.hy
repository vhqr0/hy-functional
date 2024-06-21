(eval-and-compile
  (import
    builtins
    types
    collections
    collections.abc :as abc
    typing
    dataclasses
    itertools
    functools
    operator
    contextlib))

;;; macros

(defmacro comment [#* body]
  `(do))

(defmacro ignore [#* body]
  `(do ~@body None))

(defmacro if-not [test else then]
  `(if ~test ~then ~else))

(defmacro when-not [test #* body]
  `(if ~test None (do ~@body)))

(defmacro def [#* args]
  `(setv ~@args))

(eval-and-compile
  (defn _arity [#* adefs]
    `(match args
       ~@(itertools.chain.from-iterable
           (lfor [arity #* body] adefs
                 `(#(~@arity) (do ~@body))))
       _ (raise TypeError))))

(defmacro farity [args #* adefs]
  `(fn [#* args]
     ~(_arity #* adefs)))

(defmacro defarity [name #* adefs]
  `(defn ~name [#* args]
     ~(_arity #* adefs)))

(eval-and-compile
  (defarity _->
    ([x]
      x)
    ([x form]
      (if (isinstance form hy.models.Expression)
        (let [[head #* tails] form]
          `(~head ~x ~@tails))
        `(~form ~x)))
    ([x form #* forms]
      (_-> (_-> x form) #* forms)))
  (defarity _->>
    ([x]
      x)
    ([x form]
      (if (isinstance form hy.models.Expression)
        `(~@form ~x)
        `(~form ~x)))
    ([x form #* forms]
      (_->> (_->> x form) #* forms))))

(defmacro -> [x #* forms]
  (_-> x #* forms))

(defmacro ->> [x #* forms]
  (_->> x #* forms))

(defmacro doto [x #* forms]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(map (fn [form] (_-> $ form)) forms)
       ~$)))

(defmacro cond-> [x #* clauses]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(map
           (fn [clause]
             (let [[test form] clause]
               `(when ~test (setv ~$ ~(_-> $ form)))))
           (itertools.batched clauses 2)))))

(defmacro cond->> [x #* clauses]
  (let [$ (hy.gensym)]
    `(let [~$ ~x]
       ~@(map
           (fn [clause]
             (let [[test form] clause]
               `(when ~test (setv ~$ ~(_->> $ form)))))
           (itertools.batched clauses 2)))))

(defmacro loop [bindings #* body]
  (let [ls (->> (itertools.batched bindings 2) (map (operator.itemgetter 0)) list)]
    (defn expand [form]
      (if (isinstance form hy.models.Expression)
        (let [[head #* tails] form]
          (if (= head 'recur)
            `(do
               ~@(when ls `((setv [~@ls] [~@tails])))
               (continue))
            (hy.models.Expression (map expand form))))
        form))
    (let [form `(while True (return ~(expand `(do ~@body))))
          form (if bindings `(let ~bindings ~form) form)]
      `((fn [] ~form)))))

;;; baisc

(defn none? [x] (is x None))
(defn some? [x] (is-not x None))

(defn inc   [x] (+ x 1))
(defn dec   [x] (- x 1))
(defn zero? [x] (= x 0))
(defn pos?  [x] (> x 0))
(defn neg?  [x] (< x 0))
(defn even? [x] (zero? (& x 1)))
(defn odd?  [x] (not (even? x)))

;;; func

(defn ignore [#* args #** kwargs])

(defn constantly [x] (fn [#* args #** kwargs] x))

(defn identity [x] x)

(defarity comp
  ([]
    identity)
  ([f]
    f)
  ([f g]
    (fn [x] (f (g x))))
  ([#* fs]
    (reduce comp fs)))

(def partial functools.partial)

(defarity curry
  ([]
    curry)
  ([n]
    (partial curry n))
  ([n f #* args]
    (if (>= (len args) n)
      (f #* args)
      (partial curry n f #* args))))

(defn complement [pred] (comp not pred))

;;; iter

(defarity reduce
  ([f iterable] (functools.reduce f iterable))
  ([f init iterable] (functools.reduce f iterable init)))

(def remove itertools.filterfalse)

(def cat itertools.chain.from-iterable)

(def concat itertools.chain)

(defn cons [x iterable]
  (itertools.chain [x] iterable))

(defn mapcat [f #* iterables]
  (cat (map f #* iterables)))

(defn first [iterable]
  (next (iter iterable)))

(defn rest [iterable]
  (doto (iter iterable) next))

(defn empty? [iterable]
  (try
    (next (iter iterable))
    False
    (except [StopIteration]
      True)))

(def second (comp first rest))

(defn last [iterable]
  (let [it (iter iterable)
        x (next it)]
    (for [i it]
      (setv x i))
    x))

(defn butlast [iterable]
  (let [it (iter iterable)]
    (setv x (next it))
    (for [i it]
      (yield x)
      (setv x i))))

(defn iterate [f init]
  (loop [acc init]
    (do
      (yield acc)
      (recur (f acc)))))

(defn repeat [x]
  (loop []
    (do
      (yield x)
      (recur))))

(defn repeatedly [f]
  (loop []
    (do
      (yield (f))
      (recur))))

(defn cycle [iterable]
  (loop []
    (do
      (yield-from iterable)
      (recur))))

(defn interleave [#* iterables]
  (cat (zip #* iterables)))

(defn interpose [sep iterable]
  (let [it (interleave (repeat sep) iterable)]
    (with [_ (contextlib.suppress StopIteration)]
      (next it))
    it))

;;; coll

(defarity conj
  ([l x]
    (doto l (.append x)))
  ([l #* xs]
    (doto l (.extend xs))))

(defn into [l iterable]
  (doto l (.extend iterable)))

(defarity assoc
  ([d k v]
    (setv (get d k) v)
   d)
  ([d #* kvs]
    (for [[k v] (itertools.batched kvs 2)]
      (setv (get d k) v))
   d))

(defarity dissoc
  ([d k]
    (del (get d k))
   d)
  ([d #* ks]
    (for [k ks]
      (del (get d k)))
   d))

(defn update [d k f #* args #** kwargs]
  (setv (get d k) (f (get d k) #* args #** kwargs))
  d)

(defarity select-keys
  ([d ks]
    (dfor k ks k (get d k)))
  ([d ks default]
    (dfor k ks k (.get d k default))))

(defn merge [#* ds]
  (reduce
    (fn [acc it]
      (let [[k v] it]
        (assoc acc k v)))
    {}
    (mapcat dict.items ds)))

(defn coll? [x]
  (and (isinstance x abc.Iterable)
       (not (isinstance x #(str bytes)))))

(defn flatten [iterable]
  (for [i iterable]
    (if-not (coll? i)
      (yield i)
      (yield-from (flatten i)))))

(defn group-by [f iterable]
  (reduce
    (fn [acc it]
      (update acc (f it) conj it))
    (collections.defaultdict list)
    iterable))

;;; seq

(defclass [(dataclasses.dataclass :init False :slots True)] seq []
  #^ typing.Any data
  #^ bool stopped
  #^ bool resolved

  (defn __init__ [self iterable]
    (setv self.data (iter iterable)
          self.stopped False
          self.resolved False))

  (defn __iter__ [self]
    (loop [self self]
      (.ensure-resolved self)
      (if self.stopped
        (throw StopIteration)
        (do
          (yield (get self.data 0))
          (recur (get self.data 1))))))

  (defn [classmethod] cast [cls iterable]
    (if (isinstance iterable cls)
      iterable
      (cls iterable)))

  (defn resolve [self]
    (try
      (setv self.data #((next self.data) (seq self.data)))
      (except [StopIteration]
        (setv self.stopped True)))
    (setv self.resolved True))

  (defn ensure-resolved [self]
    (when-not self.resolved
      (.resolve self)))

  (defn [property] succeed? [self]
    (and self.resolved (not self.stopped)))

  (defn peek [self]
    (loop [self self resolved []]
      (if-not self.succeed?
        #(resolved self)
        (recur (get self.data 1) (conj resolved (get self.data 0))))))

  (defn detach [self]
    (assert (not self.succeed?))
    self.data)

  (defn detach-all [self]
    (let [[resolved self] (.peek self)]
      (if self.stopped
        resolved
        (if-not resolved
          self.data
          (concat resolved self.data)))))

  (defn [property] first [self]
    (.ensure-resolved self)
    (when-not self.stopped
      (get self.data 0)))

  (defn [property] rest [self]
    (.ensure-resolved self)
    (when-not self.stopped
      (get self.data 1)))

  (defn [property] empty? [self]
    (.ensure-resolved self)
    self.stopped))

(defn take [n s]
  (assert (>= n 0))
  (loop [s (seq.cast s) n n]
    (when-not (or (<= n 0) s.empty?)
      (yield s.first)
      (recur s.rest (dec n)))))

(defn drop [n s]
  (assert (>= n 0))
  (loop [s (seq.cast s) n n]
    (if (or (<= n 0) s.empty?)
      s
      (recur s.rest (dec n)))))

(defn split-at [n s]
  (assert (>= n 0))
  (loop [s (seq.cast s) acc [] n n]
    (if (or (<= n 0) s.empty?)
      #(acc s)
      (recur s.rest (conj acc s.first) (dec n)))))

(defn take-while [pred s]
  (loop [s (seq.cast s)]
    (when-not (or s.empty? (not (pred s.first)))
      (yield s.first)
      (recur s.rest))))

(defn drop-while [pred s]
  (loop [s (seq.cast s)]
    (if (or s.empty? (not (pred s.first)))
      s
      (recur s.rest))))

(defn split-with [pred s]
  (loop [s (seq.cast s) acc []]
    (if (or s.empty? (not (pred s.first)))
      #(acc s)
      (recur s.rest (conj acc s.first)))))

(defn window [s]
  (loop [s (seq.cast s)]
    (when-not s.empty?
      (yield s)
      (recur s.rest))))

(defn sized-window [n s]
  (assert (> n 1))
  (let [s (seq.cast s)]
    (loop [head s tail (drop (dec n) s)]
      (when-not tail.empty?
        (yield head)
        (recur head.rest tail.rest)))))

(defn sized-window-all [n s]
  (assert (> n 0))
  (let [s (seq.cast s)]
    (loop [head s tail (drop n s)]
      (yield head)
      (when-not tail.empty?
        (recur head.rest tail.rest)))))

(defn take-last [n s]
  (assert (>= n 0))
  (last (sized-window-all n s)))

(defn drop-last [n s]
  (assert (>= n 0))
  (map first (sized-window (inc n) s)))

(defn take-nth [step iterable]
  (assert (>= step 1))
  (->> (enumerate iterable)
       (filter
         (fn [it]
           (let [[i x] it]
             (zero? (% i step)))))
       (map
         (fn [it]
           (let [[i x] it]
             x)))))

(defarity partition
  ([n s]
    (partition n n s))
  ([n step s]
    (assert (and (>= n 1) (>= step 1)))
   (map
     (fn [s] (list (take n s)))
     (take-nth step (sized-window n s)))))

(defarity partition-all
  ([n s]
    (partition-all n n s))
  ([n step s]
    (assert (and (>= n 1) (>= step 1)))
   (map
     (fn [s] (list (take n s)))
     (take-nth step (window s)))))

(defn partition-by [f s]
  (loop [s (seq.cast s) acc [] g (object)]
    (if s.empty?
      (when acc
        (yield acc))
      (let [ng (f s.first)]
        (when (and (!= ng g) acc)
          (yield acc)
          (setv acc []))
        (recur s.rest (conj acc s.first) ng)))))

;;; export

(export
  :objects [builtins types collections abc typing dataclasses
            itertools functools operator contextlib
            none? some? inc dec zero? pos? neg? even? odd?
            ignore constantly identity comp partial curry complement
            reduce remove cat concat cons mapcat
            first rest empty? second last butlast
            iterate repeat repeatedly cycle interleave interpose
            conj into assoc dissoc update select-keys merge coll? flatten group-by
            seq take drop split-at take-while drop-while split-with
            window sized-window sized-window-all take-last drop-last
            take-nth partition partition-all partition-by]
  :macros [comment ignore if-not when-not def farity defarity -> ->> doto cond-> cond->> loop])
