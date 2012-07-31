(ns clojure-playground.async)

(defn- let-series- [bindings forms]
  (let [[sym _] (first bindings)
        [_ expr] (second bindings)]
    `(fn [~sym]
       ~(if expr
          `(~expr ~(let-series- (next bindings) forms))
          `(do ~@forms))
       nil)))

(defmacro let-series
  "Serial cps let form.

  Bindings => binding-form cps-expr

  Destructuring is supported in `bindings`, but the cps-exprs must
  evaluate to functions of one argument, the current continuation. Each
  expression is responsible for applying the continuation to its result.

  `bindings` are performed sequencially as each one applies its
  continuation.

  `forms` will be evaluated in an implicit do block after
  the last continuation is applied.

  `forms` should only be used for side-effects, as let-series will
  always return nil.

  (let-series [a (fn [c] (c 1))
               b (fn [c] (c (inc a)))]
              (println b))
  ;; nil
  ;; => 2
  "
  [bindings & forms]
  (let [bindings (cons [(gensym) nil] (partition 2 bindings))]
    `(~(let-series- bindings forms) nil)))

(defn- ensure-unique-bindings [symbols]
  "Throw an Exception if symbols are not unique."
  (when-not (= (count (set symbols))
               (count symbols))
    (throw (IllegalArgumentException. "let-parallel requires unique binding-forms."))))

(defn- sym->index-map [syms]
  "Turn a seq of symbols into a map of symbols to index."
  (into {} (map-indexed #(identity [%2 %1]) syms)))

(defmacro let-parallel [bindings & forms]
  "Parallel cps let form.

  Bindings => binding-form cps-expr

  Destructuring is supported in `bindings`, but the cps-exprs must
  evaluate to functions of one argument, the current continuation. Each
  expression is responsible for applying the continuation to its result.

  `bindings` are performed concurrently, and thus cannot refer to other
  binding-forms.

  `forms` will be evaluated in an implicit do after all the
  continuations have been applied.

  `forms` should only be used for side-effects, as let-parallel will
  always return nil.

  (let-parallel [a (fn [c] (c 1))
                 b (fn [c] (c 2)))]
              (println b))
  ;; nil
  ;; => 2
  "
  (let [bindings (partition 2 bindings)
        syms (vec (map first bindings))]
    (ensure-unique-bindings syms)
    `(let [mappings# '~(sym->index-map syms)
           returns# (atom {})
           cont# (fn ~(vec syms) (do ~@forms))
           ~'sub-cont# (fn [sym# result#]
                         ;; Collect return value
                         (swap! returns# assoc (get mappings# sym#) result#)
                         ;; Evaluate main continuation if done
                         (when (= (count @returns#) (count mappings#))
                           (apply cont# (vals (into (sorted-set) @returns#)))))]
       ~@(map (fn [[sym expr]]
                `(~expr (partial ~'sub-cont# '~sym)))
              bindings)
       nil)))

(comment
  ;; let-parallel expansion
  (let [mappings {'a 0 'b 1}
        returns (atom {})]
    (let [main-continuation (fn [a b]
                              (println a b))
          sub-continuation (fn [sym result]
                             (swap! returns assoc (get mappings sym) result)
                             (when (= (count @returns) (count mappings))
                               (apply main-continuation
                                      (vals (into (sorted-set) @returns)))))]
      ((fn [c]
         (future (do
                   (Thread/sleep 2000)
                   (c 1)))) (partial sub-continuation 'a))
      ((fn [c]
         (future (do
                   (Thread/sleep 2000)
                   (c 2)))) (partial sub-continuation 'b))))

  )
