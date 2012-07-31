(ns clojure-playground.cps-macro)

(defn- let-series- [bindings forms]
  (let [[sym _] (first bindings)
        [_ expr] (second bindings)]
    `(fn [~sym]
       ~(if expr
          `(~expr ~(let-series- (next bindings) forms))
          `(do ~@forms))
       nil)))

(defmacro let-series
  "Continuation passing style let form.

  Bindings => binding-form cps-expr

  Destructuring is supported in `bindings`, but the cps-exprs must
  evaluate to a function of one argument, the current continuation. Each
  expression is responsible for applying the continuation to its result.

  let-series will always return nil, thus `forms` should only be used for
  side-effects.

  (let-series [a (fn [c] (c 1))
               b (fn [c] (c (inc a)))]
              (println b))
  ;; nil
  ;; => 2
  "
  [bindings & forms]
  (let [bindings (cons [(gensym) nil] (partition 2 bindings))]
    `(~(let-series- bindings forms) nil)))

(defmacro let-parallel [bindings & forms]
  (let [bindings (partition 2 bindings)
        syms (map first bindings)]
    `(let [mappings# (into {} (map-indexed #(identity [%2 %1]) '~syms))
           returns# (atom {})
           cont# (fn ~(vec syms)
                   (do ~@forms))
           ~'sub-cont (fn [sym# result#]
                        (swap! returns# assoc (get mappings# sym#) result#)
                        (when (= (count @returns#) (count mappings#))
                          (apply cont#
                                 (vals (into (sorted-set) @returns#)))))]
       ~@(map (fn [[sym expr]]
                `(~expr (partial ~'sub-cont '~sym)))
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
