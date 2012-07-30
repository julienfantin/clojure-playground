(ns clojure-playground.cps-macro)

(defn- doasync- [bindings forms]
  (let [[sym _] (first bindings)
        [_ expr] (second bindings)]
    `(fn [~sym]
       ~(if expr
          `(~expr ~(doasync- (next bindings) forms))
          `(do ~@forms))
       nil)))

(defmacro doasync
  "Continuation passing style let form.

Bindings => binding-form cps-expr

Destructuring is supported in `bindings`, but the cps-exprs must
evaluate to a function of one argument, the current continuation. Each
expression is responsible for applying the continuation to its result.

doasync will always return nil, thus `forms` should only be used for
side-effects.

(doasync [a (fn [c] (c 1))
          b (fn [c] (c (inc a)))]
         (println b))
; nil
;; => 2"
[bindings & forms]
(let [bindings (cons [(gensym) nil] (partition 2 bindings))]
  `(~(doasync- bindings forms) nil)))
