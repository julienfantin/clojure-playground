(ns clojure-playground.cps-macro)

(defn- doasync- [bindings forms]
  (let [[sym _] (first bindings)
        [_ expr] (second bindings)]
    `(fn [~sym]
       (do
         ~(if expr
            `(~expr ~(doasync- (next bindings) forms))
            forms)
         nil))))

(defmacro doasync
  ([bindings & forms]
     (let [bindings (cons [(gensym) nil] (partition 2 bindings))]
       `(~(doasync- bindings forms) nil))))
