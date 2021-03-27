# clj-ph-css
Clojure wrapper for [ph-css](https://github.com/phax/ph-css)

[![Clojars Project](https://img.shields.io/clojars/v/clj-ph-css.svg)](https://clojars.org/clj-ph-css)

## Quickstart Tutorial ##

```clj
(use 'clj-ph-css.core)


(def schema
  (string->schema "foo bar {baz:zaz}"))
;; => #'clj-ph-css.core/schema

schema
;; =>
;; [{:selectors
;;   [{:members
;;     [{:value "foo" :group :type :type :member-simple}
;;      {:name  " " :type :selector-combinator}
;;      {:value "bar" :group :type :type :member-simple}]
;;     :type :selector}]
;;   :declarations
;;   [{:property   "baz"
;;     :expression "zaz"
;;     :important? false
;;     :type       :declaration}]
;;   :type :style-rule}]

(schema->string schema)
;;=> "foo bar{baz:zaz}"
```
