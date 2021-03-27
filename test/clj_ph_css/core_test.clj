(ns clj-ph-css.core-test
  (:require
   [clj-ph-css.core :as    sut]
   [clojure.test    :refer :all]))


(defmacro deftransform-match
  "schema -> string = string -> schema"
  [& args]
  (let [stylesheet (apply str (drop-last args))
        schema     (last args)]
    `(do
       (testing "schema"
         (is (= ~schema (sut/string->schema ~stylesheet))))
       (testing "object"
         (is (= ~stylesheet (sut/schema->string ~schema)))))))


(deftest transform-test

  (testing "universal selector"
    ;; https://www.w3.org/TR/2018/REC-selectors-3-20181106/#universal-selector
    (deftransform-match
      "ns|*,"
      "*|*,"
      "|*,"
      "*"
      "{}"
      [{:selectors
        [{:members
          [{:group :type :value "ns|" :type :member-simple}
           {:group :type :value "*"   :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "*|" :type :member-simple}
           {:group :type :value "*"  :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "|" :type :member-simple}
           {:group :type :value "*" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "*" :type :member-simple}]
          :type    :selector}]
        :declarations []
        :type         :style-rule}]))


  (testing "type selector"
    ;; https://www.w3.org/TR/2018/REC-selectors-3-20181106/#type-selectors
    (deftransform-match
      "A,"
      "|A,"
      "*|A,"
      "ns|A"
      "{}"
      [{:type :style-rule
        :selectors
        [{:members
          [{:group :type :value "A" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "|" :type :member-simple}
           {:group :type :value "A" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "*|" :type :member-simple}
           {:group :type :value "A"  :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "ns|" :type :member-simple}
           {:group :type :value "A"  :type :member-simple}]
          :type :selector}]
        :declarations []}]))


  (testing "attribute selectors"
    ;; https://www.w3.org/TR/2018/REC-selectors-3-20181106/#attribute-selectors
    (deftransform-match
      "[B],"
      "[B=C],"
      "[B~=C],"
      "[B^=C],"
      "[B$=C],"
      "[B*=C],"
      "[B|=C]"
      "{}"
      [{:type :style-rule
        :selectors
        [{:members
          [{:name "B"
            :operator  nil
            :namespace nil
            :attribute nil
            :type :selector-attribute}]
          :type :selector}
         {:members
          [{:name      "B"
            :namespace nil
            :operator  {:name "=" :type :attribute-operator}
            :attribute "C"
            :type      :selector-attribute}]
          :type :selector}
         {:members
          [{:name      "B"
            :namespace nil
            :operator  {:name "~=" :type :attribute-operator}
            :attribute "C"
            :type      :selector-attribute}]
          :type :selector}
         {:members
          [{:name      "B"
            :namespace nil
            :operator  {:name "^=" :type :attribute-operator}
            :attribute "C"
            :type      :selector-attribute}]
          :type :selector}
         {:members
          [{:name      "B"
            :namespace nil
            :operator  {:name "$=" :type :attribute-operator}
            :attribute "C"
            :type      :selector-attribute}]
          :type :selector}
         {:members
          [{:name      "B"
            :namespace nil
            :operator  {:name "*=" :type :attribute-operator}
            :attribute "C"
            :type      :selector-attribute}]
          :type :selector}
         {:members
          [{:name      "B"
            :namespace nil
            :operator  {:name "|=" :type :attribute-operator}
            :attribute "C"
            :type      :selector-attribute}]
          :type :selector}]
        :declarations []}]))


  (testing "pseudo-classes"
    ;; https://www.w3.org/TR/2018/REC-selectors-3-20181106/#structural-pseudos
    (deftransform-match
      ":root,"
      "::after"
      "{}"
      [{:type :style-rule
        :selectors
        [{:members
          [{:group :pseudo :value ":root" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :pseudo :value "::after" :type :member-simple}]
          :type :selector}]
        :declarations []}]))


  (testing "classes"
    ;; https://www.w3.org/TR/2018/REC-selectors-3-20181106/#class-html
    (deftransform-match
      ".A,"
      "*.A,"
      "A.B"
      "{}"
      [{:type :style-rule
        :selectors
        [{:members
          [{:group :class :value ".A" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type  :value "*"  :type :member-simple}
           {:group :class :value ".A" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type  :value "A"  :type :member-simple}
           {:group :class :value ".B" :type :member-simple}]
          :type :selector}]
        :declarations []}]))


  (testing "id selectors"
    ;; https://www.w3.org/TR/2018/REC-selectors-3-20181106/#id-selectors
    (deftransform-match
      "#A,"
      "A#B,"
      "*#A"
      "{}"
      [{:type :style-rule
        :selectors
        [{:members
          [{:group :identifier :value "#A" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type       :value "A"  :type :member-simple}
           {:group :identifier :value "#B" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type       :value "*"  :type :member-simple}
           {:group :identifier :value "#A" :type :member-simple}]
          :type :selector}]
        :declarations []}]))


  (testing "negation"
    ;; https://www.w3.org/TR/2018/REC-selectors-3-20181106/#negation
    (deftransform-match
      ":not(A),"
      "A:not(B),"
      "A|B:not(C):not(D)"
      "{}"
      [{:type :style-rule
        :selectors
        [{:members
          [{:selectors
            [{:members
              [{:group :type :value "A" :type :member-simple}]
              :type :selector}]
            :type :member-not}]
          :type :selector}
         {:members
          [{:group :type :value "A" :type :member-simple}
           {:selectors
            [{:members
              [{:group :type :value "B" :type :member-simple}]
              :type :selector}]
            :type :member-not}]
          :type :selector}
         {:members
          [{:group :type :value "A|" :type :member-simple}
           {:group :type :value "B"  :type :member-simple}
           {:selectors
            [{:members
              [{:group :type :value "C" :type :member-simple}]
              :type :selector}]
            :type :member-not}
           {:selectors
            [{:members
              [{:group :type :value "D" :type :member-simple}]
              :type :selector}]
            :type :member-not}]
          :type :selector}]
        :declarations []}]))


  (testing "combinators"
    (deftransform-match
      "A B,"
      "A>B,"
      "A+B,"
      "A~B"
      "{}"
      [{:type :style-rule
        :selectors
        [{:members
          [{:group :type :value "A"  :type :member-simple}
           {:name  " "  :type :selector-combinator}
           {:group :type :value "B"  :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "A"  :type :member-simple}
           {:name  ">"  :type :selector-combinator}
           {:group :type :value "B"  :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "A" :type :member-simple}
           {:name  "+" :type :selector-combinator}
           {:group :type :value "B" :type :member-simple}]
          :type :selector}
         {:members
          [{:group :type :value "A" :type :member-simple}
           {:name  "~" :type :selector-combinator}
           {:group :type :value "B" :type :member-simple}]
          :type :selector}]
        :declarations []}]))


  (testing "functions"
    (deftransform-match
      ":lang(en){}"
      [{:selectors
        [{:members
          [{:name ":lang(" :expression "en" :type :member-function}]
          :type :selector}]
        :declarations []
        :type :style-rule}]))


  (testing "media-rule"
    (testing "query"
      (deftransform-match
        "@media screen{}"
        "@media not screen{}"
        "@media only screen{}"
        [{:rules []
          :queries
          [{:not? false
            :only? false
            :medium "screen"
            :expressions []
            :type :media-query}]
          :type :media-rule}
         {:rules []
          :queries
          [{:not? true
            :only? false
            :medium "screen"
            :expressions []
            :type :media-query}]
          :type :media-rule}
         {:rules []
          :queries
          [{:not? false
            :only? true
            :medium "screen"
            :expressions []
            :type :media-query}]
          :type :media-rule}]))

    (testing "expression"
      (deftransform-match
        "@media (min-width:100px){}"
        [{:rules []
          :queries
          [{:not?   false
            :only?  false
            :medium nil
            :expressions
            [{:value "100px"
              :feature "min-width"
              :type :media-expression}]
            :type :media-query}]
          :type :media-rule}]))

    (testing "rules"
      (deftransform-match
        "@media screen{A{}}"
        "@media screen{@media screen{A{}}}"
        [{:rules
          [{:type :style-rule
            :selectors
            [{:members
              [{:group :type :value "A" :type :member-simple}],
              :type :selector}]
            :declarations
            []}]
          :queries
          [{:not? false
            :only? false
            :medium "screen"
            :expressions
            []
            :type :media-query}]
          :type :media-rule}
         {:rules
          [{:rules
            [{:type :style-rule
              :selectors
              [{:members
                [{:group :type :value "A" :type :member-simple}]
                :type :selector}]
              :declarations
              []}]
            :queries
            [{:not? false
              :only? false
              :medium "screen"
              :expressions
              []
              :type :media-query}]
            :type :media-rule}]
          :queries
          [{:not? false
            :only? false
            :medium "screen"
            :expressions
            []
            :type :media-query}]
          :type :media-rule}])))


  (testing "keyframes rule"
    (deftransform-match
      "@keyframes foo{from{}50%{}20%,80%{}to{}}"
      [{:declaration "@keyframes"
        :name "foo"
        :blocks
        [{:type :keyframes-block :selectors ["from"] :declarations []}
         {:type :keyframes-block :selectors ["50%"] :declarations []}
         {:type :keyframes-block
          :selectors ["20%" "80%"]
          :declarations []}
         {:type :keyframes-block :selectors ["to"] :declarations []}]
        :type :keyframes-rule}]))


  (testing "font-face-rule"
    (deftransform-match 
      "@font-face{a:b}"
      [{:name "@font-face"
        :declarations
        [{:property   "a"
          :expression "b"
          :important? false
          :type       :declaration}]
        :type :font-face-rule}]))


  (testing "import rule"
    (deftransform-match
      "@import url(foo.css);\n"
      [{:location "foo.css"
        :queries  []
        :type     :import-rule}])

    (deftransform-match
      "@import url(foo.css) screen;\n"
      [{:location "foo.css"
        :queries
        [{:not?        false
          :only?       false
          :medium      "screen"
          :expressions []
          :type        :media-query}]
        :type     :import-rule}]))


  (testing "namespace rule"
    (deftransform-match
      "@namespace foo url(bar);\n"
      [{:prefix "foo" :url "bar" :type :namespace-rule}])

    (deftransform-match
      "@namespace url(bar);\n"
      [{:prefix nil :url "bar" :type :namespace-rule}]))


  (testing "viewport rule"
    (deftransform-match
      "@viewport{a:b}"
      [{:name "@viewport"
        :declarations
        [{:property "a"
          :expression "b"
          :important? false
          :type :declaration}]
        :type :viewport-rule}]))


  (testing "page rule"
    (deftransform-match
      "@page :first{a:b}"
      [{:selectors [":first"]
        :declarations
        [{:property "a"
          :expression "b"
          :important? false
          :type :declaration}]
        :type :page-rule}]))


  (testing "support rule"
    (deftransform-match
      "@supports (a:b){A{c:d}}"
      [{:members
        [{:declaration
          {:property "a"
           :expression "b"
           :important? false
           :type :declaration}
          :type :condition-declaration}]
        :rules
        [{:selectors
          [{:members
            [{:value "A" :group :type :type :member-simple}]
            :type :selector}]
          :declarations
          [{:property "c"
            :expression "d"
            :important? false
            :type :declaration}]
          :type :style-rule}]
        :type :support-rule}])

    (testing "negation"
      (deftransform-match
        "@supports not (a:b){A{c:d}}"
        [{:members	  
          [{:member
            {:declaration
             {:property   "a"
              :expression "b"
              :important? false
              :type       :declaration}
             :type :condition-declaration}
            :type :condition-negation}]
          :rules
          [{:selectors
            [{:members
              [{:value "A" :group :type :type :member-simple}]
              :type :selector}]
            :declarations
            [{:property   "c"
              :expression "d"
              :important? false
              :type       :declaration}]
            :type :style-rule}]
          :type :support-rule}]))


    (testing "nested"
      (deftransform-match
        "@supports ((a:b) or (c:d)){A{e:f}}"
        [{:members	  
          [{:members
            [{:declaration
              {:property "a"
               :expression "b"
               :important? false
               :type :declaration}
              :type :condition-declaration}
             {:name "or" :type :condition-operator}
             {:declaration
              {:property "c"
               :expression "d"
               :important? false
               :type :declaration}
              :type :condition-declaration}]
            :type :condition-nested}]
          :rules
          [{:selectors
            [{:members
              [{:value "A" :group :type :type :member-simple}]
              :type :selector}]
            :declarations
            [{:property "e"
              :expression "f"
              :important? false
              :type :declaration}]
            :type :style-rule}]
          :type :support-rule}]))))
