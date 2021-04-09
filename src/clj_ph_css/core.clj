(ns clj-ph-css.core
  (:gen-class)
  (:require
   [clojure.core.protocols :as p]
   [clojure.java.io        :as i]
   [clojure.string         :as s])

  (:import
   [com.helger.css.reader
    CSSReader]

   [com.helger.css.writer
    CSSWriter
    CSSWriterSettings]

   [com.helger.css
    ECSSVersion]


   [com.helger.css.decl
    CascadingStyleSheet

    CSSDeclaration
    CSSExpression


    CSSSelector
    CSSSelectorMemberNot
    CSSSelectorAttribute
    ECSSAttributeOperator
    ECSSSelectorCombinator
    CSSSelectorSimpleMember
    CSSSelectorMemberFunctionLike

    CSSStyleRule

    CSSMediaRule
    CSSMediaQuery
    CSSMediaQuery$EModifier
    CSSMediaExpression

    CSSPageRule
    CSSImportRule
    CSSFontFaceRule
    CSSViewportRule
    CSSNamespaceRule

    CSSSupportsRule
    CSSSupportsConditionNested
    CSSSupportsConditionNegation
    CSSSupportsConditionDeclaration
    ECSSSupportsConditionOperator

    CSSKeyframesRule
    CSSKeyframesBlock]))


(defmulti object :type)


(defmacro deftransform
  [classname identifier schema-constructor object-constructor]
  `(do
     (extend-type ~classname
       p/Datafiable
       (datafy [object#]
         (merge (~schema-constructor object#)
                {:type ~identifier})))

     (defmethod object ~identifier [schema#]
       (~object-constructor schema#))))


(deftransform CSSDeclaration :declaration
  (fn [^CSSDeclaration o]
    {:property   (-> o .getProperty)
     :expression (-> o .getExpressionAsCSSString)
     :important? (-> o .isImportant)})
  (fn [s]
    (CSSDeclaration.
     (-> s :property)
     (-> s :expression CSSExpression/createSimple)
     (-> s :important?))))


(deftransform ECSSSelectorCombinator :selector-combinator
  (fn [^ECSSSelectorCombinator o]
    {:name (-> o .getName)})
  (fn [s]
    (ECSSSelectorCombinator/getFromNameOrNull
     (:name s))))


(deftransform ECSSAttributeOperator :attribute-operator
  (fn [^ECSSAttributeOperator o]
    {:name (-> o .getName)})
  (fn [s]
    (ECSSAttributeOperator/getFromNameOrNull
     (:name s))))


(deftransform CSSSelectorAttribute :selector-attribute
  (fn [^CSSSelectorAttribute o]
    {:name      (-> o .getAttrName)
     :operator  (-> o .getOperator p/datafy)
     :namespace (-> o .getNamespacePrefix)
     :attribute (-> o .getAttrValue)})
  (fn [s]
    (if (-> s :operator :name)
      (CSSSelectorAttribute.
       (-> s :namespace)
       (-> s :name)
       (-> s :operator object)
       (-> s :attribute))
      (CSSSelectorAttribute.
       (-> s :namespace)
       (-> s :name)))))


(deftransform CSSSelectorMemberNot :member-not
  (fn [^CSSSelectorMemberNot o]
    {:selectors (->> o .getAllSelectors (map p/datafy))})
  (fn [s]
    (CSSSelectorMemberNot.
     (->> s :selectors (map object)))))


(deftransform CSSSelectorMemberFunctionLike :member-function
  (fn [^CSSSelectorMemberFunctionLike o]
    {:name       (-> o .getFunctionName)
     :expression (-> o .getParameterExpression .getAsCSSString)})
  (fn [s]
    (CSSSelectorMemberFunctionLike.
     (-> s :name)
     (-> s :expression CSSExpression/createSimple))))


(deftransform CSSSelectorSimpleMember :member-simple
  (fn [^CSSSelectorSimpleMember o]
    {:value (.getValue o)
     :group (cond
              (.isClass  o) :class
              (.isHash   o) :identifier
              (.isPseudo o) :pseudo
              :else         :type)})
  (fn [s]
    (CSSSelectorSimpleMember.
     (:value s))))


(deftransform CSSSelector :selector
  (fn [^CSSSelector o]
    {:members (->> o .getAllMembers (map p/datafy))})
  (fn [s]
    (let [o (CSSSelector.)]
      (doseq [member (:members s)]
        (.addMember o (object member)))
      o)))


(deftransform CSSStyleRule :style-rule
  (fn [^CSSStyleRule o]
    {:selectors    (->> o .getAllSelectors    (map p/datafy))
     :declarations (->> o .getAllDeclarations (map p/datafy))})
  (fn [s]
    (let [o (CSSStyleRule.)]
      (doseq [selector (:selectors s)]
        (.addSelector o (object selector)))
      (doseq [declaration (:declarations s)]
        (.addDeclaration o (object declaration)))
      o)))

(deftransform CSSMediaExpression :media-expression
  (fn [^CSSMediaExpression o]
    {:value   (-> o .getValue .getAsCSSString)
     :feature (-> o .getFeature)})
  (fn [s]
    (CSSMediaExpression.
     (-> s :feature)
     (-> s :value CSSExpression/createSimple))))


(deftransform CSSMediaRule :media-rule
  (fn [^CSSMediaRule o]
    {:rules   (->> o .getAllRules        (map p/datafy))
     :queries (->> o .getAllMediaQueries (map p/datafy))})
  (fn [s]
    (let [o (CSSMediaRule.)]
      (doseq [query (:queries s)]
        (.addMediaQuery o (object query)))
      (doseq [rule (:rules s)]
        (.addRule o (object rule)))
      o)))


(deftransform CSSMediaQuery :media-query
  (fn [^CSSMediaQuery o]
    {:not?        (->  o .isNot)
     :only?       (->  o .isOnly)
     :medium      (->  o .getMedium)
     :expressions (->> o .getAllMediaExpressions (map p/datafy))})
  (fn [s]
    (let [o
          (CSSMediaQuery.
           (cond
             (:only? s) CSSMediaQuery$EModifier/ONLY
             (:not?  s) CSSMediaQuery$EModifier/NOT
             :else      CSSMediaQuery$EModifier/NONE)
           (:medium s))]
      (doseq [expression (:expressions s)]
        (.addMediaExpression o (object expression)))
      o)))


(deftransform CSSKeyframesRule :keyframes-rule
  (fn [^CSSKeyframesRule o]
    {:declaration (->  o .getDeclaration)
     :name        (->  o .getAnimationName)
     :blocks      (->> o .getAllBlocks (map p/datafy))})
  (fn [s]
    (let [o
          (CSSKeyframesRule. (:declaration s) (:name s))]
      (doseq [block (:blocks s)]
        (.addBlock o (object block)))
      o)))


(deftransform CSSKeyframesBlock :keyframes-block
  (fn [^CSSKeyframesBlock o]
    {:selectors    (->  o .getAllKeyframesSelectors)
     :declarations (->> o .getAllDeclarations (map p/datafy))})
  (fn [s]
    (let [o (CSSKeyframesBlock. (:selectors s))]
      (doseq [declaration (:declarations s)]
        (.addDeclaration o (object declaration)))
      o)))


(deftransform CSSFontFaceRule :font-face-rule
  (fn [^CSSFontFaceRule o]
    {:name         (->  o .getDeclaration)
     :declarations (->> o .getAllDeclarations (map p/datafy))})
  (fn [s]
    (let [o (CSSFontFaceRule. (:name s))]
      (doseq [declaration (:declarations s)]
        (.addDeclaration o (object declaration)))
      o)))


(deftransform CSSImportRule :import-rule
  (fn [^CSSImportRule o]
    {:location (->  o .getLocationString)
     :queries  (->> o .getAllMediaQueries (map p/datafy))})
  (fn [s]
    (let [o (CSSImportRule. (:location s))]
      (doseq [query (:queries s)]
        (.addMediaQuery o (object query)))
      o)))


(deftransform CSSNamespaceRule :namespace-rule
  (fn [^CSSNamespaceRule o]
    {:prefix (-> o .getNamespacePrefix)
     :url    (-> o .getNamespaceURL)})
  (fn [s]
    (if (:prefix s)
      (CSSNamespaceRule.
       (:prefix s)
       (:url s))
      (CSSNamespaceRule.
       (:url s)))))


(deftransform CSSViewportRule :viewport-rule
  (fn [^CSSViewportRule o]
    {:name         (->  o .getDeclaration)
     :declarations (->> o .getAllDeclarations (map p/datafy))})
  (fn [s]
    (let [o (CSSViewportRule. (:name s))]
      (doseq [declaration (:declarations s)]
        (.addDeclaration o (object declaration)))
      o)))


(deftransform CSSPageRule :page-rule
  (fn [^CSSPageRule o]
    {:selectors    (->  o .getAllSelectors)
     :declarations (->> o .getAllMembers (map p/datafy))})
  (fn [s]
    (let [o (CSSPageRule. (:selectors s))]
      (doseq [declaration (:declarations s)]
        (.addMember o (object declaration)))
      o)))



(deftransform CSSSupportsConditionDeclaration :condition-declaration
  (fn [^CSSSupportsConditionDeclaration o]
    {:declaration (-> o .getDeclaration p/datafy)})
  (fn [s]
    (CSSSupportsConditionDeclaration.
     (-> s :declaration object))))


(deftransform CSSSupportsConditionNegation :condition-negation
  (fn [^CSSSupportsConditionNegation o]
    {:member (-> o .getSupportsMember p/datafy)})
  (fn [s]
    (CSSSupportsConditionNegation.
     (-> s :member object))))


(deftransform CSSSupportsRule :support-rule
  (fn [^CSSSupportsRule o]
    {:members (->> o .getAllSupportConditionMembers (map p/datafy))
     :rules   (->> o .getAllRules (map p/datafy))})
  (fn [s]
    (let [o (CSSSupportsRule.)]
      (doseq [member (:members s)]
        (.addSupportConditionMember o (object member)))
      (doseq [rule (:rules s)]
        (.addRule o (object rule)))
      o)))


(deftransform ECSSSupportsConditionOperator :condition-operator
  (fn [^ECSSSupportsConditionOperator o]
    {:name (-> o .getName)})
  (fn [s]
    (ECSSSupportsConditionOperator/getFromNameCaseInsensitiveOrNull
     (:name s))))


(deftransform CSSSupportsConditionNested :condition-nested
  (fn [^CSSSupportsConditionNested o]
    {:members (->> o .getAllMembers (map p/datafy))})
  (fn [s]
    (let [o (CSSSupportsConditionNested.)]
      (doseq [member (:members s)]
        (.addMember o (object member)))
      o)))


(defn string->schema
  [^String value]
  (let [o (CSSReader/readFromString value ECSSVersion/CSS30)]
    (mapv p/datafy
          (concat
           (.getAllRules          o)
           (.getAllImportRules    o)
           (.getAllNamespaceRules o)))))


(defn schema->string
  [schema]
  (let [cascading* (CascadingStyleSheet.)
        settings*  (CSSWriterSettings. ECSSVersion/CSS30 true)
        writer*    (doto (CSSWriter. settings*)
                     (.setWriteHeaderText false))]
    (doseq [node schema]
      (case (:type node)
        :import-rule    (.addImportRule    cascading* (object node))
        :namespace-rule (.addNamespaceRule cascading* (object node))
        (.addRule cascading* (object node))))
    (.getCSSAsString writer* cascading*)))
