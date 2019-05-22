(ns com.fulcrologic.fulcro.algorithms.denormalize-test
  (:require [clojure.test :refer [is are deftest]]
            [com.fulcrologic.fulcro.algorithms.denormalize :as denorm]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as props]
            [clojure.test.check.clojure-test :as test]
            [clojure.spec.alpha :as s]
            [fulcro.client.primitives :as fp]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.gen :as pgen]
            [com.wsscode.pathom.test :as ptest]
            [edn-query-language.core :as eql]))


(def parser
  (p/parser
    {::p/env     {::ptest/depth-limit 10
                  ::p/reader          ptest/reader
                  ::p/union-path      ptest/union-test-path}
     ::p/plugins [p/request-cache-plugin]}))

(defn valid-db-tree-props []
  (props/for-all [query (eql/make-gen
                          {::eql/gen-query-expr
                           (fn gen-query-expr [{::eql/keys [gen-property]
                                                :as        env}]
                             (gen-property env))}
                          ::eql/gen-query)]
    (let [tree (parser {} query)]
      (= (denorm/db->tree query tree {}) (fp/db->tree query tree {})))))

#_(test/defspec generator-makes-valid-db-props {} (valid-db-tree-props))

(comment
  (find {:a nil} :o)

  (let [query    [:h/i]
        tree     (parser {} query)
        new-impl (denorm/db->tree query tree {})
        old-impl (fp/db->tree query tree {})]
    {:valid?   (= new-impl old-impl)
     :query    query
     :tree     tree
     :new-impl new-impl
     :old-impl old-impl}))

(comment
  (tc/quick-check 50 (valid-db-tree-props) :max-size 12))

(deftest test-db->tree
  (is (= 0 1)))

(comment
  (parser {} [:foo :id :title])

  ((juxt :createdAt :name) {:createdAt 100 :name "Foo"})

  (denorm/db->tree [:b :c] {:a 1 :b 2 :c 3} {})

  (mapv
    (juxt identity (partial parser {}))
    (gen/sample (eql/make-gen {
                               ::eql/gen-query-expr
                               (fn gen-query-expr [{::eql/keys [gen-property gen-join gen-ident gen-param-expr gen-special-property gen-mutation]
                                                    :as        env}]
                                 (gen/frequency [[20 (gen-property env)]
                                                 #_#_#_#_#_[6 (gen-join env)]
                                                     [1 (gen-ident env)]
                                                     [2 (gen-param-expr env)]
                                                     [1 (gen-mutation env)]
                                                     [1 (gen-special-property env)]]))}
                  ::eql/gen-query)
      10)))
