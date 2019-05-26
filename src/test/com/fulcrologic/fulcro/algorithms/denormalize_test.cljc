(ns com.fulcrologic.fulcro.algorithms.denormalize-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [is are deftest]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as test]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as props]
            [com.fulcrologic.fulcro.algorithms.normalize :refer [tree->db]]
            [com.fulcrologic.fulcro.algorithms.denormalize :as denorm]
            [com.fulcrologic.fulcro.components :as comp]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.test :as ptest]
            [edn-query-language.core :as eql]
            [fulcro.client.primitives :as fp]))

(def FakeComponent {})

(defn fake-ident [ident-fn]
  (comp/configure-component! (fn [_]) ::FakeComponent {:ident ident-fn}))

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

(comment
  (tc/quick-check 100 (valid-db-tree-props)))

#_
(test/defspec generator-makes-valid-db-props {} (valid-db-tree-props))

(defn valid-db-tree-join-no-links []
  (props/for-all [query (eql/make-gen {::eql/gen-query-expr
                                       (fn gen-query-expr [{::eql/keys [gen-property gen-join]
                                                            :as        env}]
                                         (gen/frequency [[20 (gen-property env)]
                                                         [6 (gen-join env)]]))

                                       ::eql/gen-join-key
                                       (fn gen-join-key [{::eql/keys [gen-property] :as env}]
                                         (gen-property env))

                                       ::eql/gen-join-query
                                       (fn gen-join-query [{::eql/keys [gen-query] :as env}]
                                         (gen-query env))}
                          ::eql/gen-query)]
    (let [tree (parser {} query)]
      (= (denorm/db->tree query tree {}) (fp/db->tree query tree {})))))

(comment
  (tc/quick-check 50 (valid-db-tree-join-no-links) :max-size 12))

(defn first-ident [& args]
  (println "IDENT" args)
  [:foo "bar"])

(defn inject-components [query]
  (eql/ast->query
    (p/transduce-children
      (map (fn [{:keys [key type] :as node}]
             (if (and (= :join type)
                      (not (ptest/hash-mod? key 10)))
               (assoc node :component (fake-ident first-ident))
               node)))
      (eql/query->ast query))))

(comment
  (comp/has-ident? (fake-ident first-ident))

  (def query [:O-/P_*T
              #:P!{:*O [
                        #:D.8{:.F [:z8B?/-.3! #:k1-!{:?+c [:_i2G/Z?0]} :?h-/hh :_b?/s6f]}
                        #:M3{:ZW []}
                        :mE/a4
                        :P/*_]}
              :dWr*/-
              :d?./!+E!
              :Vk/U
              :-L.!/mD7?
              :t/yNYL
              #:O*_{:C6x [:S!7/+
                          :Bj/FW!
                          :z/e+
                          :G-+X/J]}])
  (ptest/hash-mod? :D.8/.F 10)

  (let [data {:foo {:id "bar"}}
        q' [{:foo (with-meta [:id] {:component (fake-ident first-ident)})}]]
    (tree->db q' data true))

  (let [data (parser {} query)
        q' (inject-components query)]

    (tree->db q' data true))

  (eql/ast->query
   (p/transduce-children
     (map (fn [{:keys [key type query] :as node}]
            (if (and (= :join type)
                     (not (ptest/hash-mod? key 10)))
              (assoc node :component (fake-ident first-ident))
              node)))
     (eql/query->ast query)))

  (parser {} query)
  (map (partial parser {})
    (gen/sample
      (eql/make-gen {::eql/gen-query-expr
                     (fn gen-query-expr [{::eql/keys [gen-property gen-join]
                                          :as        env}]
                       (gen/frequency [[20 (gen-property env)]
                                       [6 (gen-join env)]]))

                     ::eql/gen-join-key
                     (fn gen-join-key [{::eql/keys [gen-property] :as env}]
                       (gen-property env))

                     ::eql/gen-join-query
                     (fn gen-join-query [{::eql/keys [gen-query] :as env}]
                       (gen-query env))}
        ::eql/gen-query)))

  (let [query    [{:*/A []}]
        tree     (parser {} query)
        new-impl (denorm/db->tree query tree {})
        old-impl (fp/db->tree query tree {})]
    {:valid?   (= new-impl old-impl)
     :query    query
     :tree     tree
     :new-impl new-impl
     :old-impl old-impl}))

(comment
  (parser {} [:..0/y :.?/Ys_a :DI*/p :*/qe4_ :!T/? :./!0Ed])

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
