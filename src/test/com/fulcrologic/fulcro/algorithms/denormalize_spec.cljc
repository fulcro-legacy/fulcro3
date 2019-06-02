(ns com.fulcrologic.fulcro.algorithms.denormalize-spec
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [is are deftest]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as props]
            [com.fulcrologic.fulcro.algorithms.normalize :refer [tree->db]]
            [com.fulcrologic.fulcro.algorithms.denormalize :as denorm]
            [com.fulcrologic.fulcro.components :as comp]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.test :as ptest]
            [edn-query-language.core :as eql]
            [com.fulcrologic.fulcro.algorithms.legacy-db-tree :as fp]
            [fulcro-spec.core :refer [specification behavior assertions provided component when-mocking]]
            [fulcro-spec.diff :as diff]))

;; simple

(defn verify-db->tree [query entity db]
  (let [new-impl (denorm/db->tree query entity db)
        old-impl (fp/db->tree query entity db)]
    (assert (= new-impl old-impl) {:new-impl new-impl
                                   :old-impl old-impl
                                   :diff     (diff/diff old-impl new-impl)})
    new-impl))

(specification "db->tree"
  (assertions
    "simple cases"
    (verify-db->tree [] {} {}) => {}
    (verify-db->tree [:foo] {} {}) => {}
    (verify-db->tree [:foo] {:foo "bar"} {}) => {:foo "bar"}
    (verify-db->tree [:foo] {:foo "bar" :more "data"} {}) => {:foo "bar"}

    "joins"
    (verify-db->tree [{:foo [:bar]}]
      {:foo {:bar "baz" :extra "data"}} {})
    => {:foo {:bar "baz"}}

    (verify-db->tree [{:foo [:bar]}]
      {:foo [:point 123]}
      {:point {123 {:bar "baz" :extra "data"}}})
    => {:foo {:bar "baz"}}

    "join to many"
    (verify-db->tree [{:foo [:x]}]
      {:foo [[:x 1] [:x 2] [:x 3]]}
      {:x {1 {:x 1} 2 {:x 2} 3 {:x 3}}})
    => {:foo [{:x 1} {:x 2} {:x 3}]}

    "unions"
    (verify-db->tree
      [{:j {:a [:a :b]
            :c [:c :d]
            :e [:e :f]}}]
      {:j [:c 2]}
      {:a {1 {:a 1 :b "b"}}
       :c {2 {:c 2 :d "d"}}
       :e {3 {:e 3 :f "f"}}})
    => {:j {:c 2, :d "d"}}

    (verify-db->tree
      [{:j {:a [:a :b]
            :c [:c :d]
            :e [:e :f]}}]
      {:j [[:e 3]
           [:c 2]]}
      {:a {1 {:a 1 :b "b"}}
       :c {2 {:c 2 :d "d"}}
       :e {3 {:e 3 :f "f"}}})
    => {:j [{:e 3, :f "f"} {:c 2, :d "d"}]}

    "clean ident get"
    (verify-db->tree [[:point 123]]
      {:foo [:point 123]}
      {})
    => {}

    (verify-db->tree [[:point 123]]
      {:foo [:point 123]}
      {:point {123 {:bar "baz" :extra "data"}}})
    => {[:point 123] {:bar "baz", :extra "data"}}

    (verify-db->tree [{:entry [[:point 123]]}]
      {:entry {:data "foo"}}
      {:point {123 {:bar "baz" :extra "data"}}})
    => {:entry {[:point 123] {:bar "baz", :extra "data"}}}

    "ident join"
    (verify-db->tree [{[:point 123] [:bar]}]
      {:entry {:data "foo"}}
      {:point {123 {:bar "baz" :extra "data"}}})
    => {[:point 123] {:bar "baz"}}

    "recursion"
    (verify-db->tree '[{:entry [:message {:parent ...}]}]
      {:entry {:id 1 :message "foo" :parent [:entry 2]}}
      {:entry {1 {:id 1 :message "foo" :parent [:entry 2]}
               2 {:id 2 :message "foo" :parent [:entry 3]}
               3 {:id 3 :message "foo"}}})
    => {:entry {:message "foo", :parent {:message "foo", :parent {:message "foo"}}}}

    (verify-db->tree '[{:entry [:message {:parent ...}]}]
      {:entry {:id 1 :message "foo" :parent [:entry 2]}}
      {:entry {1 {:id 1 :message "foo" :parent [:entry 2]}
               2 {:id 2 :message "foo" :parent [:entry 3]}
               3 {:id 3 :message "foo" :parent nil}}})
    => {:entry {:message "foo", :parent {:message "foo", :parent {:message "foo"}}}}

    (verify-db->tree '[{:entry [:message {:parent 1}]}]
      {:entry {:id 1 :message "foo" :parent [:entry 2]}}
      {:entry {1 {:id 1 :message "foo" :parent [:entry 2]}
               2 {:id 2 :message "foo" :parent [:entry 3]}
               3 {:id 3 :message "foo"}}})
    => {:entry {:message "foo", :parent {:message "foo"}}}

    "wildcard"
    (verify-db->tree
      ['*]
      {:foo [:point 123] :x 42}
      {:point {123 {:bar "baz" :extra "data"}}})
    => {:foo [:point 123], :x 42}

    (denorm/db->tree
      [{:foo [:bar]} '*]
      {:foo [:point 123] :x 42}
      {:point {123 {:bar "baz" :extra "data"}}})
    => {:foo {:bar "baz"} :x 42}

    (denorm/db->tree [[:point 123] '*]
      {:foo [:point 123]}
      {:point {123 {:bar "baz" :extra "data"}}})
    => {:foo         [:point 123]
        [:point 123] {:bar "baz", :extra "data"}}))

;; generative

(defn fake-ident [ident-fn]
  (comp/configure-component! (fn [_]) ::FakeComponent {:ident ident-fn}))

(defn first-ident [this props]
  (vec (first props)))

(defn inject-components [query]
  (eql/ast->query
    (p/transduce-children
      (map (fn [{:keys [key type query] :as node}]
             (if (and (= :join type)
                      (or (map? query)
                          (not (ptest/hash-mod? key 10))))
               (assoc node :component (fake-ident first-ident))
               node)))
      (eql/query->ast query))))

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
  (tc/quick-check 50 (valid-db-tree-props)))

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

; broke: [{:!-n1/y!PN [{:A/A [:A/B]}]}]
(defn valid-db-tree-join-with-links []
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
    (let [query' (inject-components query)
          tree   (parser {} query)
          db     (tree->db query' tree true)]
      (= (denorm/db->tree query db db) (fp/db->tree query db db)))))

(comment
  (tc/quick-check 50 (valid-db-tree-join-with-links) :max-size 12))

; broke: [{:A/A {:A/A []}}]
; broke: [{:a/A {:A/A [:A/A0]}}]
; broke: [{:M/*S {:A/A []}}]
; broke: [{:A/A [{:A/A* {:A/A [:A/B]}}]}]
(defn valid-db-tree-unions []
  (props/for-all [query (eql/make-gen {::eql/gen-query-expr
                                       (fn gen-query-expr [{::eql/keys [gen-property gen-join]
                                                            :as        env}]
                                         (gen/frequency [[20 (gen-property env)]
                                                         [6 (gen-join env)]]))

                                       ::eql/gen-join-key
                                       (fn gen-join-key [{::eql/keys [gen-property] :as env}]
                                         (gen-property env))

                                       ::eql/gen-join-query
                                       (fn gen-join-query [{::eql/keys [gen-query gen-union] :as env}]
                                         (gen/frequency [[2 (gen-query env)]
                                                         [1 (gen-union env)]]))}
                          ::eql/gen-query)]
    (let [query' (inject-components query)
          tree   (parser {} query)
          db     (tree->db query' tree true)]
      (= (denorm/db->tree query db db) (fp/db->tree query db db)))))

(comment
  (tc/quick-check 50 (valid-db-tree-unions) :max-size 12))

(comment


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

  (let [query    [{:foo5 {:a [:a :b :c]
                          :d [:d :e :f]
                          :g [:g :h :i]}}]
        query    (inject-components query)
        tree     (parser {} query)
        db       (tree->db query tree true)
        new-impl (denorm/db->tree query db db)
        old-impl (fp/db->tree query db db)]
    {:valid?   (= new-impl old-impl)
     :query    query
     :tree     tree
     :db       db
     :old-impl old-impl
     :new-impl new-impl
     :diff     (diff/diff old-impl new-impl)})

  (eql/query->ast [{:foo {:a [:a :b :c]
                          :d [:d :e :f]
                          :g [:g :h :i]}}])
  (parser {} [{:foo5 {:a [:a :b :c]
                      :d [:d :e :f]
                      :g [:g :h :i]}}])
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
                     (fn gen-join-query [{::eql/keys [gen-query gen-union] :as env}]
                       (gen/frequency [[2 (gen-query env)]
                                       [1 (gen-union env)]]))}
        ::eql/gen-query)))

  (eql/query->ast (inject-components [{:A/A {:A/A [:A/A]}}]))
  (let [query    [{:A/A {:A/A [:A/B]}}]
        query    (inject-components query)
        tree     (parser {} query)
        db       (tree->db query tree true)
        new-impl (denorm/db->tree query db db)
        old-impl (fp/db->tree query db db)]
    {:valid?   (= new-impl old-impl)
     :query    query
     :tree     tree
     :db       db
     :old-impl old-impl
     :new-impl new-impl
     :diff     (diff/diff old-impl new-impl)})

  (let [query    [{:A/A* {:A/A [:A/B]}}]
        query    (inject-components query)
        tree     (parser {} query)
        db       (tree->db query tree true)
        new-impl (denorm/db->tree query db db)
        old-impl (fp/db->tree query db db)]
    {:valid?   (= new-impl old-impl)
     :query    query
     :tree     tree
     :db       db
     :old-impl old-impl
     :new-impl new-impl
     :diff     (diff/diff old-impl new-impl)})


  (let [query    [{:A/A [{:A/A0 [{:A/B {:A/A [:A/A0]}}]}]}]
        query    (inject-components query)
        tree     (parser {} query)
        db       (tree->db query tree true)
        new-impl (denorm/db->tree query db db)
        old-impl (fp/db->tree query db db)]
    {:valid?   (= new-impl old-impl)
     :query    query
     :tree     tree
     :db       db
     :old-impl old-impl
     :new-impl new-impl
     :diff     (diff/diff old-impl new-impl)}))

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
