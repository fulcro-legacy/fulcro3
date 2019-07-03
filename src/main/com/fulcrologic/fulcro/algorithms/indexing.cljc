(ns com.fulcrologic.fulcro.algorithms.indexing
  "Functions that implement the query and component indexing."
  (:require
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.algorithms.misc :as util]
    [edn-query-language.core :as eql]
    [taoensso.encore :as encore]
    [taoensso.timbre :as log]))

(defn- index-query*
  [prop->classes {parent-component :component
                  parent-children  :children
                  :as              ast}]
  (let [parent-key      (comp/class->registry-key parent-component)
        parent-children (seq parent-children)
        update-index    (fn [idx k c] (update idx k (fnil conj #{}) c))]
    (if parent-children
      (reduce
        (fn [idx {:keys [key dispatch-key children] :as child-ast}]
          (cond-> idx
            (and (vector? key) (= '_ (second key))) (update-index dispatch-key parent-key)
            (and (vector? key) (not= '_ (second key))) (update-index key parent-key)
            (keyword? key) (update-index key parent-key)
            (seq children) (index-query* child-ast)))
        prop->classes
        parent-children)
      prop->classes)))

(defn index-query
  "Create an index of the given component-annotated query. Returns a map from query keyword to the component
  class(es) that query for that keyword."
  [query]
  (let [ast (eql/query->ast query)]
    (index-query* {} ast)))

(defn- index-root*
  [runtime-state root-query]
  (encore/when-let [prop->classes   (index-query root-query)
                    idents-in-joins (into #{}
                                      (filter eql/ident?)
                                      (keys prop->classes))]
    (-> runtime-state
      (assoc-in [:com.fulcrologic.fulcro.application/indexes :idents-in-joins] idents-in-joins)
      (assoc-in [:com.fulcrologic.fulcro.application/indexes :prop->classes] prop->classes))))

(defn index-root!
  "Index the root query (see index-query) and side-effect the result (`prop->classes`) into the given app.
  This function assumes the `root-class` has already been supplied to the app (i.e. is has been mounted)."
  [app]
  (let [{:com.fulcrologic.fulcro.application/keys [state-atom runtime-atom]} app
        {:com.fulcrologic.fulcro.application/keys [root-class]} @runtime-atom]
    (encore/when-let [root-query (comp/get-query root-class @state-atom)]
      (log/debug "(Re)indexing application query for prop->classes")
      (swap! runtime-atom index-root* root-query))))

(defn- index-component* [runtime-state instance ident cls]
  (let [k (comp/class->registry-key cls)]
    (cond-> runtime-state
      k (update-in
          [:com.fulcrologic.fulcro.application/indexes :class->components k]
          (fnil conj #{})
          instance)
      ident (update-in
              [:com.fulcrologic.fulcro.application/indexes :ident->components ident]
              (fnil conj #{})
              instance))))

(defn index-component!
  "Add a component instance to the app index. This adds the component to the `class->components` and
   `ident->components` indexes."
  [this]
  (let [{:keys [:com.fulcrologic.fulcro.application/runtime-atom]} (comp/any->app this)
        get-ident (comp/component-options this :ident)]
    (let [ident (when get-ident (get-ident this (comp/props this)))
          cls   (comp/react-type this)]
      (when #?(:cljs goog.DEBUG :clj true)
        (when (and ident (not (eql/ident? ident)))
          (log/error "Component" (comp/component-name this) "supplied an invalid ident" ident))
        (when (and ident (nil? (second ident)))
          (log/info
            (str "component " (comp/component-name this) "'s ident (" ident ") has a `nil` second element."
              " This warning can be safely ignored if that is intended.")))
        (log/debug "Adding" (comp/component-name this) "instance to class index")
        (when ident
          (log/debug "Adding" (comp/component-name this) "with ident" ident "to ident index")))
      (swap! runtime-atom index-component* this ident cls))))

(defn- drop-component*
  [runtime-state instance ident cls]
  (let [k (comp/class->registry-key cls)]
    (cond-> (update-in runtime-state [:com.fulcrologic.fulcro.application/indexes :class->components k]
              disj instance)
      ident (update-in
              [:com.fulcrologic.fulcro.application/indexes :ident->components ident]
              disj
              instance))))

(defn drop-component!
  "Remove the component instance from the indexes. If ident is supplied it uses that, otherwise it gets the
  ident from the component itself."
  ([this ident]
   (let [{:keys [:com.fulcrologic.fulcro.application/runtime-atom]} (comp/any->app this)
         cls (comp/react-type this)]
     (log/debug "Dropping component instance with ident " ident "from indexes")
     (swap! runtime-atom drop-component* this ident cls)))
  ([this]
   (let [old-ident (comp/get-ident this)]
     (drop-component! this old-ident))))
