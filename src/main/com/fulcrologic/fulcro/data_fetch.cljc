(ns com.fulcrologic.fulcro.data-fetch
  (:refer-clojure :exclude [load])
  (:require
    [clojure.walk :refer [walk prewalk]]
    [com.fulcrologic.fulcro.algorithms.data-targeting :as targeting]
    [com.fulcrologic.fulcro.algorithms.misc :as util]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.data-fetch-impl :as impl]
    [com.fulcrologic.fulcro.mutations :as m]
    [edn-query-language.core :as eql]
    [taoensso.timbre :as log]
    [com.fulcrologic.fulcro.algorithms.application-helpers :as ah]))

(declare load load-action load-field load-field-action)

(defn data-state? [state] (contains? state :status))
(defn ready? [state] (= :loading (:status state)))
(defn loading? [state] (= :loading (:status state)))
(defn failed? [state] (= :failed (:status state)))

(def marker-table
  "The name of the table in which fulcro load markers are stored"
  impl/marker-table)

(defn multiple-targets [& targets]
  (apply targeting/multiple-targets targets))

(defn prepend-to [target]
  (targeting/prepend-to target))

(defn append-to [target]
  (targeting/append-to target))

(defn replace-at [target]
  (targeting/replace-at target))

(defn elide-query-nodes
  "Remove items from a query when the query element where the (node-predicate key) returns true. Commonly used with
   a set as a predicate to elide specific well-known UI-only paths."
  [query node-predicate]
  (-> query eql/query->ast (util/elide-ast-nodes node-predicate) eql/ast->query))

(defn load-params*
  "Internal function to validate and process the parameters of `load` and `load-action`."
  [state-map server-property-or-ident class-or-factory {:keys [target params marker post-mutation post-mutation-params without
                                                               fallback focus ok-action post-action error-action remote
                                                               abort-id update-query]
                                                        :or   {remote :remote marker false}}]
  {:pre [(or (nil? target) (vector? target))
         (or (nil? post-mutation) (symbol? post-mutation))
         (or (nil? post-mutation-params) (map? post-mutation-params))
         (or (nil? params) (map? params))
         (or (eql/ident? server-property-or-ident) (keyword? server-property-or-ident))]}
  (let [query' (if class-or-factory
                 (cond-> (comp/get-query class-or-factory state-map)
                   (set? without) (elide-query-nodes without)
                   focus (eql/focus-subquery focus)
                   update-query update-query)
                 nil)
        query  (cond
                 (and class-or-factory (map? params)) `[({~server-property-or-ident ~query'} ~params)]
                 class-or-factory [{server-property-or-ident query'}]
                 (map? params) [(list server-property-or-ident params)]
                 :else [server-property-or-ident])
        marker (if (true? marker)
                 (do
                   (log/warn "Boolean load marker no longer supported.")
                   false)
                 marker)]
    {:query                query
     :source-key           server-property-or-ident
     :remote               remote
     :target               target
     :ok-action            ok-action
     :error-action         error-action
     :post-action          post-action
     :post-mutation        post-mutation
     :post-mutation-params post-mutation-params
     :fallback             fallback
     :marker               marker
     :abort-id             abort-id}))

(defn set-load-marker! [app marker status]
  (when marker
    (let [{:keys [::app/state-atom]} app]
      (when marker
        (log/debug "Setting load marker")
        (swap! state-atom assoc-in [marker-table marker] {:status status})))))

(defn remove-load-marker! [app marker]
  (when marker
    (let [{:keys [::app/state-atom]} app]
      (when marker
        (log/debug "Removing load marker")
        (swap! state-atom update marker-table dissoc marker)))))

(defn finish-load! [{:keys [app result] :as env} {:keys [query ok-action post-mutation post-mutation-params
                                                         post-action target marker source-key]}]
  (when (or (nil? app) (nil? result))
    (throw (ex-info "Load is missing app or result" {})))

  (if (fn? ok-action)
    (do
      (log/debug "Skipping default merge and calling user-supplied ok-action.")
      (ok-action env))
    (let [{:keys [body]} result
          {:keys [::app/state-atom]} app]
      (log/debug "Doing merge and targeting steps")
      (swap! state-atom (fn [s] (cond-> (merge/merge* s body query)
                                  target (targeting/process-target source-key target))))
      (remove-load-marker! app marker)
      (when (symbol? post-mutation)
        (log/debug "Doing post mutation " post-mutation)
        (comp/transact! app `[(~post-mutation ~(or post-mutation-params {}))]))
      (when (fn? post-action)
        (log/debug "Doing post action ")
        (post-action env)))))

(defn load-failed! [{:keys [app] :as env} {:keys [error-action fallback] :as params}]
  (log/debug "Running load failure logic.")
  (when (fn? error-action)
    (error-action env))
  (when (symbol? fallback)
    (comp/transact! app `[(fallback ~(assoc env :load-params params))])))

(defmethod m/mutate `load! [{:keys [query remote marker] :as params}]
  (let [remote-key (or remote :remote)]
    (log/debug "Loading " remote " query:" query)
    (cond-> {:action        (fn [{:keys [app]}] (set-load-marker! app marker :loading))
             :result-action (fn [{:keys [result app] :as env}]
                              (let [load-error? (ah/app-algorithm app :load-error?)]
                                (if (load-error? result)
                                  (load-failed! env params)
                                  (finish-load! env params))))
             remote-key     (fn [_]
                              (eql/query->ast query))})))

(defn load!
  "Load data from the server.

  This function triggers a server interaction and normalizes the server response into your app state database. During
  operation it also adds (by default) fetch markers into the app state so you can show busy indicators on the UI
  components that are waiting for data. The `:target` parameter can be used to place the data somewhere besides app
  state root (which is the default).

  The server will receive a query of the form: [({server-property (comp/get-query class-or-factory)} params)], which
  a Fulcro parser will correctly parse as a join on server-property with the given subquery and params. See the AST and
  instructions on parsing queries in the developer's guide.

  Parameters:
  - `app-or-comp` : A component instance, Fulcro application, or reconciler
  - `server-property-or-ident` : A keyword or ident that represents the root of the query to send to the server. If this is an ident
  you are loading a specific entity from the database into a local app db table. A custom target will be ignored.
  - `class-or-factory` : A component that implements IQuery, or a factory for it (if using dynamic queries). This will be combined with `server-property` into a join for the server query. Needed to normalize results.
    class-or-factory can be nil, in which case the resulting server query will not be a join.
  - `config` : A map of load configuration parameters.

  Config (all optional):
  - `target` - An assoc-in path at which to put the result of the Subquery (as an edge (normalized) or value (not normalized)).
    Can also be special targets (multiple-targets, append-to,
    prepend-to, or replace-at). If you are loading by keyword (into root), then this relocates the result (ident or value) after load.
    When loading an entity (by ident), then this option will place additional idents at the target path(s) that point to that entity.
  - `initialize` - REMOVED. Use component pre-merge instead.
  - `remote` - Optional. Keyword name of the remote that this load should come from.
  - `params` - Optional parameters to add to the generated query
  - `marker` - ID of marker. Normalizes a load marker into app state so you can see progress.
  - `refresh` - REMOVED. Not needed.
  - `parallel` - REMOVED. Implemented as automatic network combining.
  - `post-mutation` - DEPRECATED. use post-action. A mutation (symbol) to run after the data is merged. Note, if target is supplied be sure your post mutation
  should expect the data at the targeted location. The `env` of that mutation will be the env of the load (if available), but will also include `:load-request`.
  - `post-mutation-params` - An optional map  that will be passed to the post-mutation when it is called. May only contain raw data, not code!
  - `ok-action` - WARNING: OVERRIDES ALL DEFAULT OK BEHAVIOR (post-mutation, merge, clearing load marker)! A lambda that will receive the remote result in a mutation env parameter `(fn [env] ...)`.
  - `post-action` - A lambda that will receive the remote result in a mutation env parameter `(fn [env] ...)`. Called after success, like post-mutation,
  but as a lambda.
  - `error-action` - A lambda that will receive the errant result (bad server status code/body) in a mutation env parameter `(fn [env] ...)`. Like the error-action section
  of mutations.
  - `fallback` - A mutation (symbol) to run if there is a server/network error. The `env` of the fallback will be like a mutation `env`, and will
  include a `:result` key with the real result from the server.
  - `update-query` - A optional function that can transform the component query before sending to remote.
      For example, to focus a subquery using update-query:
          {:update-query #(eql/focus-subquery % [:my {:sub [:query]}])}

      Removing properties (like previous :without option):
          {:update-query #(df/elide-query-nodes % #{:my :elisions})}
  - `focus` - Focus the query along a path. See eql/focus-subquery.
  - `without` - A set of keys to remove (recursively) from the query.
  - `abort-id` - TODO. Normally use txn id, which is returned by this function.

  Returns a transaction ID, which can be used with abort
  "
  ([app-or-comp server-property-or-ident class-or-factory] (load! app-or-comp server-property-or-ident class-or-factory {}))
  ([app-or-comp server-property-or-ident class-or-factory config]
   (let [app           (comp/any->app app-or-comp)
         {:keys [load-marker-default query-transform-default]} (-> app :config)
         config        (merge
                         (cond-> {:marker load-marker-default :parallel false :refresh [] :without #{}}
                           query-transform-default (assoc :update-query query-transform-default))
                         config)
         state-map     (app/current-state app)
         mutation-args (load-params* state-map server-property-or-ident class-or-factory config)]
     (comp/transact! app `[(load! ~mutation-args)]))))

(defn load-field!
  "Load a field of the current component. Runs `prim/transact!`.

  Parameters
  - `component`: The component (**instance**, not class). This component MUST have an Ident.
  - `field`: A field on the component's query that you wish to load.
  - `parameters` : A map of: (will also accept as named parameters)

    See `load`

  WARNING: If you're using dynamic queries, you won't really know what factory your parent is using,
  nor can you pass it as a parameter to this function. Therefore, it is not recommended to use load-field from within
  a component that has a dynamic query unless you can base it on the original static query.
  "
  [component field & params]
  (let [params       (if (map? (first params)) (first params) params)
        app          (comp/any->app component)
        {:keys [params update-query]
         :or   {;; TASK: restore query-transform-default
                #_#_update-query query-transform-default}} params
        state-map    (app/current-state app)
        ident        (comp/get-ident component)
        update-query (fn [q]
                       (cond-> (eql/focus-subquery q [field])
                         update-query (update-query)))
        params       (load-params* state-map ident component (assoc params
                                                               :update-query update-query
                                                               :source-key (comp/get-ident component)))]
    (comp/transact! app [(list `load! params)])))

