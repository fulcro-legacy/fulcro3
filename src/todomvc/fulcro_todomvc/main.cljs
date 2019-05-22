(ns fulcro-todomvc.main
  (:require
    [fulcro-todomvc.ui :as ui]
    [fulcro-todomvc.server :as sapi]
    [fulcro-todomvc.api :as api]
    [clojure.core.async :as async]
    [com.fulcrologic.fulcro.algorithms.tx-processing :as txn]
    [com.fulcrologic.fulcro.application :as app]
    [com.wsscode.pathom.connect :as pc]
    [com.wsscode.pathom.core :as p]
    [taoensso.timbre :as log]
    [edn-query-language.core :as eql]
    [com.fulcrologic.fulcro.components :as comp]
    [com.fulcrologic.fulcro.algorithms.application-helpers :as ah]
    [com.fulcrologic.fulcro.rendering.keyframe-render :as kr]))

(defn handle-remote [{:keys [::txn/ast ::txn/result-handler] :as send-node}]
  (log/info "Remote got AST: " ast)
  (let [query (eql/ast->query ast)]
    (async/go
      (result-handler
        (if-let [result (async/<! (sapi/parser {} query))]
          {:status-code 200 :body result}
          {:status-code 500 :body "Parser Failed to return a value"})))))

(defonce app (-> (app/fulcro-app {:remotes {:remote handle-remote}})
               (ah/with-optimized-render kr/render!)))

(defn ^:export start []
  (log/info "mount")
  (app/mount! app ui/Root "app")
  (log/info "submit")
  (df/load app `[(api/load-list ~{:key       [:list/id 1]
                                         :component ui/TodoList})]))

(comment
  (-> app ::app/runtime-atom deref)
  (-> app ::app/state-atom deref))