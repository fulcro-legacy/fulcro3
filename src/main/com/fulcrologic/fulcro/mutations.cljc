(ns com.fulcrologic.fulcro.mutations
  #?(:cljs (:require-macros com.fulcrologic.fulcro.mutations))
  (:require
    #?(:clj com.fulcrologic.fulcro.macros.defmutation)
    [com.fulcrologic.fulcro.components :as comp]
    [taoensso.timbre :as log])
  #?(:clj
     (:import (clojure.lang IFn))))

#?(:clj
   (deftype Mutation [sym]
     IFn
     (invoke [this]
       (this {}))
     (invoke [this args]
       (list sym args)))
   :cljs
   (deftype Mutation [sym]
     IFn
     (-invoke [this]
       (this {}))
     (-invoke [this args]
       (list sym args))))

(defn default-result-action [env]
  (let [{:keys [result handlers]} env
        {:keys [ok-action error-action]} handlers
        {:keys [status-code]} result]
    (if (= 200 status-code)
      (when ok-action
        ;...merge mutation joins...
        (ok-action env))
      (when error-action
        (error-action env)))))

(defmulti mutate (fn [env] (-> env :ast :dispatch-key)))

#?(:clj
   (defmacro
     ^{:doc
       "Define a Fulcro mutation.

     The given symbol will be prefixed with the namespace of the current namespace, and if you use a simple symbol it
     will also be def'd into a name that when used as a function will simply resolve to that function call as data:

     ```
     (defmutation f [p]
       ...)

     (f {:x 1}) => `(f {:x 1})
     ```

     This allows mutations to behave as data in transactions without needing quoting.

     Mutations can have any number of handlers. By convention things that contain logic use names that end
     in `action`.  The remote behavior of a mutation is defined by naming a handler after the remote.

     ```
     (defmutation boo
       \"docstring\" [params-map]
       (action [env] ...)
       (my-remote [env] ...)
       (other-remote [env] ...)
       (remote [env] ...))
     ```

     NOTE: Every handler in the defmutation is turned into a lambda, and that lambda will be available in `env` under
     the key `:handlers`. Thus actions and remotes can cross-call (TODO: Make the macro rewrite cross calls so they
     can look like fn calls?):

     ```
     (defmutation boo
       \"docstring\" [params-map]
       (action [env] ...)
       (ok-action [env] ...)
       (result-action [env] ((-> env :handlers :ok-action) env)))
     ```

     This macro normally adds a `:result-action` handler that does normal Fulcro mutation remote result logic unless
     you supply your own.

     Remotes in Fulcro 3 are also lambdas, and are called with an `env` that contains the state as it exists *after*
     the `:action` has run in `state`, but also include the 'before action state' as a map in `:state-before-action`.
     "
       :arglists
       '([sym docstring? arglist handlers])} defmutation
     [& args]
     (com.fulcrologic.fulcro.macros.defmutation/defmutation* &env args)))

#?(:clj
   (defmacro declare-mutation
     "Define a quote-free interface for using the given `target-symbol` in mutations.
     The declared mutation can be used in lieu of the true mutation symbol
     as a way to prevent circular references while also allowing the shorthand of ns aliasing.

     In IntelliJ, use Resolve-as `def` to get proper IDE integration."
     ([name target-symbol]
      `(def ~name (->Mutation '~target-symbol)))))

#?(:cljs
   (com.fulcrologic.fulcro.mutations/defmutation set-props
     "
     mutation: A convenience helper, generally used 'bit twiddle' the data on a particular database table (using the component's ident).
     Specifically, merge the given `params` into the state of the database object at the component's ident.
     In general, it is recommended this be used for ui-only properties that have no real use outside of the component.
     "
     [params]
     (action [{:keys [state ref]}]
       (when (nil? ref) (log/error "ui/set-props requires component to have an ident."))
       (swap! state update-in ref (fn [st] (merge st params))))))

#?(:cljs
   (com.fulcrologic.fulcro.mutations/defmutation toggle
     "mutation: A helper method that toggles the true/false nature of a component's state by ident.
      Use for local UI data only. Use your own mutations for things that have a good abstract meaning. "
     [{:keys [field]}]
     (action [{:keys [state ref]}]
       (when (nil? ref) (log/error "ui/toggle requires component to have an ident."))
       (swap! state update-in (conj ref field) not))))

(defmethod mutate :default [{:keys [target]} k _]
  (when (nil? target)
    (log/error "Unknown app state mutation. Have you required the file with your mutations?" k)))

(defn toggle!
  "Toggle the given boolean `field` on the specified component. It is recommended you use this function only on
  UI-related data (e.g. form checkbox checked status) and write clear top-level transactions for anything more complicated."
  [comp field]
  (comp/transact! comp `[(toggle {:field ~field})]))

(defn set-value!
  "Set a raw value on the given `field` of a `component`. It is recommended you use this function only on
  UI-related data (e.g. form inputs that are used by the UI, and not persisted data). Changes made via these
  helpers are compressed in the history."
  [component field value]
  (comp/transact! component `[(set-props ~{field value})]))

#?(:cljs
   (defn- ensure-integer
     "Helper for set-integer!, use that instead. It is recommended you use this function only on UI-related
     data (e.g. data that is used for display purposes) and write clear top-level transactions for anything else."
     [v]
     (let [rv (js/parseInt v)]
       (if (js/isNaN rv) 0 rv)))
   :clj
   (defn- ensure-integer [v] (Integer/parseInt v)))

(defn target-value [evt] (.. evt -target -value))

(defn set-integer!
  "Set the given integer on the given `field` of a `component`. Allows same parameters as `set-string!`.

   It is recommended you use this function only on UI-related data (e.g. data that is used for display purposes)
   and write clear top-level transactions for anything else. Calls to this are compressed in history."
  [component field & {:keys [event value]}]
  (assert (and (or event value) (not (and event value))) "Supply either :event or :value")
  (let [value (ensure-integer (if event (target-value event) value))]
    (set-value! component field value)))

#?(:cljs
   (defn- ensure-double [v]
     (let [rv (js/parseFloat v)]
       (if (js/isNaN rv) 0 rv)))
   :clj
   (defn- ensure-double [v] (Double/parseDouble v)))

(defn set-double!
  "Set the given double on the given `field` of a `component`. Allows same parameters as `set-string!`.

   It is recommended you use this function only on UI-related data (e.g. data that is used for display purposes)
   and write clear top-level transactions for anything else. Calls to this are compressed in history."
  [component field & {:keys [event value]}]
  (assert (and (or event value) (not (and event value))) "Supply either :event or :value")
  (let [value (ensure-double (if event (target-value event) value))]
    (set-value! component field value)))

(defn set-string!
  "Set a string on the given `field` of a `component`. The string can be literal via named parameter `:value` or
  can be auto-extracted from a UI event using the named parameter `:event`

  Examples

  ```
  (set-string! this :ui/name :value \"Hello\") ; set from literal (or var)
  (set-string! this :ui/name :event evt) ; extract from UI event target value
  ```

  It is recommended you use this function only on UI-related
  data (e.g. data that is used for display purposes) and write clear top-level transactions for anything else.
  Calls to this are compressed in history."
  [component field & {:keys [event value]}]
  (assert (and (or event value) (not (and event value))) "Supply either :event or :value")
  (let [value (if event (target-value event) value)]
    (set-value! component field value)))
