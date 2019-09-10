(ns app.react
  (:require [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            ["react" :as React]
            ["react-dom" :as ReactDOM]))

(defonce component-cache (atom {}))
(declare hiccup->react)

(defn- create-element
  [el props children]
  (apply React/createElement el props children))

(defn- kebab->camel [name]
  (string/replace name #"-(\w)"
                  #(string/upper-case (second %1))))

(defn- cljs-props->js-props [props]
  (-> (postwalk (fn [form]
                  (if (vector? form)
                    (let [key  (first form)
                          val' (second form)
                          val  (if (= key :ref)
                                 (:react-ref (meta val'))
                                 val')]
                      [(kebab->camel (name key)) val])
                    form)) props)
      (clj->js)))

(defn- intrinsic-element [el props children]
  (let [js-props (some-> props (cljs-props->js-props))]
    (create-element el js-props children)))

(defn- wrap-component-fn [component-fn]
  (let [cached (get @component-cache component-fn)
        comp   (or cached
                   (fn [props]
                     (-> (js->clj props :keywordize-keys true)
                         (component-fn props)
                         (hiccup->react))))]
    (when (nil? cached)
      (swap! component-cache assoc component-fn comp))
    comp))

(defn- component-element [f props children]
  (create-element (wrap-component-fn f) (clj->js props) children))

(defn- context-element [provider {:keys [value]} children]
  (let [ctx-props #js {:value value}]
    (create-element provider ctx-props children)))

(defn name->element-name [kw-name]
  (first (string/split (name kw-name) ".")))

(defn append-classes [props kw-name]
  (merge props
         (->> (seq (rest (string/split (name kw-name) #"\.")))
              (cons (:class-name props))
              (string/join " ")
              (assoc {} :class-name))))

(defn- react-context? [el]
  (when (= (type el) js/Object)
    (= "react.context" (some-> (.-$$typeof el) .-description))))

(defn- hiccup->react [hiccup]
  (if (list? (seq hiccup))
    (mapv hiccup->react hiccup)
    (let [el             (first hiccup)
          props+children (rest hiccup)
          key            (:key (meta hiccup))
          props'         (when (map? (first props+children)) (first props+children))
          children       (if (some? props') (drop 1 props+children) props+children)
          props          (if (some? key)
                           (merge {:key key} props')
                           props')]
      (cond
        (empty? hiccup) nil
        (= :<> el) (intrinsic-element React/Fragment nil (mapv hiccup->react children))
        (keyword? el) (intrinsic-element
                        (name->element-name el)
                        (append-classes props el)
                        (mapv hiccup->react children))
        (fn? el) (component-element el props (mapv hiccup->react children))
        (react-context? el) (context-element (.-Provider el) props (mapv hiccup->react children))
        :else (str hiccup)))))

(defn- wrap-effect-fn [f]
  (fn [] (or (f) js/undefined)))

(defn- clj->react-deps [deps]
  (clj->js deps))


;; PUBLIC

(defn create-context [value]
  (React/createContext value))

(defn provide-context [ctx value hiccup]
  (with-meta hiccup {:ctx {:ctx ctx :value value}}))

(defn use-context [ctx]
  (React/useContext ctx))

(defn use-state [initial]
  (React/useState initial))

(defn use-reducer [reducer initialState]
  (React/useReducer reducer initialState))

(defn use-effect [f deps]
  (React/useEffect
    (wrap-effect-fn f)
    (clj->react-deps deps)))

(defn use-layout-effect [f deps]
  (React/useLayoutEffect
    (wrap-effect-fn f)
    (clj->react-deps deps)))

(defn use-callback [f deps]
  (React/useCallback f (clj->react-deps deps)))

(defn use-memo [f deps]
  (React/useMemo f (clj->react-deps deps)))

(defn use-ref [initial]
  (let [react-ref (React/useRef initial)]
    (with-meta (reify IDeref
                 (-deref [_]
                   (.-current react-ref)))
               {:react-ref react-ref})))

(defn use-latest-atom-value [atom]
  (let [[state set-state] (use-state @atom)
        _ (use-effect (fn []
                        (let [watch-key (gensym)]
                          (add-watch atom watch-key (fn [_ _ _ new-val] (set-state new-val)))
                          (fn [] (remove-watch atom watch-key)))) [atom])]
    state))

(defn render [hiccup element]
  (ReactDOM/render
    (hiccup->react hiccup)
    element))