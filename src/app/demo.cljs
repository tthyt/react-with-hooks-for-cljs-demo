(ns app.demo
  (:require [app.hooks :as hooks]
            [app.react :as react]
            [clojure.walk :refer [postwalk]]))

(def ctx-provider (react/create-context nil))

(defn new-association [from to]
  {:id          (random-uuid)
   :description ""
   :from        from
   :to          to})

(defn new-todo []
  {:id          (random-uuid)
   :name        "New Todo"
   :description "New description"})

(defn attribute [editor instance key]
  {:value     (get instance key)
   :on-change #(hooks/set-attr editor instance key %)})

(defn text-input [{:keys [value on-change]}]
  [:input {:value     value
           :on-change #(on-change (.-value (.-target %)))}])

(defn select-input [{:keys [value on-change options]}]
  (letfn [(on-change' [event]
            (let [selected-id (.-value (.-target event))]
              (-> (filter #(= (str (:id %)) selected-id) options)
                  first
                  :id
                  on-change)))]
    [:select {:value (str (or value "")) :on-change on-change'}
     (for [option (cons {:id ""} options)]
       ^{:key (:id option)}
       [:option {:value (str (:id option))} (:name option)])]))

(defn to-association-editor [{:keys [association entities on-delete]}]
  (let [editor (hooks/use-association-editor ctx-provider association)]
    [:div.inlined
     [text-input (attribute editor association :description)]
     [select-input (merge (attribute editor association :to)
                          {:options (filter #(not (= (:id %) (:from association))) entities)})]
     [:button {:on-click on-delete} "x"]]))

(defn todo-item-editor [{:keys [todo-item on-delete]}]
  (let [editor (hooks/use-entity-editor ctx-provider todo-item)
        ctx    (react/use-context ctx-provider)]
    [:div.container {:class-name "this-works-too"}
     [:h2 (:name todo-item) [:button {:on-click on-delete} "x"]]
     [:label "Name"]
     [text-input (attribute editor todo-item :name)]
     [:label "Description"]
     [text-input (attribute editor todo-item :description)]
     [:label "Associations"]
     (for [association (hooks/get-associations-for ctx todo-item :from)]
       ^{:key (:id association)}
       [to-association-editor {:association association :entities (hooks/get-entities ctx)
                               :on-delete   #(hooks/remove-association ctx association)}])
     [:button {:on-click #(hooks/add-association ctx (new-association (:id todo-item) nil))} "Add association"]]))

(defn association-editor [{:keys [association entities on-delete]}]
  (let [editor (hooks/use-association-editor ctx-provider association)]
    [:div.inlined
     [select-input (merge (attribute editor association :from)
                          {:options (filter #(not (= (:id %) (:to association))) entities)})]
     [text-input (attribute editor association :description)]
     [select-input (merge (attribute editor association :to)
                          {:options (filter #(not (= (:id %) (:from association))) entities)})]
     [:button {:on-click on-delete} "x"]]))

(defn root-editor [{:keys [todo-items associations]}]
  (let [ctx (hooks/use-editor-context {:entities todo-items :associations associations})]
    [ctx-provider {:value ctx}
     [:section
      (for [todo-item (hooks/get-entities ctx)]
        ^{:key (:id todo-item)}
        [todo-item-editor {:todo-item (hooks/get-entity ctx (:id todo-item))
                           :on-delete #(hooks/remove-entity ctx todo-item)}])
      [:button {:on-click #(hooks/add-entity ctx (new-todo))} "Add Todo"]]
     [:section
      [:h2 "All associations"]
      (for [association (hooks/get-associations ctx)]
        ^{:key (:id association)}
        [association-editor {:association association
                             :entities    (hooks/get-entities ctx)
                             :on-delete   #(hooks/remove-association ctx association)}])
      [:button {:on-click #(hooks/add-association ctx (new-association nil nil))} "Add association"]]
     [:div.preformatted
      (with-out-str (cljs.pprint/pprint (hooks/debug ctx)))]]))

(defn mount! []
  (react/render
    [:div
     [:h1 "Todo Editor"]
     (let [report-1-id    (random-uuid)
           report-2-id    (random-uuid)
           report-3-id    (random-uuid)
           state-from-api {:todo-items   [{:id          report-1-id
                                           :name        "Todo Item 1"
                                           :description "Todo"}
                                          {:id          report-2-id
                                           :name        "Todo Item 2"
                                           :description "Todo"}
                                          {:id          report-3-id
                                           :name        "Todo Item 3"
                                           :description "Todo"}]
                           :associations [{:from        report-2-id
                                           :to          report-1-id
                                           :id          (random-uuid)
                                           :description "Depends on"}
                                          {:from        report-3-id
                                           :to          report-2-id
                                           :id          (random-uuid)
                                           :description "Depends on"}]}]
       [root-editor state-from-api])]
    (js/document.getElementById "app")))

(defn main! []
  (mount!))

(defn reload! []
  (mount!))

(comment
  ;; shadow cljs repl
  (in-ns 'shadow.cljs.devtools.api)
  (repl :app))