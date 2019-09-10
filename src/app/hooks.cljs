(ns app.hooks
  (:require [app.react :as react]
            [clojure.walk :refer [postwalk]]))

(defprotocol EditorContext
  (add-entity [_ entity])
  (update-entity [_ entity])
  (remove-entity [_ entity])
  (add-association [_ association])
  (remove-association [_ association])
  (get-entities [_])
  (get-entity [_ id])
  (get-associations [_])
  (get-association [_ id])
  (update-association [_ association])
  (get-associations-for [_ entity direction])
  (debug [_]))

(defprotocol EntityController
  (set-attr [_ instance attr value])
  (add-list-item [_ instance attr item])
  (remove-list-item [_ instance attr item]))

(defn use-editor-context [initial-state]
  (let [[state set-state] (react/use-state initial-state)
        update-state! (fn [new-state] (set-state new-state))]
    (reify EditorContext
      (add-entity [_ entity]
        (update-state! (update state :entities conj entity)))
      (update-entity [_ entity]
        (update-state! (update state :entities
                               (fn [entities]
                                 (mapv (fn [old-entity]
                                        (if (= (:id old-entity) (:id entity))
                                          entity
                                          old-entity)) entities)))))
      (update-association [_ entity]
        (update-state! (update state :associations
                               (fn [entities]
                                 (mapv (fn [old-entity]
                                        (if (= (:id old-entity) (:id entity))
                                          entity
                                          old-entity)) entities)))))
      (remove-entity [_ entity]
        (update-state! (update state :entities (fn [entities] (remove #(= (:id %) (:id entity)) entities)))))
      (add-association [_ association]
        (update-state! (update state :associations conj association)))
      (remove-association [_ association]
        (update-state!
          (update state :associations (fn [associations] (remove #(= (:id %) (:id association)) associations)))))
      (get-entities [_] (:entities state))
      (get-entity [_ id]
        (-> (filter #(= (:id %) id) (:entities state))
            (first)))
      (get-associations [_] (:associations state))
      (get-association [_ id]
        (-> (filter #(= (:id %) id) (:associations state))
            (first)))
      (get-associations-for [_ {:keys [id]} direction]
        (if direction
          (filter #(= (get % direction) id) (:associations state))
          (filter #(or (= (:from %) id)
                       (= (:to %) id)) (:associations state))))
      (debug [_] state))))

(defn- update-at [entity instance key update-fn]
  (postwalk
    (fn [form]
      (if (and (map? form)
               (= (:id form) (:id instance)))
        (update form key (fn [old-val] (update-fn old-val)))
        form)) entity))

(defn use-entity-editor [ctx base-entity]
  (let [ctx              (react/use-context ctx)
        update-entity-at (fn [instance key update-fn]
                           (let [updated-entity (update-at base-entity instance key update-fn)]
                             (update-entity ctx updated-entity)))]
    (reify EntityController
      (set-attr [_ instance attr value] (update-entity-at instance attr (constantly value)))
      (add-list-item [_ instance attr item]
        (update-entity-at instance attr (fn [old-list] (conj old-list item))))
      (remove-list-item [_ instance attr item]
        (update-entity-at instance attr (fn [old-list] (remove #(= (:id item) (:id %)) old-list)))))))

(defn use-association-editor [ctx base-association]
  (let [ctx                (react/use-context ctx)
        update-at          (fn [instance key update-fn]
                             (let [updated-association (update-at base-association instance key update-fn)]
                               (update-association ctx updated-association)))]
    (reify EntityController
      (set-attr [_ instance attr value] (update-at instance attr (constantly value)))
      (add-list-item [_ instance attr item]
        (update-at instance attr (fn [old-list] (conj old-list item))))
      (remove-list-item [_ instance attr item]
        (update-at instance attr (fn [old-list] (remove #(= (:id item) (:id %)) old-list)))))))
