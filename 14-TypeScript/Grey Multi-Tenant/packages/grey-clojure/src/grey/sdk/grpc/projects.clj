(ns grey.sdk.grpc.projects
  "Grey SDK Projects gRPC service wrapper.
  
  This is a placeholder stub. In a real implementation, this would
  use generated protobuf client stubs from .proto files."
  (:require [grey.sdk.grpc.channel :as channel]
            [grey.sdk.result :as r]
            [grey.sdk.error :as error])
  (:import [java.time Instant]))

(defn- iso8601-now []
  (str (Instant/now)))

(defn list-projects
  "Lists projects via gRPC.
  
  Arguments:
    grey-channel - The Grey channel
    metadata     - Auth metadata
    opts         - Pagination options (:page, :page-size)
    
  Returns:
    Result with projects data or error"
  ([grey-channel metadata]
   (list-projects grey-channel metadata {}))
  ([grey-channel metadata opts]
   (let [page (get opts :page 1)
         page-size (get opts :page-size 20)]
     (channel/with-channel grey-channel
       (fn [_managed-channel]
         ;; Stub implementation
         (Thread/sleep 1)
         (r/ok {:projects [{:id "project_1"
                            :name "Stub Project 1"
                            :description "A stub project for testing"
                            :created-at (iso8601-now)
                            :updated-at nil
                            :metadata nil}
                           {:id "project_2"
                            :name "Stub Project 2"
                            :description nil
                            :created-at (iso8601-now)
                            :updated-at nil
                            :metadata nil}]
                :total 2
                :page page
                :page-size page-size}))))))

(defn create-project
  "Creates a project via gRPC.
  
  Arguments:
    grey-channel - The Grey channel
    metadata     - Auth metadata
    name         - Project name
    opts         - Additional options (:description, :metadata)
    
  Returns:
    Result with project data or error"
  ([grey-channel metadata project-name]
   (create-project grey-channel metadata project-name {}))
  ([grey-channel metadata project-name opts]
   (let [description (get opts :description)]
     (channel/with-channel grey-channel
       (fn [_managed-channel]
         ;; Stub implementation
         (Thread/sleep 1)
         (r/ok {:id "new_project_id"
                :name project-name
                :description description
                :created-at (iso8601-now)
                :updated-at nil
                :metadata (:metadata opts)}))))))
