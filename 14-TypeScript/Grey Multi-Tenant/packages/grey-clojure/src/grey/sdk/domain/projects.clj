(ns grey.sdk.domain.projects
  "Grey SDK Projects domain.
  
  Provides project operations including listing and creating projects."
  (:require [grey.sdk.grpc.channel :as channel]
            [grey.sdk.grpc.projects :as grpc-projects]
            [grey.sdk.result :as r]
            [grey.sdk.error :as error]
            [clojure.string :as str]))

(defn- validate-pagination
  "Validates pagination options."
  [opts]
  (let [page (get opts :page 1)
        page-size (get opts :page-size 20)]
    (cond
      (or (not (integer? page)) (< page 1))
      (r/err (error/validation-error "Page must be a positive integer"))
      
      (or (not (integer? page-size)) (< page-size 1) (> page-size 100))
      (r/err (error/validation-error "Page size must be between 1 and 100"))
      
      :else
      (r/ok opts))))

(defn- validate-project-name
  "Validates project name."
  [name]
  (cond
    (str/blank? name)
    (r/err (error/validation-error "Project name cannot be empty"))
    
    (> (count name) 255)
    (r/err (error/validation-error "Project name cannot exceed 255 characters"))
    
    :else
    (r/ok name)))

(defn list-projects
  "Lists projects for the authenticated user.
  
  Arguments:
    grey-channel - The Grey channel
    opts         - Optional parameters:
                   :page      - Page number (default: 1)
                   :page-size - Items per page (default: 20, max: 100)
                   :filter    - Filter criteria (map)
    
  Returns:
    Result with projects data (:projects, :total, :page, :page-size) or error"
  ([grey-channel]
   (list-projects grey-channel {}))
  ([grey-channel opts]
   (if (channel/authenticated? grey-channel)
     (r/then (validate-pagination opts)
             (fn [validated-opts]
               (grpc-projects/list-projects grey-channel
                                            (channel/auth-metadata grey-channel)
                                            validated-opts)))
     (r/err (error/unauthorized "User not authenticated")))))

(defn create-project
  "Creates a new project.
  
  Arguments:
    grey-channel - The Grey channel
    name         - Project name
    opts         - Optional parameters:
                   :description - Project description
                   :metadata    - Additional metadata (map)
    
  Returns:
    Result with project data (:id, :name, :description, :created-at, :updated-at, :metadata) or error"
  ([grey-channel name]
   (create-project grey-channel name {}))
  ([grey-channel name opts]
   (if (channel/authenticated? grey-channel)
     (r/then (validate-project-name name)
             (fn [validated-name]
               (grpc-projects/create-project grey-channel
                                             (channel/auth-metadata grey-channel)
                                             validated-name
                                             opts)))
     (r/err (error/unauthorized "User not authenticated")))))
