#' @title Projects Client
#' @description Projects client for project-related operations
#' @name ProjectsClient
NULL

#' @importFrom R6 R6Class
#' @export
ProjectsClient <- R6::R6Class(
  "ProjectsClient",
  
  public = list(
    #' @description Create a new projects client
    #' @param http_client HttpClient instance
    initialize = function(http_client) {
      private$http <- http_client
    },
    
    #' @description List all projects
    #' @param page Page number (optional)
    #' @param per_page Items per page (optional)
    #' @return grey_result with projects list or error
    list_projects = function(page = NULL, per_page = NULL) {
      params <- list()
      if (!is.null(page)) params$page <- page
      if (!is.null(per_page)) params$per_page <- per_page
      
      if (length(params) == 0) params <- NULL
      
      private$http$get("/api/v1/projects", params)
    },
    
    #' @description Create a new project
    #' @param name Project name
    #' @param description Project description (optional)
    #' @param metadata Additional metadata (optional)
    #' @return grey_result with created project or error
    create_project = function(name, description = NULL, metadata = NULL) {
      if (is.null(name) || nchar(trimws(name)) == 0) {
        return(result_err(error_validation("Project name is required")))
      }
      
      body <- list(name = name)
      if (!is.null(description)) body$description <- description
      if (!is.null(metadata)) body$metadata <- metadata
      
      private$http$post("/api/v1/projects", body)
    },
    
    #' @description Get a project by ID
    #' @param project_id The project ID
    #' @return grey_result with project data or error
    get_project = function(project_id) {
      if (is.null(project_id) || nchar(trimws(as.character(project_id))) == 0) {
        return(result_err(error_validation("Project ID is required")))
      }
      
      private$http$get(paste0("/api/v1/projects/", project_id))
    },
    
    #' @description Update a project
    #' @param project_id The project ID
    #' @param data Update data (list)
    #' @return grey_result with updated project or error
    update_project = function(project_id, data) {
      if (is.null(project_id) || nchar(trimws(as.character(project_id))) == 0) {
        return(result_err(error_validation("Project ID is required")))
      }
      
      if (is.null(data) || length(data) == 0) {
        return(result_err(error_validation("Update data is required")))
      }
      
      private$http$patch(paste0("/api/v1/projects/", project_id), data)
    },
    
    #' @description Delete a project
    #' @param project_id The project ID
    #' @return grey_result
    delete_project = function(project_id) {
      if (is.null(project_id) || nchar(trimws(as.character(project_id))) == 0) {
        return(result_err(error_validation("Project ID is required")))
      }
      
      private$http$delete(paste0("/api/v1/projects/", project_id))
    }
  ),
  
  private = list(
    http = NULL
  )
)
