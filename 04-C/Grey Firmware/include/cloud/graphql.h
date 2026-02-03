/**
 * @file graphql.h
 * @brief GraphQL Query Client Stub
 * 
 * INDUSTRY RELEVANCE:
 * GraphQL is increasingly adopted for IoT backend communication due to its
 * efficiency in requesting exactly what's needed. AWS AppSync, Hasura, and
 * custom backends use GraphQL. It reduces over-fetching and enables real-time
 * subscriptions, ideal for IoT dashboards and device management.
 * 
 * WHY THIS MATTERS:
 * - Efficient data fetching (request only needed fields)
 * - Single endpoint for all operations
 * - Strongly typed queries
 * - Real-time subscriptions for live updates
 * - Reduces bandwidth for constrained devices
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Query construction and execution
 * - Variable substitution
 * - Subscription handling
 * - Response parsing
 */

#ifndef GF_GRAPHQL_H
#define GF_GRAPHQL_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_GQL_MAX_QUERY_LEN        1024    /* Maximum query length */
#define GF_GQL_MAX_VARIABLES        16      /* Maximum query variables */
#define GF_GQL_MAX_RESPONSE         4096    /* Maximum response size */
#define GF_GQL_VAR_NAME_LEN         32      /* Variable name length */
#define GF_GQL_VAR_VALUE_LEN        128     /* Variable value length */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Operation type */
typedef enum {
    GF_GQL_QUERY = 0,               /* Query (read) */
    GF_GQL_MUTATION,                /* Mutation (write) */
    GF_GQL_SUBSCRIPTION             /* Subscription (real-time) */
} gf_gql_operation_t;

/* Variable type */
typedef enum {
    GF_GQL_VAR_STRING = 0,
    GF_GQL_VAR_INT,
    GF_GQL_VAR_FLOAT,
    GF_GQL_VAR_BOOL,
    GF_GQL_VAR_ID,
    GF_GQL_VAR_JSON                 /* Complex JSON object */
} gf_gql_var_type_t;

/* Query variable */
typedef struct {
    char                name[GF_GQL_VAR_NAME_LEN];
    gf_gql_var_type_t   type;
    union {
        char            str[GF_GQL_VAR_VALUE_LEN];
        int64_t         i;
        double          f;
        bool            b;
    } value;
} gf_gql_variable_t;

/* GraphQL request */
typedef struct {
    gf_gql_operation_t  operation;
    char                query[GF_GQL_MAX_QUERY_LEN];
    char                operation_name[64];     /* Optional op name */
    gf_gql_variable_t   variables[GF_GQL_MAX_VARIABLES];
    int                 variable_count;
} gf_gql_request_t;

/* GraphQL error */
typedef struct {
    char                message[128];
    char                path[64];
    int                 line;
    int                 column;
} gf_gql_error_t;

/* GraphQL response */
typedef struct {
    bool                success;            /* True if no errors */
    char                data[GF_GQL_MAX_RESPONSE];  /* JSON data */
    uint16_t            data_len;
    gf_gql_error_t      errors[4];          /* Errors if any */
    int                 error_count;
    uint32_t            duration_ms;
} gf_gql_response_t;

/* Subscription state */
typedef enum {
    GF_GQL_SUB_DISCONNECTED = 0,
    GF_GQL_SUB_CONNECTING,
    GF_GQL_SUB_CONNECTED,
    GF_GQL_SUB_SUBSCRIBED,
    GF_GQL_SUB_ERROR
} gf_gql_sub_state_t;

/* Subscription callback */
typedef void (*gf_gql_sub_callback_t)(const char *data, uint16_t len, void *ctx);

/* Query callback for async operations */
typedef void (*gf_gql_callback_t)(const gf_gql_response_t *response, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize GraphQL client
 * @param endpoint GraphQL endpoint URL
 * @param auth_token Bearer token (NULL if not needed)
 */
int gf_gql_init(const char *endpoint, const char *auth_token);

/**
 * @brief Set authentication token
 */
int gf_gql_set_auth(const char *token);

/**
 * @brief Execute synchronous query/mutation
 * @param request Query request
 * @param response Output response
 */
int gf_gql_execute(const gf_gql_request_t *request, 
                   gf_gql_response_t *response);

/**
 * @brief Execute asynchronous query/mutation
 */
int gf_gql_execute_async(const gf_gql_request_t *request,
                         gf_gql_callback_t callback, void *ctx);

/**
 * @brief Start subscription
 * @param request Subscription request
 * @param callback Data callback
 * @param ctx User context
 * @return Subscription handle or error
 */
int gf_gql_subscribe(const gf_gql_request_t *request,
                     gf_gql_sub_callback_t callback, void *ctx);

/**
 * @brief Stop subscription
 */
int gf_gql_unsubscribe(int handle);

/**
 * @brief Get subscription state
 */
gf_gql_sub_state_t gf_gql_get_sub_state(int handle);

/**
 * @brief Build simple query
 * @param operation Query, mutation, or subscription
 * @param body Query body
 * @param request Output request structure
 */
int gf_gql_build_query(gf_gql_operation_t operation, const char *body,
                       gf_gql_request_t *request);

/**
 * @brief Add string variable to request
 */
int gf_gql_add_string_var(gf_gql_request_t *request, const char *name,
                          const char *value);

/**
 * @brief Add integer variable to request
 */
int gf_gql_add_int_var(gf_gql_request_t *request, const char *name,
                       int64_t value);

/**
 * @brief Add float variable to request
 */
int gf_gql_add_float_var(gf_gql_request_t *request, const char *name,
                         double value);

/**
 * @brief Add boolean variable to request
 */
int gf_gql_add_bool_var(gf_gql_request_t *request, const char *name,
                        bool value);

/**
 * @brief Extract field from JSON data
 * @param json JSON string
 * @param path Field path (e.g., "user.name")
 * @param output Output buffer
 * @param output_len Buffer size
 */
int gf_gql_extract_field(const char *json, const char *path,
                         char *output, int output_len);

/**
 * @brief Extract integer field
 */
int gf_gql_extract_int(const char *json, const char *path, int64_t *value);

/**
 * @brief Extract float field
 */
int gf_gql_extract_float(const char *json, const char *path, double *value);

/**
 * @brief Extract boolean field
 */
int gf_gql_extract_bool(const char *json, const char *path, bool *value);

/**
 * @brief Process pending operations (call from main loop)
 */
int gf_gql_process(void);

#endif /* GF_GRAPHQL_H */
