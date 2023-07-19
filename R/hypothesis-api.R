#' Request parameters
#' @param api_path The hypothesis API path, can be specify by `hypothesis.api.api_path` option or
#' `HYPOTHESIS_API_PATH` environment variable. Default: `https://hypothes.is/api/`.
#' @param endpoint API request endpoint
#' @param type API request type
#' @param api_key User api key, generated on the platform.
#'
#' @name api_params
NULL

#' Handle hypothesis request
#' @param ... Parameter for request function
#' @inheritParams api_params
#' @export
hs_api_handler <- function(..., api_path = get_hs_host(), endpoint = NULL, type = NULL, api_key = NULL) {

  if (is.null(api_key)) api_key <- Sys.getenv("HYPOTHESIS_PAT", "")

  handler_fun <- get(type, envir = asNamespace("httr"))

  if(!grepl("/$", api_path)) {
    stop("`api_path` needs to ends with `\`")
  }

  url <- httr::parse_url(api_path)
  url$path <- paste0(url$path, endpoint)

  res <- handler_fun(url, ..., httr::add_headers("Authorization" = paste0("Bearer ", api_key)))

  if (httr::http_error(res)) {
    stop(
      "The request failed:\nStatus code: ",
      httr::status_code(res),
      "\nMessage: ",
      httr::content(res, as = "text", encoding = "UTF-8")
    )
  }

  content <- httr::content(res, as = "text", encoding = "UTF-8")

  if(is.character(content) && jsonlite::validate(content)) {
    content <- jsonlite::fromJSON(content, flatten = TRUE, simplifyDataFrame = TRUE)
  }

  return(content)
}

get_hs_host <- function() {
  getOption("hypothesis.api.api_path", Sys.getenv("HYPOTHESIS_API_PATH", "https://hypothes.is/api/"))
}

#' Search for annotations
#' @param limit The maximum number of annotations to return.
#' @param sort Available values : updated, created, group, id, user, Default: `updated`
#' @param search_after Define a start point for a subset (page) of annotation search results.
#' Example: 2023-01-01T10:00:00.000000+00:00
#' @param offset The number of initial annotations to skip in the result set.
#' @param order The order in which the results should be sorted. Default: `desc`
#' @param uri Limit the results to annotations matching the specific URI or equivalent URIs.
#' URI can be a URL (a web page address) or a URN representing another kind of resource such as
#' DOI (Digital Object Identifier) or a PDF fingerprint.
#' @param uri.parts Limit the results to annotations containing the given keyword (tokenized chunk)
#'  in the URI.
#' @param wildcard_uri Limit the results to annotations whose URIs match the wildcard pattern.
#' @param user Limit the results to annotations made by the specified user.
#' Example: `user=acct:username@hypothes.is`
#' @param group Limit the results to annotations made in the specified group (by group ID).
#' @param tags Limit the results to annotations tagged with the specified value.
#' Example: tags=artificial,intelligence
#' @param any Limit the results to annotations who contain the indicated keyword in any of the
#' following field: `quote, tags, text, url`
#' @param quote Limit the results to annotations that contain this text inside the text that was annotated.
#' @param references Returns annotations that are replies to this parent annotation ID.
#' @param text Limit the results to annotations that contain this text in their textual body.
#'
#' @inheritParams api_params
#'
#' @source \url{https://h.readthedocs.io/en/latest/api-reference/v1/}
#'
#' @export
search_annotations <- function(
    limit = 20L,
    sort = c("updated", "created", "group", "id", "user"),
    search_after = NULL,
    offset = 0,
    order = c("desc", "asc"),
    uri = NULL,
    uri.parts = NULL,
    wildcard_uri = NULL,
    user = NULL,
    group = NULL,
    tags = NULL,
    any = NULL,
    quote = NULL,
    references = NULL,
    text = NULL,
    api_path = get_hs_host(),
    api_key = NULL
  ) {

  sort <- rlang::arg_match(sort)
  order <- rlang::arg_match(order)

  query <- as.list(rlang::current_env()) |>
    purrr::compact() |>
    purrr::discard_at(c("api_path", "api_key"))

  results <- hs_api_handler(query = query, endpoint = "search", type = "GET", api_path = api_path, api_key = api_key)

  return(results$rows)
}

#' Operate with annotation
#'
#' @param annotation_id Annotation ID
#' @param action Action to process, Default: `fetch`
#' Hide and show action needs moderator permissions.
#' @param ... Parameters for update annotation action, more information in hypothesis
#' documentation
#' @inheritParams api_params
#'
#' @source \url{https://h.readthedocs.io/en/latest/api-reference/v1/}
#'
#' @examples
#' \dontrun{
#' annotation("annotation_id")
#' annotation("annotation_id", action = "update", text = "updated text")
#' annotation("annotation_id", action = "flag")
#' annotation("annotation_id", action = "hide")
#' annotation("annotation_id", action = "show")
#' annotation(
#'   action = "create",
#'   uri = "https://r-world-devs.github.io/hypothesis/articles/hypothesis-api.html",
#'   text = "test"
#' )
#' }
#' @export
annotation <- function(
    annotation_id,
    action = c("fetch", "update", "delete", "flag", "hide", "show", "create"),
    ...,
    api_path = get_hs_host(),
    api_key = NULL
) {
  action <- rlang::arg_match(action)

  if (rlang::is_missing(annotation_id)) {
    if (!identical(action, "create")) {
      stop("You need to specify annotation_id.")
    }
    annotation_id <- ""
  }

  request_type <- switch(
    action,
    "fetch" = "GET",
    "update" = "PATCH",
    "delete" = "DELETE",
    "flag" = "PUT",
    "hide" = "PUT",
    "show" = "DELETE",
    "create" = "POST"
  )

  endpoint <- switch(
    action,
    "flag" = "annotations/%s/flag",
    "hide" = "annotations/%s/hide",
    "show" = "annotations/%s/hide",
    "create" = "annotations%s",
    "annotations/%s"
  )

  body <- list(...)

  body$text <- jsonlite::unbox(body$text)
  body$uri <- jsonlite::unbox(body$uri)

  json_body <- jsonlite::toJSON(body)

  results <- hs_api_handler(
    body = json_body,
    endpoint = sprintf(endpoint, annotation_id),
    type = request_type,
    api_path = api_path,
    api_key = api_key
  )

  return(results)
}

#' Get a list of groups
#' @param authority Filter returned groups to this authority. Default: main instance authority.
#' @param document_uri Document URI - only retrieve public (i.e. non-private) groups that apply
#' to a given document URI.
#' @param expand One or more relations to expand for a group resource Optional: `organization, scopes`
#' @inheritParams api_params
#'
#' @source \url{https://h.readthedocs.io/en/latest/api-reference/v1/}
get_groups <- function(authority = NULL, document_uri = NULL, expand = NULL,
                       api_path = get_hs_host(), api_key = NULL) {

  query <- as.list(rlang::current_env()) |>
    purrr::compact() |>
    purrr::discard_at(c("api_path", "api_key"))

  results <- hs_api_handler(
    query = query,
    endpoint = "groups",
    type = "GET",
    api_path = api_path,
    api_key = api_key
  )

  return(results)

}



#' Operate with group
#'
#' @param group_id Group ID
#' @param action Action to process, Default: `fetch`
#' @param ... Parameters for update group information, more details can be found
#' in hypothesis documentation.
#' @inheritParams api_params
#'
#' @source \url{https://h.readthedocs.io/en/latest/api-reference/v1/}
#'
#' @examples
#' \dontrun{
#' group("group_id")
#' group("group_id", action = "update", name = "updated text", description = "desc")
#' group("group_id", action = "fetch_members")
#' }
#' @export
group <- function(
    group_id,
    action = c("fetch", "update", "fetch_members"),
    ...,
    api_path = get_hs_host(),
    api_key = NULL
) {
  action <- rlang::arg_match(action)

  request_type <- switch(
    action,
    "fetch" = "GET",
    "update" = "PATCH",
    "fetch_members" = "GET"
  )


  endpoint <- switch(
    action,
    "fetch_members" = "groups/%s/members",
    "groups/%s"
  )

  json_body <- jsonlite::toJSON(list(...), auto_unbox = TRUE)

  results <- hs_api_handler(
    body = json_body,
    endpoint = sprintf(endpoint, group_id),
    type = request_type,
    api_path = api_path,
    api_key = api_key
  )

  return(results)
}

#' Fetch profile information for the currently-authenticated user.
#'
#' @inheritParams api_params
#'
#' @export
get_profile <- function(api_path = get_hs_host(), api_key = NULL) {
  results <- hs_api_handler(endpoint = "profile", type = "GET", api_path = api_path, api_key = api_key)

  return(results)
}

#' Fetch the groups for which the currently-authenticated user is a member.
#'
#' @inheritParams api_params
#'
#' @export
get_profile_groups <- function(api_path = get_hs_host(), api_key = NULL) {
  results <- hs_api_handler(endpoint = "profile/groups", type = "GET", api_path = api_path, api_key = api_key)

  return(results)
}
