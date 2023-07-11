drop_nulls <- function(x) {
  valid_elements <- purrr::keep(x, ~!is.null(.x))
  if (!length(valid_elements)) {
    return(NULL)
  }
  return(valid_elements)
}

#' Convert character string to JSON
#'
#' Allows to include raw JS code in JSON object.
#'
#' Along with \code{json_verbatim = TRUE} argument of \link[jsonlite]{toJSON}
#' makes passing the code to JSON object possible.
#'
#' @param x Character storing JS code or object.
#' @examples
#' # Function stored as text
#' jsonlite::toJSON(
#'   list(fun = "function(x) {console.log(x);}"),
#'   auto_unbox = TRUE, json_verbatim = TRUE
#' )
#' # Function stored as raw JS object
#' jsonlite::toJSON(
#'   list(fun = as_json("function(x) {console.log(x);}")),
#'   auto_unbox = TRUE, json_verbatim = TRUE
#' )
#' @return A character of class 'json'.
#' The output can be used to store bare JS element withing JSON object.
#' @export
as_json <- function(x) {
  class(x) <- c("json", class(x))
  return(x)
}

make_verbatim <- function(x, which = NULL) {
  el_names <- names(x)
  x %>%
    purrr::imodify(~ if (.y %in% which) {as_json(.x)} else {.x}) %>%
    stats::setNames(el_names)
}

#' Branding configuration
#'
#' Configuration for custom annotations branding.
#' See more at: \href{https://h.readthedocs.io/projects/client/en/latest/publishers/config.html#cmdoption-arg-branding}{branding}.
#'
#' @param accentColor Css-valid color value defining sidebar accent.
#' @param appBackgroundColor Css-valid color value defining sidebar background.
#' @param ctaBackgroundColor Css-valid color value defining main call-to-action button background.
#' @param ctaTextColor Css-valid color value defining font color used inside of the call-to-action buttons.
#' @param selectionFontFamily Css-valid color value defining font family used for annotation selection popup.
#' @param annotationFontFamily Css-valid color value defining font family used for annotation popup.
#' @return Named list object storing provided arguments.
#' @export
hypothesisBranding <- function(accentColor, appBackgroundColor, ctaBackgroundColor,
                               ctaTextColor, selectionFontFamily, annotationFontFamily) {
  args <- environment() %>%
    as.list() %>%
    purrr::keep(~ !is.symbol(.x))

  if (!length(args)) {
    return(NULL)
  }
  return(args)
}
#' Hypothesis Service Configuration
#'
#' A list of alternative annotation services which the client should connect.
#' For more information visit: \href{https://h.readthedocs.io/projects/client/en/latest/publishers/config.html#cmdoption-arg-services}{services}.
#'
#' @param apiUrl,authority,grantToken,allowLeavingGroups,enableShareLinks,groups,icon,onLoginRequest,onLogoutRequest,onSignupRequest,onProfileRequest,onHelpRequest Arguments defining external hypothesis services.
#' @return Named list object storing provided arguments.
#' @export
hypothesisServices <- function(apiUrl, authority, grantToken, allowLeavingGroups, enableShareLinks, groups, icon,
                               onLoginRequest, onLogoutRequest, onSignupRequest, onProfileRequest, onHelpRequest) {

  args <- environment() %>%
    as.list() %>%
    purrr::keep(~ !is.symbol(.x)) %>%
    make_verbatim(
      c("onLoginRequest", "onLogoutRequest", "onSignupRequest", "onProfileRequest", "onHelpRequest")
    )

  if (!length(args)) {
    return(NULL)
  }
  return(args)
}

#' Turn On/Off Annotations Feature
#'
#' Generates button that allows to turn on and turn off annotations tool.
#'
#' @param initShow Define initial state of annotations. When FALSE, annotations are turned off.
#' @param labelShow,labelHide Label to be shown in the button when annotations are hidden and shown respectively.
#'
#' @return A 'button' shiny.tag object, responsible for enabling/disabling of the package functionality.
#' @export
hypothesisOnOff <- function(initShow = TRUE, labelShow = "Enable Annotations",
                            labelHide = "Disable Annotations") {

  htmltools::tags$button(
    type = "Button",
    class = "btn btn-default hi-onoff",
    if (initShow) labelHide else labelShow,
    `data-visible` = if (initShow) "yes" else "no",
    onclick = htmlwidgets::JS(glue::glue("toggleHypothesis(this, '{labelShow}', '{labelHide}');"))
  )
}

to_list <- function(x, element) {
  if (!is.null(x[[element]])) {
    x[[element]] <- list(x[[element]])
  }
  return(x)
}

#' Use hypothesis annotations
#'
#' Function used to initialize hypothesis annotations in Shiny app or R Markdown document.
#'
#' @param openSidebar Set to TRUE if annotations sidebar should be opened in initial state.
#' @param theme Annotations sidebar theme. Available options "classic" (default) and "clean".
#' @param showHighlights Configure when annotated text should be highlighted.
#'   "whenSidebarOpen" highlights content only when annotations sidebar is open (default).
#'   The remaining options are "always" and "never".
#' @param branding Configure annotations tool branding (e.g. sidebar background).
#'   See \link{hypothesisBranding} for possible options.
#' @param sidebarAppUrl,onLayoutChange,services,enableExperimentalNewNoteButton,usernameUrl,externalContainerSelector,focus,requestConfigFromFrame,assetRoot,notebookAppUrl,enableShareLinks Extra
#'   arguments used for external services configuration and assets configuration.
#'   Visit \href{https://h.readthedocs.io/projects/client/en/latest/publishers/config.html}{config} for more details.
#' @param ... Extra arguments passed to client configuration.
#'
#' @return A list of class 'shiny.tag.list' storing the package JS/CSS dependencies.
#' @export
useHypothesis <- function(openSidebar = TRUE,
                          showHighlights = c("whenSidebarOpen", "always", "never"),
                          theme = c("classic", "clean"),
                          branding = hypothesisBranding(),
                          sidebarAppUrl = NULL,
                          onLayoutChange = as_json("tryHideHypothesis"),
                          services = hypothesisServices(),
                          enableExperimentalNewNoteButton = NULL,
                          usernameUrl = NULL,
                          externalContainerSelector = NULL,
                          focus = NULL,
                          requestConfigFromFrame = NULL,
                          assetRoot = NULL,
                          notebookAppUrl = NULL,
                          enableShareLinks = FALSE,
                          ...) {

  hi_config_stat <- jsonlite::toJSON(
    list(
      sidebarAppUrl = sidebarAppUrl,
      openSidebar = openSidebar,
      showHighlights = match.arg(showHighlights),
      branding = branding,
      theme = match.arg(theme),
      enableExperimentalNewNoteButton = enableExperimentalNewNoteButton,
      usernameUrl = usernameUrl,
      externalContainerSelector = externalContainerSelector,
      focus = focus,
      requestConfigFromFrame = requestConfigFromFrame,
      assetRoot = assetRoot,
      notebookAppUrl = notebookAppUrl,
      enableShareLinks = enableShareLinks,
      ...
    ) %>%
      drop_nulls(),
    auto_unbox = TRUE,
    json_verbatim = TRUE
  )
  hi_config_js <- jsonlite::toJSON(
    list(
      services = services,
      onLayoutChange = onLayoutChange
    ) %>%
      drop_nulls() %>%
      to_list("services"),
    auto_unbox = TRUE,
    json_verbatim = TRUE
  )

  htmltools::tagList(
    htmltools::htmlDependency(
      name = "hypothesis",
      version = "1.1155.0",
      src = "www",
      script = list(
        list(src = "script.js"),
        list(src = "libs/hypothesis/hi_embed.js", class = "hi_embed", async = NA)
      ),
      head = format(htmltools::tagList(
        htmltools::tags$script(
          type = "application/json",
          class = "js-hypothesis-config",
          htmlwidgets::JS(glue::glue(
            "{hi_config_stat}"
          ))
        ),
        htmltools::tags$script(
          id = "hi_source",
          htmlwidgets::JS(glue::glue(
            "window.hypothesisConfig = function () {{
              return {hi_config_js};
             }};"
          ))
        )
      )),
      package = "hypothesis"
    )
  )
}
