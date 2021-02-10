#' Manage error handlers
#'
#' An error handler is responsible for providing a response when a router
#' has to return a failure status code.
#'
#' @param status An http status code to map error handler from the `@error` plumber tag to the global error handlers list.
#' @param error_handler The error handler function to be added. This build the error handler function. See Details for more information.
#' @param verbose Logical value which determines if a warning should be
#' displayed when alias in map are overwritten.
#'
#' @details
#' When `error_handler` is evaluated, it should return an error handler function.
#' Error handler matching is done by using the response status code.
#'
#' Plumber will try to use [error_handler_default()] (if available) when no
#' status is set for a response.
#'
#' Functions signature should include `...` and possibly `req`, `res`, `err`.
#' Plumber will pass along `req` and `res` environments and `err` depending on
#' context.
#'
#' Error handler function structure is something like below.
#' ```r
#' function(error_hanlder_arguments) {
#'   # return a function to handle failure status code
#' }
#' ```
#'
#' @examples
#' not_found_error <- function(res, ...) {
#'     list(error = "404 - Resource Not Found")
#' }
#'
#' # Register the newly created error handler
#' \dontrun{register_error_handler(404, not_found_error)}
#' @export
register_error_handler <- function(
  status,
  error_handler,
  verbose = TRUE
) {

  if (!is.null(.globals$error_handlers[[as.character(status)]])) {
    if (isTRUE(verbose)) {
      warning("Overwriting error handler for status code: ", status)
    }
  }

  stopifnot(is.function(error_handler))

  # Init the parser function with outside arguments
  init_parser_func <- function(req, res, err, ...) {

    res$status <- status
    res$serializer <- serializer_unboxed_json()

    error_hanlder_function <- do.call(error_handler, list(req, res, err, ...))

    error_hanlder_formals <- formals(error_hanlder_function)
    if (!("..." %in% names(parser_formals))) {
      stop("For status code '", status, "' error handler, please add a `...` argument to the returned function for possible future parameter expansion")
    }

    return(parser_info)
  }

  .globals$error_handlers[[as.character(status)]] <- init_error_handler_func
  invisible(.globals$error_handlers)
}

#' @describeIn register_parser Return all registered parsers
#' @export
registered_parsers <- function() {
  sort(names(.globals$parsers))
}

# ' @describeIn register_parser Select from global parsers and create a combined parser list for programmatic use.
# ' @param aliases Can be one of:
# '   * A character vector of `alias` names.
# '   * A named `list()` whose keys are `alias` names and values are arguments to be applied with [do.call()]
# '   * A `TRUE` value, which will default to combining all parsers. This is great for seeing what is possible, but not great for security purposes.
# '   * Already combined parsers. (Will be returned immediately.)
# '
# ' If `"all"` is found in any `alias` character value or list name, all remaining parsers will be added.  When using a list, aliases already defined will maintain their existing argument values.  All other parser aliases will use their default arguments.
# ' @examples
# ' # provide a character string
# ' make_parser("json")
# '
# ' # provide a named list with no arguments
# ' make_parser(list(json = list()))
# '
# ' # provide a named list with arguments; include `rds`
# ' make_parser(list(json = list(simplifyVector = FALSE), rds = list()))
# '
# ' # default plumber parsers
# ' make_parser(c("json", "form", "text", "octet", "multi"))
make_parser <- function(aliases) {
  if (inherits(aliases, "plumber_parsed_parsers")) {
    return(aliases)
  }
  if (isTRUE(aliases)) {
    # use all parsers
    aliases <- "all"
  }
  if (is.character(aliases)) {
    if (any(is.na(aliases))) {
      stop("aliases can not be `NA` values")
    }
    if ("all" %in% aliases) {
      # use all available parsers expect `all` and `none`
      aliases <- setdiff(registered_parsers(), c("all", "none"))
    }
    # turn aliases into a named list with empty values
    aliases <- setNames(
      replicate(length(aliases), {list()}),
      aliases
    )
  }

  stopifnot(is.list(aliases))
  if (is.null(names(aliases))) {
    stop("aliases must be able to convert to a named list")
  }

  local({
    aliases_not_found <- !(names(aliases) %in% registered_parsers())
    if (any(aliases_not_found)) {
      missing_aliases <- names(aliases)[aliases_not_found]
      stop("Aliases not available: ", paste0(missing_aliases, collapse = ", "), ". See: registered_parsers()")
    }
  })

  # if "all" is found, remove "all" and add all remaining registered parsers (except 'none') to the `aliases` list
  if ("all" %in% names(aliases)) {
    all_parser_names <- setdiff(registered_parsers(), c("all", "none"))
    # remove to avoid infinite recursion
    aliases$all <- NULL
    names_to_add <- setdiff(all_parser_names, names(aliases))
    if (length(names_to_add)) {
      aliases[names_to_add] <- replicate(length(names_to_add), list())
    }
  }

  # convert parser functions into initialized information
  parser_infos <-
    lapply(
      names(aliases),
      function(alias) {
        # get init function
        init_parser_func <- .globals$parsers[[alias]]
        # call outer parser function to init the params for inner function
        do.call(init_parser_func, aliases[[alias]])
      }
    )

  # combine information into a single list
  combined_parser_info <-
    Reduce(
      function(cur_parser_info, parser_info) {
        utils::modifyList(cur_parser_info, parser_info)
      },
      parser_infos,
      init = list()
    )

  class(combined_parser_info) <- c("plumber_parsed_parsers", class(combined_parser_info))
  combined_parser_info
}

#' Plumber Parsers
#'
#' Parsers are used in Plumber to transform request body received
#' by the API. Extra parameters may be provided to parser
#' functions when enabling them on router. This will allow for
#' non-default behavior.
#'
#' Parsers are optional. When unspecified, only default endpoint parsers are enabled.
#' You can use `@parser NAME` tag to enable parser on endpoint.
#' Multiple parsers can be enabled on the same endpoint using multiple `@parser NAME` tags.
#'
#' User should be aware that `rds` parsing should only be done from a
#' trusted source. Do not accept `rds` files blindly.
#'
#' See [registered_parsers()] for a list of registered parsers names.
#'
#' @param ... parameters supplied to the appropriate internal function
#' @describeIn parsers Form query string parser
#' @examples
#' \dontrun{
#' # Overwrite `text/json` parsing behavior to not allow JSON vectors to be simplified
#' #* @parser json simplifyVector = FALSE
#' # Activate `rds` parser in a multipart request
#' #* @parser multi
#' #* @parser rds
#' pr <- Plumber$new()
#' pr$handle("GET", "/upload", function(rds) {rds}, parsers = c("multi", "rds"))
#' }
#' @export
parser_form <- function() {
  parser_text(parseQS)
}


#' @describeIn parsers JSON parser. See [jsonlite::parse_json()] for more details. (Defaults to using `simplifyVectors = TRUE`)
#' @export
parser_json <- function(...) {
  parser_text(function(txt_value) {
    safeFromJSON(txt_value, ...)
  })
}


#' @describeIn parsers Helper parser to parse plain text
#' @param parse_fn function to further decode a text string into an object
#' @export
parser_text <- function(parse_fn = identity) {
  stopifnot(is.function(parse_fn))
  function(value, content_type = NULL, ...) {
    charset <- get_character_set(content_type)
    txt_value <- rawToChar(value)
    Encoding(txt_value) <- charset
    parse_fn(txt_value)
  }
}

register_parsers_onLoad <- function() {
  # parser alias names for plumbing
  register_parser("csv",     parser_csv,     fixed = c("application/csv", "application/x-csv", "text/csv", "text/x-csv"))
  register_parser("json",    parser_json,    fixed = c("application/json", "text/json"))
  register_parser("multi",   parser_multi,   fixed = "multipart/form-data", regex = "^multipart/")
  register_parser("octet",   parser_octet,   fixed = "application/octet-stream")
  register_parser("form",    parser_form,   fixed = "application/x-www-form-urlencoded")
  register_parser("rds",     parser_rds,     fixed = "application/rds")
  register_parser("feather", parser_feather, fixed = "application/feather")
  register_parser("text",    parser_text,    fixed = "text/plain", regex = "^text/")
  register_parser("tsv",     parser_tsv,     fixed = c("application/tab-separated-values", "text/tab-separated-values"))
  # yaml types: https://stackoverflow.com/a/38000954/591574
  register_parser("yaml",    parser_yaml,    fixed = c("text/vnd.yaml", "application/yaml", "application/x-yaml", "text/yaml", "text/x-yaml"))
  register_parser("none",    parser_none,    regex = "*")

  parser_all <- function() {
    stop("This function should never be called. It should be handled by `make_parser('all')`")
  }
  register_parser("all", parser_all, regex = "*")
}












# old handlers----------------------------------



default404Handler <- function(req, res) {
  res$status <- 404
  res$serializer <- serializer_unboxed_json()
  list(error="404 - Resource Not Found")
}

default405Handler <- function(req, res) {
  res$status <- 405L
  # https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Allow
  res$setHeader("Allow", paste(req$verbsAllowed, collapse = ", "))
  res$serializer <- serializer_unboxed_json()
  list(error = "405 - Method Not Allowed")
}

default405Handler <- http_error(405, function())

http_error <- function(status, errorfunc = function(...) {}) {
  function(req, res, ...) {
    res$status <- status
    res$serializer <- serializer_unboxed_json()
    res$body <- list(error = paste(status, status_desc(status)))
    errorfunc(req, res, ...)
    res$body
  }
}


defaultErrorHandler <- function() {
  function(req, res, func){
    print(err)

    li <- list()

    # always serialize error with unboxed json
    res$serializer <- serializer_unboxed_json()

    if (res$status == 200L){
      # The default is a 200. If that's still set, then we should probably override with a 500.
      # It's possible, however, than a handler set a 40x and then wants to use this function to
      # render an error, though.
      res$status <- 500
      li$error <- "500 - Internal server error"
    } else {
      li$error <- "Internal error"
    }


    # Don't overly leak data unless they opt-in
    if (is.function(req$pr$getDebug) && isTRUE(req$pr$getDebug())) {
      li["message"] <- as.character(err)
    }

    li
  }
}

# this function works under the assumption that the regular route was not found.
# Here we are only trying to find if there are other routes that can be
#' @noRd
allowed_verbs <- function(pr, path_to_find) {

  verbs_allowed <- c()

  # look at all possible endpoints
  for (endpoint_group in pr$endpoints) {
    for (endpoint in endpoint_group) {
      if (endpoint$matchesPath(path_to_find)) {
        verbs_allowed <- c(verbs_allowed, endpoint$verbs)
      }
    }
  }

  # look at all possible mounts
  for (i in seq_along(pr$mounts)) {
    mount <- pr$mounts[[i]]
    mount_path <- sub("/$", "", names(pr$mounts)[i]) # trim trailing `/`

    # if the front of the urls don't match, move on to next mount
    if (!identical(
      substr(path_to_find, 1, nchar(mount_path)),
      mount_path
    )) {
      next
    }

    # remove path and recurse
    mount_path_to_find <- substr(path_to_find, nchar(mount_path) + 1, nchar(path_to_find))
    m_verbs <- allowed_verbs(mount, mount_path_to_find)

    # add any verbs found
    if (length(m_verbs) > 0) {
      verbs_allowed <- c(verbs_allowed, m_verbs)
    }
  }

  # return verbs
  sort(unique(verbs_allowed))
}

#' @noRd
is_405 <- function(pr, path_to_find, verb_to_find) {
  verbs_allowed <- allowed_verbs(pr, path_to_find)

  # nothing found, not 405
  if (length(verbs_allowed) == 0) return(FALSE)

  # is verb excluded?
  !(verb_to_find %in% verbs_allowed)
}
router_has_route <- function(pr, path_to_find, verb_to_find) {
  verbs_allowed <- allowed_verbs(pr, path_to_find)

  # nothing found, not a route
  if (length(verbs_allowed) == 0) return(FALSE)

  # is verb found?
  verb_to_find %in% verbs_allowed
}

#' @noRd
status_desc <- function(status) {
  statuses <- list(
    "404" = "Resource Not Found",
    "405" = "Method Not Allowed",
    "500" = "Internal server error"
  )[[as.character(status)]]
}
