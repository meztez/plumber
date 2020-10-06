#' Convert router to/from OpenAPI specifications
#' @rdname openapi
#' @param pr A plumber router or a plumbable file or directory.
#' @description These functions are used to convert between OpenAPI
#' Specifications and plumber file. Plumber only supports a limited
#' set of OpenAPI Specifications.
#' @param output An optional filename where to write OpenAPI Specifications.
#' @param format Either `json` or `yaml`.
#' @details OpenAPI Specifications are used to document APIs. More info
#' can be found at (https://www.openapis.org/)
#' @examples
#' pr <- plumber$new()
#' to_openapi(pr)
#' @export
to_openapi <- function(plumber, output = NULL) {
  if (!inherits(pr, "plumber")) {
    if (file.exists(plumber)) {
      if (file.info(plumber)$isdir) {
        pr <- plumb(dir = plumber)
      } else {
        pr <- plumb(plumber)
      }
    }
  }
  spec <- pr$apiSpec()
  openapi_url <- {
    api_url_1 <- getOption("plumber.apiURL")
    api_url_2 <- urlHost(scheme = getOption("plumber.apiScheme", ""),
                         host   = getOption("plumber.apiHost", ""),
                         port   = getOption("plumber.apiPort", ""),
                         path   = getOption("plumber.apiPath", ""),
                         changeHostLocation = TRUE)
    priorizeProperty(api_url_1, api_url_2)
  }

  spec$servers$url <- openapi_url

  spec <- jsonlite::toJSON(spec, auto_unbox = TRUE)
  if (!is.null(output)) {
    writeLines(spec, output)
    return()
  }
  return(spec)
}

#' @rdname openapi
#' @param openapi an OpenAPI specifications string, URL or file
#' @param format Input file format. Either "json" or "yml". If not
#' provided, will be guessed from file extension.
#' @export
from_openapi <- function(openapi, format = c("json", "yaml")) {
  format <- match.arg(format)
  spec <- new.env()
  if (tools::file_ext(openapi) %in% c("yml", "yaml") || format == "yaml") {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("The yaml package is not available but is required in order to parse yaml specifications.\ninstall.packages(\"yaml\")",
           call. = FALSE)
    }
    if (stringi::stri_detect_fixed(openapi, "\n")) {
      s <- yaml::yaml.load(openapi)
    } else {
      s <- yaml::read_yaml(openapi)
    }
  } else {
  s <- jsonlite::fromJSON(openapi)
  }
  mapply(assign, names(s), s, MoreArgs = list(envir = spec))
  stubSpec(spec)
  return(spec)
}

#' @noRd
stubSpec <- function(spec) {

  info_lines <- function(lines, value, field) {
    if (is.null(value)) return(lines)
    if (is.list(value)) {
      value <- paste0("list(\"", paste0(names(value), "\" = ", value, collapse = ", "), ")")
    }
    line <- paste0("#* @api", field, " ", value)
    line <- gsub("\n", "\n#* ", line)
    c(lines, line)
  }

  # Info Object
  lines <- character()
  lines <- info_lines(lines, spec$info$title, "Title")
  lines <- info_lines(lines, spec$info$description, "Description")
  lines <- info_lines(lines, spec$info$termsOfService, "TOS")
  lines <- info_lines(lines, spec$info$contact$name, "Contact")
  lines <- info_lines(lines, spec$info$license$name, "License")
  lines <- info_lines(lines, spec$info$version, "Version")

  # Tag Objects
  lines <- c(lines, vapply(spec$tags, function(tag) {
    if (grepl('\\s', tag$name)) {
      tag$name <- paste0('"', gsub('"', "''", tag$name), '"')
    }
    value <- paste("#* @apiTags", tag$name, tag$description)
    value <- gsub("\n", "\n#* ", value)
  }, character(1)))

  # Path Objects


  a <- c("title","description", "termsOfService", "contact", "license", "version", "bob")
  b <- c("Title","Description","TOS","Contact","License", "Version", "Bobby")
  line <- paste0("#* @api", b, " ", spec$info[a])
}

stubEndpoint <- function(spec) {

}

stubParameters <- function(spec) {

}
