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
  # if (tools::file_ext(openapi) == "yaml" || format == "yaml") {
  #   if (!requireNamespace("yaml", quietly = TRUE)) {
  #     stop("The yaml package is not available but is required in order to parse yaml specifications.\ninstall.packages(\"yaml\")",
  #          call. = FALSE)
  #   }
  #   if (stringi::stri_detect_fixed(openapi, "\n")) {
  #     s <- yaml::yaml.load(openapi)
  #   } else {
  #     s <- yaml::read_yaml(openapi)
  #   }
  # } else {
  s <- jsonlite::fromJSON(openapi)
  # }
  mapply(assign, names(s), s, MoreArgs = list(envir = spec))
  return(spec)
  stubSpec(spec)
}

#' @noRd
stubSpec <- function(spec) {
  l <- function(lines, value, field) {
    if (is.null(value)) return(lines)
    line <- paste0("#* @api", field, " ", value)
    c(lines, lines)}
  options("plumber.apiURL" = spec$servers[[1]]$url)
  lines <- character()
  lines <- l(lines, spec$info$title, "Title")
  lines <- l(lines, spec$info$description, "Description")
  lines <- l(lines, spec$info$termsOfService, "TOS")
  lines <- l(lines, spec$info$contact$name, "ContactName")
  lines <- l(lines, spec$info$contact$email, "ContactEmail")
  lines <- l(lines, spec$info$contact$url, "ContactUrl")
  lines <- l(lines, spec$info$license$name, "LicenseName")
  lines <- l(lines, spec$info$license$url, "LicenseUrl")
  lines <- l(lines, spec$info$version, "Version")

  a <- c("title","description", "termsOfService", "contact", "license", "version", "bob")
  b <- c("Title","Description","TOS","Contact","License", "Version", "Bobby")
  line <- paste0("#* @api", b, " ", spec$info[a])
}

stubEndpoint <- function(spec) {

}

stubParameters <- function(spec) {

}
