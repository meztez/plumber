#' Convert router to/from OpenAPI specifications
#' @rdname openapi
#' @param plumber A plumber router or a plumbable file or directory
#' @description These functions are used to convert between OpenAPI
#' specifications and plumber file. Plumber only supports a limited
#' set of OpenAPI specifications.
#' @param output An optional filename where to write specifications.
#' @details OpenAPI is a specifications to describe API. More info
#' can be found at (https://www.openapis.org/)
#' @examples
#' pr <- plumber$new()
#' toOpenAPI(pr)
#' @export
toOpenAPI <- function(plumber, output = NULL) {
  if (!inherits(pr, "plumber")) {
    if (file.exists(plumber) && file.info(plumber)$isdir) {
      pr <- plumb(dir = plumber)
    } else {
      pr <- plumb(plumber)
    }
  }
  spec <- pr$openAPISpec()
  open_api_url <- {
    apiURL1 <- getOption("plumber.apiURL")
    apiURL2 <- urlHost(scheme = getOption("plumber.apiScheme", ""),
                       host   = getOption("plumber.apiHost", ""),
                       port   = getOption("plumber.apiPort", ""),
                       path   = getOption("plumber.apiPath", ""),
                       changeHostLocation = TRUE)
    priorizeProperty(apiURL1, apiURL2)
  }

  spec$servers$url <- open_api_url
  spec$servers$description <- "OpenAPI"

  spec <- jsonlite::toJSON(spec, auto_unbox = TRUE)
  if (!is.null(output)) {
    return(writeLines(spec, output))
  }
  return(spec)
}

#' @rdname openapi
#' @param openapi an OpenAPI specifications string, URL or file
#' @param format Input file format. Either "json" or "yml". If not
#' provided, will be guessed from file extension.
#' @export
fromOpenAPI <- function(openapi, format = c("json", "yaml")) {
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
  #stubSpec(spec)
}

#' @
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
