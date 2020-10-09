#' @rdname openapi
#' @param openapi a URL or file path to an OpenAPI document
#' @export
from_openapi <- function(openapi) {
  s <- resolve_spec(openapi)
  lines <- stub_spec(s)
  return(lines)
}

#' @noRd
resolve_uri <- function(uri) {
  # Regex adapted from https://tools.ietf.org/html/rfc3986#appendix-B
  ref <- stri_match_first_regex(uri, "^(?:([^:/?#]+):)?(?://([^/?#]*))?([^?#]*)(?:\\?([^#]*))?(?:#(.*))?")
  colnames(ref) <- c("origin", "scheme", "authority", "path", "query", "fragment")

  url <- character(length(uri))

  idx <- !is.na(ref[, "scheme"])
  url[idx] <- paste0(url[idx], ref[idx, "scheme"], ":")

  idx <- !is.na(ref[, "authority"])
  url[idx] <- paste0(url[idx], "//", ref[idx, "authority"])

  idx <- !is.na(ref[, "path"])
  url[idx] <- paste0(url[idx], ref[idx, "path"])

  domain <- dirname(url)

  idx <- !is.na(ref[, "query"])
  url[idx] <- paste0(url[idx], "?", ref[idx, "query"])

  url[nchar(url) == 0L] <- NA_character_

  cbind(ref, url)

}

#' @noRd
resolve_spec <- function(openapi, env = new.env()) {

  stopifnot(length(openapi) == 1L)
  uri <- resolve_uri(openapi)
  if (!is.na(uri[, "url"])) {
    if (tools::file_ext(uri[, "path"]) %in% c("yml", "yaml")) {
      if (!requireNamespace("yaml", quietly = TRUE)) {
        stop("The yaml package is not available but is required in order to parse yaml documentation\ninstall.packages(\"yaml\")",
             call. = FALSE)
      }
      s <- yaml::read_yaml(uri[, "url"])
    } else {
      s <- jsonlite::fromJSON(uri[, "url"])
    }
    mapply(assign, names(s), s, MoreArgs = list(envir = env))
    assign(".__source__.")
  }

  fragment <- "#/components/schemas/Weather/properties/description"
  eval(parse(text = paste(c("spec", stri_split_fixed(stri_replace_first_regex(fragment, "^#/?", ""), "/")[[1]]),collapse = "$"))))


  if (!is.na(uri[, "fragment"])) {
    resolve_ref()
  }

  resolve_ref(env)
}

#' @noRd
resolve_ref <- function(env) {

}

#' @noRd
stub_spec <- function(spec) {

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
