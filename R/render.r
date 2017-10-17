#' Render page with template
#'
#' Each page is composed of four templates: "head", "header", "content", and
#' "footer". Each of these templates is rendered using the \code{data}, and
#' then assembled into an overall page using the "layout" template.
#'
#' @section YAML config:
#' You can use either the templates provided by pkgdown, or provide your
#' own by supplying \code{templates_path} key in your \code{_pkgdown.yml}.
#' Generally, you'll find it easiest to customise if you start with a copy
#' of the pkgdown templates and modify from there.
#'
#' @param pkg Path to package to document.
#' @param name Name of the template (e.g. "home", "vignette", "news")
#' @param data Data for the template.
#'
#'   This is automatically supplemented with three lists:
#'   \itemize{
#'   \item \code{site}: \code{title} and path to \code{root}.
#'   \item \code{yaml}: the \code{template} key from
#'      \code{_pkgdown.yml}.
#'   \item \code{package}: package metadata including \code{name} and
#'      \code{version}.
#'   }
#'
#'   See the full contents by running \code{data_template()}.
#' @param path Location to create file. If \code{""} (the default),
#'   prints to standard out.
#' @param depth Depth of path relative to base directory.
#' @export
render_page <- function(pkg = ".", name, data, path = "", depth = 0L) {
  pkg <- as_pkgdown(pkg)
  path <- rel_path(path, pkg$path)

  data <- utils::modifyList(data, data_template(pkg, depth = depth))

  # render template components
  pieces <- c("head", "navbar", "header", "content", "footer")

  components <- pieces %>%
    purrr::map_chr(find_template, name, template_path = template_path(pkg)) %>%
    purrr::map(render_template, data = data) %>%
    purrr::set_names(pieces)
  components$template <- name

  find_template("layout", name, template_path = template_path(pkg)) %>%
    render_template(components) %>%
    write_if_different(path)
}

#' @export
#' @rdname render_page
data_template <- function(pkg = ".", depth = 0L) {

  strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

  pkg <- as_pkgdown(pkg)
  desc <- pkg$desc
  name <- desc$get("Package")[[1]]
  authors <- data_authors(pkg)$main %>%
    purrr::map_chr("name") %>%
    paste(collapse = ", ")

  authors <- strReverse(sub('< ,>', '< & >', strReverse(authors)))

  # Force inclusion so you can reliably refer to objects inside yaml
  # in the moustache templates
  yaml <- pkg$meta[["template"]]$params %||% list()
  yaml$.present <- TRUE

  print_yaml(list(
    year = strftime(Sys.time(), "%Y"),
    package = list(
      name = name,
      version = desc$get("Version")[[1]],
      authors = authors
    ),
    site = list(
      root = up_path(depth),
      title = pkg$meta$title %||% name
    ),
    navbar = data_navbar(pkg, depth = depth),
    yaml = yaml
  ))
}

template_path <- function(pkg = ".") {
  pkg <- as_pkgdown(pkg)

  template <- pkg$meta[["template"]]

  if (!is.null(template$path)) {
    path <- rel_path(pkg$path, template$path)

    if (!file.exists(path))
      stop("Can not find template path '", path, "'", call. = FALSE)

    path
  } else if (!is.null(template$package)) {
    package_path(template$package, "templates")
  } else {
    character()
  }
}

render_template <- function(path, data) {
  template <- readLines(path)
  if (length(template) == 0)
    return("")

  whisker::whisker.render(template, data)
}

find_template <- function(type, name, template_path = NULL) {
  paths <- c(
    template_path,
    file.path(inst_path(), "templates")
  )
  names <- c(
    paste0(type, "-", name, ".html"),
    paste0(type, ".html")
  )
  all <- expand.grid(path = paths, name = names)
  locations <- file.path(all$path, all$name)

  Find(file.exists, locations, nomatch =
    stop("Can't find template for ", type, "-", name, ".", call. = FALSE))
}


write_if_different <- function(contents, path) {
  if (!made_by_pkgdown(path)) {
    message("Skipping '", path, "': not generated by pkgdown")
    return(FALSE)
  }

  if (same_contents(path, contents)) {
    return(FALSE)
  }

  message("Writing '", path, "'")
  cat(contents, file = path)
  TRUE
}

same_contents <- function(path, contents) {
  if (!file.exists(path))
    return(FALSE)

  # contents <- paste0(paste0(contents, collapse = "\n"), "\n")

  text_hash <- digest::digest(contents, serialize = FALSE)
  file_hash <- digest::digest(file = path)

  identical(text_hash, file_hash)
}

made_by_pkgdown <- function(path) {
  if (!file.exists(path)) return(TRUE)

  first <- paste(readLines(path, n = 2), collapse = "\n")
  check_made_by(first)
}

check_made_by <- function(first) {
  if (length(first) == 0L) return(FALSE)
  grepl("<!-- Generated by pkgdown", first, fixed = TRUE)
}
