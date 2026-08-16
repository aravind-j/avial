#' Generate Zenodo metadata
#'
#' Generate a \code{.zenodo.json} file from either an R package DESCRIPTION file
#' or a Citation File Format (CFF) file.
#'
#' @param description_file Path to the \code{DESCRIPTION} file.
#' @param cff_file Path to the \code{CITATION.cff} file.
#' @param output_file Path to the generated Zenodo JSON file.
#' @param from Source of metadata: either \code{"description"} or \code{"cff"}.
#' @param github_topics Logical; if \code{TRUE}, attempt to retrieve GitHub
#'   repository topics and add them as keywords.
#'
#' @return Invisibly returns the generated Zenodo metadata as a list.
#'
#' @importFrom httr2 request req_headers req_perform resp_body_json
#' @importFrom desc desc
#' @importFrom jsonlite write_json
#' @importFrom yaml read_yaml
#'
#' @export
generate_zenodo_json <- function(
    description_file = "DESCRIPTION",
    cff_file = "CITATION.cff",
    output_file = ".zenodo.json",
    from = c("description", "cff"),
    github_topics = TRUE) {

  from <- match.arg(from)

  # Helpers ----

  normalize_orcid <- function(orcid) {
    if (is.null(orcid) || length(orcid) == 0 || is.na(orcid)) {
      return(NULL)
    }

    orcid <- trimws(as.character(orcid)[1])

    if (!nzchar(orcid)) {
      return(NULL)
    }

    sub("^https?://orcid\\.org/", "", orcid)
  }

  normalize_character <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NULL)
    }

    x <- as.character(x)
    x <- trimws(x)
    x <- x[nzchar(x) & !is.na(x)]

    if (length(x) == 0) {
      return(NULL)
    }

    x
  }

  normalize_license <- function(license) {
    if (is.null(license) || length(license) == 0) {
      return(NULL)
    }

    license <- trimws(as.character(license))

    # Handle vectors from CFF
    if (length(license) > 1) {

      if (setequal(license, c("GPL-2.0-only", "GPL-3.0-only"))) {
        return("GPL-2.0-or-later")
      }

      # Zenodo's license field is singular
      warning("Multiple licenses were found: ",
              paste(license, collapse = ", "),
              ". Zenodo requires a single license identifier; ",
              "using the first recognized license.", call. = FALSE)

      license <- license[[1]]
    }

    # Normalize DESCRIPTION expressions
    license <- trimws(license[[1]])

    if (grepl("^GPL-2\\s*\\|\\s*GPL-3$", license, ignore.case = TRUE)) {
      return("GPL-2.0-or-later")
    }

    if (grepl("GPL-3", license, ignore.case = TRUE)) {
      return("GPL-3.0-only")
    }

    if (grepl("GPL-2", license, ignore.case = TRUE)) {
      return("GPL-2.0-only")
    }

    if (grepl("^MIT$", license, ignore.case = TRUE)) {
      return("MIT")
    }

    if (grepl("Apache", license, ignore.case = TRUE)) {
      return("Apache-2.0")
    }

    # Assume an existing SPDX identifier
    license
  }

  normalize_author <- function(
    given = NULL,
    family = NULL,
    orcid = NULL,
    affiliation = NULL) {

    given <- normalize_character(given)
    family <- normalize_character(family)
    affiliation <- normalize_character(affiliation)
    orcid <- normalize_orcid(orcid)

    given <- if (is.null(given)) "" else paste(given, collapse = " ")
    family <- if (is.null(family)) "" else paste(family, collapse = " ")

    name <- if (nzchar(family) && nzchar(given)) {
      paste(family, given, sep = ", ")
    } else if (nzchar(family)) {
      family
    } else {
      given
    }

    creator <- list(name = name)

    if (!is.null(orcid)) {
      creator$orcid <- orcid
    }

    if (!is.null(affiliation)) {
      creator$affiliation <- paste(affiliation, collapse = "; ")
    }

    creator
  }

  split_urls <- function(urls) {
    urls <- normalize_character(urls)

    if (is.null(urls)) {
      return(character())
    }

    urls <- unlist(strsplit(urls, ",", fixed = TRUE), use.names = FALSE)

    urls <- trimws(urls)
    urls[nzchar(urls)]
  }

  normalize_keywords <- function(keywords) {
    keywords <- normalize_character(keywords)

    if (is.null(keywords)) {
      return(character())
    }

    unique(keywords)
  }

  extract_github_url <- function(urls) {
    urls <- normalize_character(urls)

    if (is.null(urls)) {
      return(NULL)
    }

    github_urls <- urls[
      grepl("^https://github\\.com/[^/]+/[^/]+/?$",
            urls,
            ignore.case = TRUE)]

    if (length(github_urls) == 0) {
      return(NULL)
    }

    github_urls[[1]]
  }

  get_github_topics <- function(url) {
    if (is.null(url) || !nzchar(url)) {
      return(character())
    }

    repo <- sub("^https://github\\.com/", "", url, ignore.case = TRUE)

    repo <- sub("/$", "", repo)

    api_url <- paste0("https://api.github.com/repos/", repo, "/topics")

    tryCatch(
      {
        request <- httr2::request(api_url)
        request <-
          httr2::req_headers(request,
                             Accept = "application/vnd.github+json")
        response <- httr2::req_perform(request)

        topics <- httr2::resp_body_json(response)$names

        normalize_keywords(topics)
      },
      error = function(e) {
        warning("Could not retrieve GitHub topics for '", url, "': ",
                conditionMessage(e), call. = FALSE)

        character()
      })
  }

  # Metadata readers ----

  read_description_metadata <- function(file) {

    if (!file.exists(file)) {
      stop("DESCRIPTION file does not exist: ", file, call. = FALSE)
    }

    pkg <- desc::desc(file = file)

    authors <- pkg$get_authors()

    creators <-
      lapply(authors,
             function(auth) {
               comments <- auth$comment

               orcid <- NULL
               affiliation <- NULL

               if (!is.null(comments)) {
                 if ("ORCID" %in% names(comments)) {
                   orcid <- comments[["ORCID"]]
                 }

                 if ("affiliation" %in% names(comments)) {
                   affiliation <- comments[["affiliation"]]
                 }
               }

               normalize_author(given = auth$given,
                                family = auth$family,
                                orcid = orcid,
                                affiliation = affiliation)
             })

    creators <- Filter(function(x) {
      !is.null(x$name) && nzchar(x$name)
    }, creators)

    urls <- split_urls(pkg$get("URL"))

    list(package = normalize_character(pkg$get("Package")),
         title = normalize_character(pkg$get("Title")),
         description = normalize_character(pkg$get("Description")),
         version = normalize_character(pkg$get("Version")),
         creators = creators,
         license = normalize_license(pkg$get("License")),
         keywords = character(), urls = urls)
  }

  read_cff_metadata <- function(file) {

    if (!file.exists(file)) {
      stop("CFF file does not exist: ", file, call. = FALSE)
    }

    cff <- yaml::read_yaml(file)

    authors <- cff$authors

    creators <- if (is.null(authors)) {
      list()
    } else {
      lapply(authors,
             function(auth) {
               normalize_author(given = auth[["given-names"]],
                                family = auth[["family-names"]],
                                orcid = auth$orcid,
                                affiliation = auth$affiliation)
             })
    }

    # CFF commonly stores the repository in repository-code.
    # Fall back to URL if available.
    urls <- normalize_character(c(cff$`repository-code`, cff$url))

    urls <- unique(urls[!is.na(urls) & nzchar(urls)])

    list(package = NULL, title = normalize_character(cff$title),
         description = normalize_character(cff$abstract),
         version = normalize_character(cff$version), creators = creators,
         license = normalize_license(cff$license),
         keywords = normalize_keywords(cff$keywords), urls = urls)
  }

  # Build Zenodo metadata from normalized metadata ----

  build_zenodo_metadata <- function(metadata) {

    if (is.null(metadata$title)) {
      stop("No title could be obtained from the source metadata.",
           call. = FALSE)
    }

    description <- metadata$description

    if (is.null(description)) {
      description <- "An R package."
    }

    zenodo <- list(title = metadata$title,
                   upload_type = "software", description = description,
                   creators = metadata$creators, access_right = "open")

    if (!is.null(metadata$license)) {
      zenodo$license <- metadata$license
    }

    if (!is.null(metadata$version)) {
      zenodo$version <- metadata$version
    }

    keywords <- metadata$keywords

    if (!is.null(metadata$package)) {
      keywords <- c(metadata$package, keywords)
    }

    keywords <- normalize_keywords(keywords)

    if (length(keywords) > 0) {
      zenodo$keywords <- keywords
    }

    # Convert source URLs to Zenodo related identifiers
    if (length(metadata$urls) > 0) {

      zenodo$related_identifiers <-
        lapply(metadata$urls,
               function(url) {
                 list(identifier = url, relation = "isSupplementTo",
                      resource_type = "software")
               })
    }

    # Optional GitHub topic enrichment
    if (github_topics) {
      github_url <- extract_github_url(metadata$urls)

      if (!is.null(github_url)) {
        topics <- get_github_topics(github_url)

        if (length(topics) > 0) {
          zenodo$keywords <-
            normalize_keywords(c(zenodo$keywords %||% character(), topics))
        }
      }
    }

    zenodo
  }

  # Small internal null-coalescing helper
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  # Read source metadata ----

  metadata <-
    switch(from, description = read_description_metadata(description_file),
           cff = read_cff_metadata(cff_file))

  # Generate Zenodo metadata ----

  zenodo_data <- build_zenodo_metadata(metadata)

  # Write JSON ----

  jsonlite::write_json(zenodo_data, path = output_file,
                       auto_unbox = TRUE,
                       pretty = TRUE, na = "null")

  source_file <- switch(from, description = description_file,
                        cff = cff_file)

  package_name <- metadata$package

  if (is.null(package_name)) {
    package_name <- metadata$title
  }

  message("Success! Zenodo metadata generated from ",
          source_file, " for '",
          package_name, "' -> ", output_file)

  invisible(zenodo_data)
}

# file_exist_abort <- utils::getFromNamespace("file_exist_abort", "cffr")
file_exist_abort <-  function(x, abort = FALSE) {
  res <- file.exists(x)
  if (all(abort, isFALSE(res))) {
    cli::cli_abort("{.file {x}} does not exist. Check the {.file {dirname(x)}} directory.")
  }
  invisible(res)
}

#' Install a GitHub Actions workflow to create a \code{.zenodo.json} file
#'
#' @description This function installs a
#'   \href{https://github.com/features/actions}{GitHub Actions} workflow in your
#'   repository. The workflow updates your
#'   \href{https://help.zenodo.org/docs/github/describe-software/zenodo-json/}{\code{.zenodo.json}}
#'   file from the package \code{DESCRIPTION} when any of these events occur:
#'   \itemize{
#'     \item You publish a new release of the package.
#'     \item Your \code{DESCRIPTION} or \code{inst/CITATION} file is modified.
#'     \item The action can be run manually.
#'   }
#'
#' @param path Project root directory.
#' @param overwrite A logical value. If \code{TRUE}, overwrite an existing
#'   workflow.
#'
#' @details Workflow triggers can be modified. See
#'   \href{https://docs.github.com/en/actions/learn-github-actions/events-that-trigger-workflows}{Events
#'   that trigger workflows}.
#'
#' @family git
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_abort
#'
#' @export
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' zenodo_gha_update()
#' }
zenodo_gha_update <- function(path = ".", overwrite = FALSE) {
  destdir <- file.path(path, ".github", "workflows")
  checkdir <- dir.exists(destdir)

  if (isFALSE(checkdir)) {
    cli::cli_alert_info("Creating directory {.path {destdir}}.")
    dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
  }

  newfile <- file.path(destdir, "update-zenodo-json.yaml")

  if (!file_exist_abort(newfile) || isTRUE(overwrite)) {
    cli::cli_alert_success("Workflow installed at {.file {newfile}}.")

    file.copy(
      system.file("yaml/update-zenodo-json.yaml", package = "avial"),
      newfile,
      overwrite = TRUE
    )
  } else {
    cli::cli_alert_warning(paste0(
      "Workflow file {.file {newfile}} already exists. ",
      "Set {.arg overwrite} to {.val TRUE} to overwrite it."
    ))
  }

  rbuildignore <- file.path(path, ".Rbuildignore")

  if (file_exist_abort(rbuildignore)) {
    ignore <- readLines(rbuildignore)

    # If not already present.
    if (!("^\\.github$" %in% ignore)) {
      ignore <- c(ignore, "^\\.github$")
      ignore <- unique(ignore)
      cli::cli_alert_info("Adding {.path .github} to {.file .Rbuildignore}.")
      writeLines(ignore, rbuildignore)
    }
  }

  invisible()
}

