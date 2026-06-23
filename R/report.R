# Report bundling helpers
#
# Adapted from shinymeta's report.R, archive.R, and utils.R
# (https://github.com/rstudio/shinymeta), Copyright (c) Joe Cheng,
# Carson Sievert, and RStudio (now Posit Software, PBC);
# released under the MIT license

# Expand an Rmd template against vars, then render it and zip the
# result with any include_files. On shinylive the expanded template
# is written out directly, because system zip is unavailable.
buildRmdBundle <- function(report_template, output_zip_path, vars = list(),
                           include_files = list(), render = TRUE,
                           render_args = list()) {
  force(report_template)
  force(vars)

  with_progress_obj(function(progress) {
    progress$set(value = 0)
    progress$set(message = "Generating code")

    if (is.list(vars)) {
      vars <- lapply(vars, function(x) {
        if (is.language(x)) {
          paste(shinymeta::formatCode(x), collapse = "\n")
        } else {
          x
        }
      })
    }

    progress$set(value = 0.1)
    progress$set(message = "Expanding Rmd template")

    rmd_source <- knit_expand_safe(report_template, vars = vars)
    rmd_filename <- template_rename(report_template, "Rmd")

    # WG: can't bundle with shinylive because system(zip) doesn't work
    if (is_shinylive()) {
      progress$set(value = 1)
      writeLines(rmd_source, output_zip_path)
      return(invisible(output_zip_path))
    } else {
      build_bundle(rmd_source, rmd_filename, output_zip_path,
                   include_files = include_files, render = render,
                   render_args = render_args, progress = progress)
    }
  })
}

# Stage the report and any included files in a private temp dir, then zip it.
build_bundle <- function(input_src, input_filename, output_zip_path,
                         include_files = list(), render = TRUE,
                         render_args = list(), progress) {
  force(input_src)
  force(input_filename)
  force(output_zip_path)
  force(include_files)
  force(render)
  force(render_args)

  # TODO: validate args
  progress$set(value = 0.2)
  progress$set(message = "Adding items to zip archive")

  # owner-only staging dir; removed once the bundle has been written
  basedir <- tempfile("archive")
  fs::dir_create(basedir, mode = "u=rwx,go=")
  on.exit(unlink(basedir, recursive = TRUE), add = TRUE)

  dest_filename_full <- fs::path(basedir, input_filename)

  # TODO: Verify UTF-8 encoding is preserved
  writeLines(input_src, dest_filename_full)

  add_items(basedir, include_files)

  progress$set(value = 0.3)

  if (render) {
    progress$set(message = "Rendering report")
    # WG: fork = TRUE doesn't work with shinylive because of callr::r
    render_report(dest_filename_full, render_args, fork = !is_shinylive())
  }

  progress$set(value = 0.9)
  progress$set(message = "Compressing bundle")
  archive <- build_archive(basedir, output_zip_path)
  progress$set(value = 1)
  archive
}

# Copy include_files into the staging dir. include_files is a named list whose
# names are the relative destination paths in the bundle and whose values are
# the source paths on disk (an empty value means destination == source).
add_items <- function(basedir, include_files) {
  if (length(include_files) == 0) {
    return(invisible(basedir))
  }
  to <- names(include_files)
  if (is.null(to)) {
    to <- as.character(include_files)
  }
  for (i in seq_along(include_files)) {
    from <- include_files[[i]]
    if (length(from) == 0 || !nzchar(from)) {
      from <- to[[i]]
    }
    add_item(basedir, from, to[[i]])
  }
  invisible(basedir)
}

# Copy a single file or directory to the relative path `target_file` inside the
# staging dir. A trailing slash on target_file means "into this directory".
add_item <- function(basedir, source_file, target_file) {
  stopifnot(is.character(source_file), length(source_file) == 1,
            is.character(target_file), length(target_file) == 1)
  if (fs::is_absolute_path(target_file)) {
    stop("target_file must be a relative path")
  }

  full_src <- fs::path_abs(source_file)

  if (fs::dir_exists(full_src)) {
    full_dest <- fs::path(basedir, target_file)
    fs::dir_copy(full_src, full_dest)
  } else {
    if (grepl("[/\\]$", target_file)) {
      # target names a directory, so copy the file into it (keep its own name)
      target_file <- fs::path(target_file, fs::path_file(source_file))
    }
    full_dest <- fs::path(basedir, target_file)
    if (!fs::path_dir(target_file) %in% c("", ".")) {
      fs::dir_create(fs::path_dir(full_dest), recurse = TRUE)
    }
    fs::file_copy(full_src, full_dest)
  }

  invisible(basedir)
}

# Zip the staging dir's contents (paths relative to the dir) into output_file.
build_archive <- function(basedir, output_file) {
  olddir <- getwd()
  setwd(basedir)
  on.exit(setwd(olddir))
  utils::zip(fs::path_abs(output_file, olddir), ".")
  invisible(output_file)
}

# Render input_file in its own directory so the outputs land beside it. When
# fork = TRUE, render in a separate process (via callr) so the report's code
# can't pollute the running app's session; see rstudio/rmarkdown#1204.
render_report <- function(input_file, render_args = list(), fork = TRUE) {
  old_wd <- setwd(fs::path_dir(input_file))
  on.exit(setwd(old_wd))

  if (fork) {
    callr::r(
      function(...) rmarkdown::render(...),
      args = c(list(input_file, envir = globalenv()), render_args)
    )
  } else {
    do.call(rmarkdown::render, c(list(input_file), render_args), quote = TRUE)
  }
}

# Run callback(progress) with a shiny Progress object (or a no-op stand-in when
# there is no reactive session). Closes any stale progress left from a previous
# failed run, shows errors in the progress bar, then re-raises.
with_progress_obj <- function(callback) {
  # Note that `session` may be NULL.
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session$userData$shinypal_last_progress)) {
    suppressWarnings(session$userData$shinypal_last_progress$close())
  }

  progress <- make_progress()
  session$userData$shinypal_last_progress <- progress

  tryCatch(shiny::captureStackTraces({
    callback(progress)
    progress$close()
    session$userData$shinypal_last_progress <- NULL
  }), error = function(err) {
    progress$set(value = 1, message = "An error has occurred:",
                 detail = conditionMessage(err))
    stop(err)
  })
}

# A shiny::Progress when there is a session, otherwise a no-op stand-in.
make_progress <- function(...) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    shiny::Progress$new(session = session, ...)
  } else {
    nothing <- function(...) {}
    list(
      set = nothing,
      inc = nothing,
      getMin = nothing,
      getMax = nothing,
      getValue = nothing,
      close = nothing,
      clone = nothing
    )
  }
}

# Strip the directory and one extension, then ensure a single `extension`:
#   /foo/report.Rmd.in => report.Rmd
#   /foo/report.Rmd    => report.Rmd
#   /foo/report        => report.Rmd
template_rename <- function(input_template, extension = "Rmd") {
  stopifnot(is.character(extension), length(extension) == 1, nzchar(extension))

  filename <- fs::path_ext_remove(fs::path_file(input_template))
  if (tolower(fs::path_ext(filename)) == tolower(extension)) {
    filename
  } else {
    paste0(filename, ".", extension)
  }
}

# knitr::knit_expand() that (1) evaluates {{expr}} against `vars` plus the
# global env and (2) refuses to expand if doing so adds or removes a knitr
# code-chunk delimiter, so user-supplied values can't inject new chunks.
# Adapted from shinymeta:::knit_expand_safe().
knit_expand_safe <- function(file, vars = list(),
                             text = xfun::read_utf8(file),
                             delim = c("{{", "}}")) {
  # knitr's markdown chunk-delimiter patterns
  patterns <- unname(unlist(knitr::all_patterns$md))

  matches_before <- count_matches_by_pattern(text, patterns)

  # an environment holding the template vars (plus the global env)
  eval_envir <- list2env(vars, parent = globalenv())

  # force inline {{/}} evaluation to use eval_envir, not knit_expand's caller
  orig_eval_inline <- knitr::knit_hooks$get("evaluate.inline")
  knitr::knit_hooks$set(evaluate.inline = function(code, envir) {
    orig_eval_inline(code, eval_envir)
  })
  on.exit(knitr::knit_hooks$set(evaluate.inline = orig_eval_inline), add = TRUE)

  res <- knitr::knit_expand(text = text, delim = delim)

  matches_after <- count_matches_by_pattern(xfun::split_lines(res), patterns)

  if (!identical(matches_before, matches_after)) {
    # expansion introduced or removed a code chunk
    stop("Can't build report--user input values must not contain code chunk ",
         "delimiters")
  }

  res
}

# Total matches of each `pattern` across the character vector `string`. A base-
# regex equivalent of shinymeta:::count_matches_by_pattern().
count_matches_by_pattern <- function(string, pattern) {
  vapply(pattern, function(regex) {
    hits <- gregexpr(regex, string)
    sum(vapply(hits, function(h) if (h[1L] == -1L) 0L else length(h),
               integer(1)))
  }, integer(1), USE.NAMES = FALSE)
}
