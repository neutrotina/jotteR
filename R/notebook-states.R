
.empty_notebook_state <- function() {
  list(
    notes = list(),
    done = list(),
    general = list()
  )
}


.load_notebook_state <- function(
note_dir = notebook_dir()
) {
  paths <- .notebook_paths(note_dir)

  if (!file.exists(paths$state)) {
    return(
      .empty_notebook_state()
    )
  }

  state <- readRDS(paths$state)

  required <- c(
    "notes",
    "done",
    "general"
  )

  if (
    !is.list(state) ||
      !all(required %in% names(state))
  ) {
    stop(
      "The notebook state file has an invalid format.",
      call. = FALSE
    )
  }

  state
}


.save_notebook_state <- function(
    state,
    note_dir = notebook_dir()
  ) {
  paths <- .notebook_paths(note_dir)

  temporary_file <- tempfile(
    pattern = ".notebook_state_",
    tmpdir = paths$directory,
    fileext = ".rds"
  )

  saveRDS(
    object = state,
    file = temporary_file
  )

  if (
    !file.rename(
      from = temporary_file,
      to = paths$state
    )
  ) {
    unlink(temporary_file)

    stop(
      "Could not replace the notebook state file.",
      call. = FALSE
    )
  }

  invisible(paths$state)
}


.note_text <- function(note) {
  if (!is.character(note)) {
    stop(
      "`note` must be a character vector.",
      call. = FALSE
    )
  }

  if (length(note) == 0L) {
    stop(
      "`note` cannot be empty.",
      call. = FALSE
    )
  }

  note <- paste(
    note,
    collapse = "\n"
  )

  if (!nzchar(trimws(note))) {
    stop(
      "`note` cannot be empty or whitespace-only.",
      call. = FALSE
    )
  }

  note
}


.new_id <- function(
    prefix,
    entries
  ) {
  existing_ids <- vapply(
    entries,
    function(entry) entry$id,
    character(1L)
  )

  counter <- length(entries) + 1L

  candidate <- sprintf(
    "%s_%03d",
    prefix,
    counter
  )

  while (candidate %in% existing_ids) {
    counter <- counter + 1L

    candidate <- sprintf(
      "%s_%03d",
      prefix,
      counter
    )
  }

  candidate
}


.resolve_entry <- function(
    entries,
    index_or_id
  ) {
  if (length(entries) == 0L) {
    stop(
      "There are no entries.",
      call. = FALSE
    )
  }

  if (length(index_or_id) != 1L) {
    stop(
      "`index_or_id` must have length one.",
      call. = FALSE
    )
  }

  if (is.numeric(index_or_id)) {
    index <- as.integer(index_or_id)

    if (
      is.na(index) ||
        index < 1L ||
        index > length(entries)
    ) {
      stop(
        "Invalid note index.",
        call. = FALSE
      )
    }

    return(index)
  }

  if (is.character(index_or_id)) {
    ids <- vapply(
      entries,
      function(entry) entry$id,
      character(1L)
    )

    index <- match(
      index_or_id,
      ids
    )

    if (!is.na(index)) {
      return(index)
    }

    titles <- vapply(
      entries,
function(entry) {
        if (is.null(entry$title)) {
          ""
        } else {
          entry$title
        }
      },
      character(1L)
    )

    index <- match(
      index_or_id,
      titles
    )

    if (!is.na(index)) {
      return(index)
    }
  }

  stop(
    "No note was found with that index or ID.",
    call. = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


.write_notebook_text <- function(
    state,
    note_dir = notebook_dir(),

    filename = "notes.txt"
  ) {
  paths <- .notebook_paths(note_dir)
  output_file <- file.path(paths$directory, filename)

  output <- c(
    sprintf(
      "Notebook export: %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    "",
    "NOT DONE:"
  )

  if (length(state$notes) == 0L) {
    output <- c(
      output,
      "  (none)"
    )
  } else {
    for (entry in state$notes) {
      title <- if (
        is.null(entry$title)
      ) {
        ""
      } else {
        paste0(
          " | ",
          entry$title
        )
      }

      output <- c(
        output,
        sprintf(
          "  [%s]%s",
          entry$id,
          title
        ),
        strsplit(
          entry$text,
          "\n",
          fixed = TRUE
        )[[1L]],
        ""
      )
    }
  }

  output <- c(
    output,
    "DONE:"
  )

  if (length(state$done) == 0L) {
    output <- c(
      output,
      "  (none)"
    )
  } else {
    for (entry in state$done) {
      output <- c(
        output,
        sprintf(
          "  [%s] %s",
          entry$id,
          entry$title %||% ""
        ),
        strsplit(
          entry$text,
          "\n",
          fixed = TRUE
        )[[1L]],
        ""
      )
    }
  }

  output <- c(
    output,
    "GENERAL NOTES:"
  )

  if (length(state$general) == 0L) {
    output <- c(
      output,
      "  (none)"
    )
  } else {
    for (entry in state$general) {
      title <- if (
        is.null(entry$title)
      ) {
        ""
      } else {
        entry$title
      }

      output <- c(
        output,
        sprintf(
          "  [%s] %s | %s",
          entry$id,
          entry$date,
          title
        ),
        strsplit(
          entry$text,
          "\n",
          fixed = TRUE
        )[[1L]],
        ""
      )
    }
  }

  writeLines(
    text = output,
    con = output_file,
    useBytes = TRUE
  )

  invisible(output_file)
}
