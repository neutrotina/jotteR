#' Show open notebook notes
#'
#' Prints all currently open notebook notes.
#'
#' @param note_dir Directory containing the notebook files.
#'
#' @return The open notes, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' show_notes()
#' }
show_notes <- function(
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  if (length(state$notes) == 0L) {
    cat("No open notes.\n")
    return(
      invisible(state$notes)
    )
  }

  for (i in seq_along(state$notes)) {
    entry <- state$notes[[i]]

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

    cat(
      sprintf(
        "[%d] %s%s\n",
        i,
        entry$id,
        title
      )
    )

    cat(
      entry$text,
      "\n\n",
      sep = ""
    )
  }

  invisible(state$notes)
}

#' Show completed notebook notes
#'
#' Prints all notes that have been marked as completed.
#'
#' @param note_dir Directory containing the notebook files.
#'
#' @return The completed notes, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' show_done_notes()
#' }
show_done_notes <- function(
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  if (length(state$done) == 0L) {
    cat("No done notes.\n")
    return(
      invisible(state$done)
    )
  }

  for (i in seq_along(state$done)) {
    entry <- state$done[[i]]

    cat(
      sprintf(
        "[%d] %s\n%s\n\n",
        i,
        entry$id,
        entry$text
      )
    )
  }

  invisible(state$done)
}

#' Show general notebook notes
#'
#' Prints all general notebook notes.
#'
#' @param note_dir Directory containing the notebook files.
#'
#' @return The general notes, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' show_general_notes()
#' }
show_general_notes <- function(
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  if (length(state$general) == 0L) {
    cat("No general notes.\n")
    return(
      invisible(state$general)
    )
  }

  for (i in seq_along(state$general)) {
    entry <- state$general[[i]]

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

    cat(
      sprintf(
        "[%d] %s%s\n%s\n\n",
        i,
        entry$date,
        title,
        entry$text
      )
    )
  }

  invisible(state$general)
}
#' Mark a notebook note as completed
#'
#' Moves an open note to the completed-note collection and saves the
#' notebook state immediately.
#'
#' @param index_or_id Numeric index or character ID of an open note.
#' @param note_dir Directory containing the notebook files.
#'
#' @return The completed note ID, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' mark_note_done("high_count_check")
#' }
mark_note_done <- function(
    index_or_id,
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  index <- .resolve_entry(
    state$notes,
    index_or_id
  )

  entry <- state$notes[[index]]
  entry$completed <- Sys.time()

  state$notes <- state$notes[-index]
  state$done <- c(
    state$done,
    list(entry)
  )

  .save_notebook_state(
    state,
    note_dir
  )

  .write_notebook_text(
state,
    note_dir
  )

  invisible(entry$id)
}

#' Remove an open notebook note
#'
#' Permanently removes an open note and saves the notebook state immediately.
#'
#' @param index_or_id Numeric index or character ID of an open note.
#' @param note_dir Directory containing the notebook files.
#'
#' @return The removed note ID, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' remove_note("high_count_check")
#' }
remove_note <- function(
    index_or_id,
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  index <- .resolve_entry(
    state$notes,
    index_or_id
  )

  removed_id <- state$notes[[index]]$id

  state$notes <- state$notes[-index]

  .save_notebook_state(
    state,
    note_dir
  )

  .write_notebook_text(
    state,
    note_dir
  )

  invisible(removed_id)
}
#' Save the notebook state and text export
#'
#' Explicitly saves the durable notebook state and rewrites the
#' human-readable text export. Notes are also saved automatically after
#' each mutation.
#'
#' @param note_dir Directory containing the notebook files.
#' @param filename Name of the human-readable text export.
#'
#' @return The path to the text export, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' save_all_notes()
#' save_all_notes(filename = "analysis_notes.txt")
#' }
save_all_notes <- function(
    note_dir = notebook_dir(),
    filename = "notes.txt"
  ) {
  state <- .load_notebook_state(note_dir)

  .save_notebook_state(
    state,
    note_dir
  )

  output_file <- .write_notebook_text(
    state,
    note_dir,
    filename
  )

  cat(
    sprintf(
      "Notebook saved to: %s\n",
      output_file
    )
  )

  invisible(output_file)
}


#' Reload the project notebook
#'
#' Reloads the durable notebook state from the RDS state file.
#'
#' @param note_dir Directory containing the notebook files.
#'
#' @return The notebook state, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' reload_notes()
#' }
reload_notes <- function(
    note_dir = notebook_dir()
  ) {
  paths <- .notebook_paths(note_dir)

  if (!file.exists(paths$state)) {
    cat(
      "No notebook state file found.\n"
    )

    return(
      invisible(.empty_notebook_state())
    )
  }

  state <- .load_notebook_state(note_dir)

  cat(
    sprintf(
      "Reloaded %d open note(s), %d done note(s), and %d general note(s).\n",
      length(state$notes),
      length(state$done),
      length(state$general)
    )
  )

  invisible(state)
}
