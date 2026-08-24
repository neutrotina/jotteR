#' Show open general notes
#'
#' Prints all currently open general notes.
#'
#' @param note_dir Directory containing the notebook files.
#'
#' @return The general notes, invisibly.
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
    cat("No notes.\n")

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

    date <- entry$date %||% ""

    cat(
      sprintf(
        "[%d] %s | %s%s\n",
        i,
        entry$id,
        date,
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


#' Show open notebook tasks
#'
#' Prints all currently open notebook tasks.
#'
#' @param note_dir Directory containing the notebook files.
#'
#' @return The open tasks, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' show_tasks()
#' }
show_tasks <- function(
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  if (length(state$tasks) == 0L) {
    cat("No open tasks.\n")

    return(
      invisible(state$tasks)
    )
  }

  for (i in seq_along(state$tasks)) {
    entry <- state$tasks[[i]]

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

  invisible(state$tasks)
}


#' Show completed notebook tasks
#'
#' Prints all tasks that have been marked as completed.
#'
#' @param note_dir Directory containing the notebook files.
#'
#' @return The completed tasks, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' show_done_tasks()
#' }
show_done_tasks <- function(
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  if (length(state$done) == 0L) {
    cat("No completed tasks.\n")

    return(
      invisible(state$done)
    )
  }

  for (i in seq_along(state$done)) {
    entry <- state$done[[i]]

    cat(
      sprintf(
        "[%d] %s%s\n%s\n\n",
        i,
        entry$id,
        if (
          is.null(entry$title)
        ) {
          ""
        } else {
          paste0(
            " | ",
            entry$title
          )
        },
        entry$text
      )
    )
  }

  invisible(state$done)
}


#' Mark a notebook task as completed
#'
#' Moves an open task to the completed-task collection and saves the
#' notebook state immediately.
#'
#' @param index_or_id Numeric index or character ID of an open task.
#' @param note_dir Directory containing the notebook files.
#'
#' @return The completed task ID, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' mark_task_done("high_count_check")
#' }
mark_task_done <- function(
    index_or_id,
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  index <- .resolve_entry(
    state$tasks,
    index_or_id
  )

  entry <- state$tasks[[index]]
  entry$completed <- Sys.time()

  state$tasks <- state$tasks[-index]

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


#' Remove an open general note
#'
#' Permanently removes an open general note and saves the notebook state
#' immediately.
#'
#' @param index_or_id Numeric index or character ID of an open note.
#' @param note_dir Directory containing the notebook files.
#'
#' @return The removed note ID, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' remove_note("result_1")
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


#' Remove an open notebook task
#'
#' Permanently removes an open task and saves the notebook state immediately.
#'
#' @param index_or_id Numeric index or character ID of an open task.
#' @param note_dir Directory containing the notebook files.
#'
#' @return The removed task ID, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' remove_task("high_count_check")
#' }
remove_task <- function(
    index_or_id,
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  index <- .resolve_entry(
    state$tasks,
    index_or_id
  )

  removed_id <- state$tasks[[index]]$id

  state$tasks <- state$tasks[-index]

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
#' human-readable text export. Notes and tasks are also saved automatically
#' after each mutation.
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
      "Reloaded %d note(s), %d open task(s), and %d completed task(s).\n",
      length(state$notes),
      length(state$tasks),
      length(state$done)
    )
  )

  invisible(state)
}
