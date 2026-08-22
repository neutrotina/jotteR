
#' Add an open notebook note
#'
#' Adds a note to the project notebook and saves the notebook state
#' immediately. Notes may contain multiple lines.
#'
#' @param note Character vector containing the note text.
#' @param id Optional stable identifier. If omitted, an identifier is generated.
#' @param title Optional short title.
#' @param note_dir Directory containing the notebook files.
#'
#' @return The note ID, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' add_note(
#'   note = "Check the high-count macrophage cells.",
#'   id = "high_count_check",
#'   title = "High-count cells"
#' )
#' }
add_note <- function(
    note,
    id = NULL,
    title = NULL,
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  if (is.null(id)) {
    id <- .new_id(
      prefix = "note",
      entries = state$notes
    )
  }

  if (
    length(id) != 1L ||
      !is.character(id) ||
      !nzchar(trimws(id))
  ) {
    stop(
      "`id` must be one non-empty character string.",
      call. = FALSE
    )
  }

  existing_ids <- vapply(
    state$notes,
    function(entry) entry$id,
    character(1L)
  )

  if (id %in% existing_ids) {
    stop(
      sprintf(
        "A note with ID `%s` already exists.",
        id
      ),
      call. = FALSE
    )
  }

  entry <- list(
    id = id,
    title = title,
    text = .note_text(note),
    created = Sys.time(),
    updated = Sys.time()
  )

  state$notes <- c(
    state$notes,
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

  invisible(id)
}

#' Add a general notebook note
#'
#' Adds a dated general note and saves the notebook state immediately.
#'
#' @param note Character vector containing the note text.
#' @param title Optional short title.
#' @param id Optional stable identifier.
#' @param date Date associated with the note.
#' @param note_dir Directory containing the notebook files.
#'
#' @return The note ID, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' add_general_note(
#'   note = "The final QC retained 24,201 cells.",
#'   title = "Final QC",
#'   id = "result_1"
#' )
#' }
add_general_note <- function(
    note,
    title = NULL,
    id = NULL,
    date = Sys.Date(),
    note_dir = notebook_dir()
  ) {
  state <- .load_notebook_state(note_dir)

  if (is.null(id)) {
    id <- .new_id(
      prefix = "general",
      entries = state$general
    )
  }

  existing_ids <- vapply(
    state$general,
    function(entry) entry$id,
    character(1L)
  )

  if (id %in% existing_ids) {
    stop(
      sprintf(
"A general note with ID `%s` already exists.",
        id
      ),
      call. = FALSE
    )
  }

  entry <- list(
    id = id,
    title = title,
    text = .note_text(note),
    date = as.character(date),
    created = Sys.time(),
    updated = Sys.time()
  )

  state$general <- c(
    state$general,
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

  invisible(id)
}
#' Print a notebook note
#'
#' Prints an open, completed, or general notebook note. A numeric input
#' selects an open note by its current index. A character input matching a
#' note ID or title retrieves the saved note. Other character input is
#' printed directly.
#'
#' @param note A numeric note index, a saved note ID or title, or a character
#'   object containing text to print.
#' @param note_dir Directory containing the notebook state file.
#'
#' @return The printed text, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' print_note("result_1")
#'
#' result_1 <- c(
#'   "The final QC retained 24,201 cells.",
#'   "High-count myeloid cells were retained."
#' )
#' print_note(result_1)
#' }
print_note <- function(
    note = NULL,
    note_dir = notebook_dir()
  ) {
  if (missing(note)) {
    stop(
      "Supply a note ID, note index, or character object.",
      call. = FALSE
    )
  }

  state <- .load_notebook_state(note_dir)

  if (is.numeric(note)) {
    index <- .resolve_entry(
      state$notes,
      note
    )

    text <- state$notes[[index]]$text

    cat(
      text,
      "\n",
      sep = ""
    )

    return(
      invisible(text)
    )
  }

  all_entries <- c(
    state$notes,
    state$done,
    state$general
  )

  if (
    is.character(note) &&
      length(note) == 1L
  ) {
    ids <- vapply(
      all_entries,
      function(entry) entry$id,
      character(1L)
    )

    titles <- vapply(
      all_entries,
      function(entry) entry$title %||% "",
      character(1L)
    )

    index <- match(
      note,
      c(ids, titles)
    )

    if (!is.na(index)) {
      text <- all_entries[[index]]$text

      cat(
        text,
        "\n",
        sep = ""
      )

      return(
        invisible(text)
      )
    }
  }

  text <- .note_text(note)

  cat(
    text,
    "\n",
    sep = ""
  )

  invisible(text)
}
