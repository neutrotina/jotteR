new_test_notebook_dir <- function() {
  path <- tempfile(
    pattern = "jotteR-notebook-"
  )

  dir.create(
    path = path,
    recursive = TRUE,
    showWarnings = FALSE
  )

  path
}

test_that(
  "a new notebook starts empty",
  {
    note_dir <- new_test_notebook_dir()

    state <- reload_notes(
      note_dir = note_dir
    )

    expect_type(
      state,
      "list"
    )

    expect_length(
      state$notes,
      0L
    )

    expect_length(
      state$done,
      0L
    )

    expect_length(
      state$general,
      0L
    )
  }
)


test_that(
  "adding a note saves it immediately",
  {
    note_dir <- new_test_notebook_dir()

    id <- add_note(
      note = "Check the macrophage markers.",
      id = "marker_check",
      title = "Macrophage markers",
      note_dir = note_dir
    )

    expect_identical(
      id,
      "marker_check"
    )

    expect_true(
      file.exists(
        file.path(
          note_dir,
          ".notebook_state.rds"
        )
      )
    )

    expect_true(
      file.exists(
        file.path(
          note_dir,
          "notes.txt"
        )
      )
    )

    state <- reload_notes(
      note_dir = note_dir
    )

    expect_length(
      state$notes,
      1L
    )

    expect_identical(
      state$notes[[1L]]$id,
      "marker_check"
    )

    expect_identical(
      state$notes[[1L]]$text,
      "Check the macrophage markers."
    )
  }
)


test_that(
  "multiline notes are preserved",
  {
    note_dir <- new_test_notebook_dir()

    add_note(
      note = c(
        "First result.",
        "Second result.",
        "Interpret cautiously."
      ),
      id = "multiline",
      note_dir = note_dir
    )

    state <- reload_notes(
      note_dir = note_dir
    )

    expect_identical(
      state$notes[[1L]]$text,
      paste(
        c(
          "First result.",
          "Second result.",
          "Interpret cautiously."
        ),
        collapse = "\n"
      )
    )
  }
)


test_that(
  "general notes can be printed by ID",
  {
    note_dir <- new_test_notebook_dir()

    add_general_note(
      note = c(
        "The final QC retained 24,201 cells.",
        "High-count cells were retained."
      ),
      title = "Final QC",
      id = "result_1",
      note_dir = note_dir
    )

    printed <- utils::capture.output(
  print_note(
    "result_1",
    note_dir = note_dir
  )
)


    expect_true(
      any(
        grepl(
          "24,201 cells",
          printed,
          fixed = TRUE
        )
      )
    )
  }
)


test_that(
  "print_note prints supplied character objects",
  {
    note_dir <- new_test_notebook_dir()

    result_1 <- c(
      "Line one.",
      "Line two."
    )

    expect_output(
      print_note(
        result_1,
        note_dir = note_dir
      ),
      "Line one\\."
    )

    expect_output(
      print_note(
        result_1,
        note_dir = note_dir
      ),
      "Line two\\."
    )
  }
)


test_that(
  "notes can be marked as done and reloaded",
  {
    note_dir <- new_test_notebook_dir()

    add_note(
      note = "Complete this task.",
      id = "task_1",
      note_dir = note_dir
    )

    completed_id <- mark_note_done(
      index_or_id = "task_1",
      note_dir = note_dir
    )

    expect_identical(
      completed_id,
      "task_1"
    )

    state <- reload_notes(
      note_dir = note_dir
    )

    expect_length(
      state$notes,
      0L
    )

    expect_length(
      state$done,
      1L
    )

    expect_identical(
      state$done[[1L]]$id,
      "task_1"
    )
  }
)


test_that(
  "notes can be removed by ID",
  {
    note_dir <- new_test_notebook_dir()

    add_note(
      note = "Temporary note.",
      id = "temporary",
      note_dir = note_dir
    )

    removed_id <- remove_note(
      index_or_id = "temporary",
      note_dir = note_dir
    )

    expect_identical(
      removed_id,
      "temporary"
    )

    state <- reload_notes(
      note_dir = note_dir
    )

    expect_length(
      state$notes,
      0L
    )
  }
)


test_that(

"duplicate note IDs are rejected",
  {
    note_dir <- new_test_notebook_dir()

    add_note(
      note = "First note.",
      id = "duplicate",
      note_dir = note_dir
    )

    expect_error(
      add_note(
        note = "Second note.",
        id = "duplicate",
        note_dir = note_dir
      ),
      "already exists"
    )
  }
)


test_that(
  "saving repeatedly does not duplicate notes",
  {
    note_dir <- new_test_notebook_dir()

    add_note(
      note = "A note.",
      id = "note_1",
      note_dir = note_dir
    )

    save_all_notes(
      note_dir = note_dir
    )

    save_all_notes(
      note_dir = note_dir
    )

    state <- reload_notes(
      note_dir = note_dir
    )

    expect_length(
      state$notes,
      1L
    )

    expect_identical(
      state$notes[[1L]]$id,
      "note_1"
    )
  }
)
