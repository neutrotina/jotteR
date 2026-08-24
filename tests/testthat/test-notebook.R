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
      state$tasks,
      0L
    )

    expect_length(
      state$done,
      0L
    )
  }
)


test_that(
  "adding a note saves it immediately",
  {
    note_dir <- new_test_notebook_dir()

    id <- add_note(
      note = "The final QC retained 24,201 cells.",
      id = "result_1",
      title = "Final QC",
      note_dir = note_dir
    )

    expect_identical(
      id,
      "result_1"
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

    expect_length(
      state$tasks,
      0L
    )

    expect_identical(
      state$notes[[1L]]$id,
      "result_1"
    )

    expect_identical(
      state$notes[[1L]]$text,
      "The final QC retained 24,201 cells."
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
  "notes can be printed by ID",
  {
    note_dir <- new_test_notebook_dir()

    add_note(
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
  "adding a task saves it separately from notes",
  {
    note_dir <- new_test_notebook_dir()

    id <- add_task(
      task = "Complete the neutrophil QC audit.",
      id = "neutrophil_qc",
      title = "Neutrophil QC",
      note_dir = note_dir
    )

    expect_identical(
      id,
      "neutrophil_qc"
    )

    state <- reload_notes(
      note_dir = note_dir
    )

    expect_length(
      state$notes,
      0L
    )

    expect_length(
      state$tasks,
      1L
    )

    expect_identical(
      state$tasks[[1L]]$id,
      "neutrophil_qc"
    )

    expect_identical(
      state$tasks[[1L]]$text,
      "Complete the neutrophil QC audit."
    )
  }
)


test_that(
  "tasks can be marked as done and reloaded",
  {
    note_dir <- new_test_notebook_dir()

    add_task(
      task = "Complete this task.",
      id = "task_1",
note_dir = note_dir
    )

    completed_id <- mark_task_done(
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
      state$tasks,
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
  "tasks can be removed by ID",
  {
    note_dir <- new_test_notebook_dir()

    add_task(
      task = "Temporary task.",
      id = "temporary_task",
      note_dir = note_dir
    )

    removed_id <- remove_task(
      index_or_id = "temporary_task",
      note_dir = note_dir
    )

    expect_identical(
      removed_id,
      "temporary_task"
    )

    state <- reload_notes(
      note_dir = note_dir
    )

    expect_length(
      state$tasks,
      0L
    )
  }
)


test_that(
  "duplicate entry IDs are rejected",
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
  "duplicate IDs are rejected across notes and tasks",
  {
    note_dir <- new_test_notebook_dir()

    add_note(
      note = "A general note.",
      id = "shared_id",
      note_dir = note_dir
    )

    expect_error(
      add_task(
        task = "A task with the same ID.",
        id = "shared_id",
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
