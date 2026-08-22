#' Return the notebook directory
#'
#' Constructs the path to the project notebook directory. By default,
#' `project_dir` is the current working directory.
#'
#' @param project_dir Project directory. Defaults to `getwd()`.
#' @param folder Name of the notebook folder.
#'
#' @return A character path.
#' @export
#'
#' @examples
#' \dontrun{
#' notebook_dir()
#' notebook_dir(project_dir = "/path/to/project")
#' notebook_dir(folder = "analysis_notes")
#' }
notebook_dir <- function(
    project_dir = getwd(),
    folder = "notebook"
  ) {
  if (
    length(project_dir) != 1L ||
      !is.character(project_dir) ||
      is.na(project_dir) ||
      !nzchar(project_dir)
  ) {
    stop(
      "`project_dir` must be one non-empty character string.",
      call. = FALSE
    )
  }

  if (
    length(folder) != 1L ||
      !is.character(folder) ||
      is.na(folder) ||
      !nzchar(folder)
  ) {
    stop(
      "`folder` must be one non-empty character string.",
      call. = FALSE
    )
  }

  file.path(
    project_dir,
    folder
  )
}


.notebook_paths <- function(
note_dir = notebook_dir()
) {
  dir.create(
    path = note_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  list(
    directory = note_dir,
    state = file.path(
      note_dir,
      ".notebook_state.rds"
    ),
    text = file.path(
      note_dir,
      "notes.txt"
    )
  )
}
