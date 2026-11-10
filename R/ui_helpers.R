uploaded_path <- function(upload) {
  if (is.null(upload) || is.null(upload$datapath) || length(upload$datapath) == 0 ||
      is.na(upload$datapath[1]) || !nzchar(upload$datapath[1])) {
    return("")
  }
  as.character(upload$datapath[1])
}

projects_from_gl <- function(upload, gl = NULL) {
  path <- uploaded_path(upload)
  if (!nzchar(path) || !file.exists(path)) return(character())
  if (is.null(gl)) gl <- read_gl_export(path)
  normalized_names <- normalize_gl_headers(names(gl))
  project_column <- names(gl)[normalized_names == "grant_and_program_name"]
  projects <- trimws(as.character(gl[[project_column[1]]]))
  sort(unique(projects[!is.na(projects) & nzchar(projects)]))
}

projects_setting <- function(selected_projects, available_projects) {
  selected_projects <- trimws(as.character(selected_projects))
  selected_projects <- selected_projects[!is.na(selected_projects) & nzchar(selected_projects)]
  if (length(selected_projects) == 0 || length(available_projects) == 0 ||
      setequal(selected_projects, available_projects)) {
    return("All projects in GL")
  }
  paste(selected_projects, collapse = ";")
}

selected_project_values <- function(selected_projects, available_projects) {
  selected_projects <- trimws(as.character(selected_projects))
  selected_projects <- selected_projects[!is.na(selected_projects) & nzchar(selected_projects)]
  if (length(selected_projects) == 0 && length(available_projects) > 0) available_projects else selected_projects
}

suggest_mileage_sheet <- function(project, sheets) {
  if (length(sheets) == 0) return("")
  project_key <- gsub("[^a-z0-9]", "", tolower(project))
  sheet_keys <- gsub("[^a-z0-9]", "", tolower(sheets))
  contains_project <- vapply(sheet_keys, function(sheet_key) {
    grepl(project_key, sheet_key, fixed = TRUE) || grepl(sheet_key, project_key, fixed = TRUE)
  }, logical(1))
  if (any(contains_project)) return(sheets[which(contains_project)[1]])
  sheets[which.min(utils::adist(project_key, sheet_keys))]
}

mileage_mapping_id <- function(prefix, index) paste0(prefix, "_mileage_sheet_", index)

selected_mileage_sheet_map <- function(input, prefix, projects) {
  if (length(projects) == 0) return(character())
  mapped_sheets <- vapply(seq_along(projects), function(index) {
    value <- input[[mileage_mapping_id(prefix, index)]]
    if (is.null(value) || !nzchar(value)) "" else value
  }, character(1))
  stats::setNames(mapped_sheets[nzchar(mapped_sheets)], projects[nzchar(mapped_sheets)])
}

mileage_mapping_ui <- function(prefix, projects, sheets) {
  if (length(sheets) == 0) return(helpText("Upload a mileage workbook to choose its worksheets. CSV logs use their Project column instead."))
  if (length(projects) == 0) {
    return(div(class = "warning-note", "Select GL projects to match them to mileage worksheets."))
  }
  tagList(
    p("For each GL project below, choose the worksheet that contains its mileage. Worksheet names can differ from GL project names. Confirm each suggested match, or choose No mileage worksheet."),
    lapply(seq_along(projects), function(index) {
      selectInput(
        mileage_mapping_id(prefix, index),
        label = paste("Mileage worksheet for", projects[index]),
        choices = c("No mileage worksheet" = "", sheets),
        selected = suggest_mileage_sheet(projects[index], sheets)
      )
    })
  )
}
