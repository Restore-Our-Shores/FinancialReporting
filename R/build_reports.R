#' Build the same reports used by the Shiny app, without launching Shiny
#' @export
build_reports <- function(quarter_id, period_start, period_end,
  general_ledger, receipt_folder = "", mileage_log = "",
  included_projects = "All projects in GL", mileage_sheet_map = character(),
  mileage_rate = 0.765, fringe_rate = 0.18, indirect_rate = 0.10,
  root = "reports", project_output_root = "", delivery = "financial_report", prepare_only = FALSE) {
  if (blank_value(quarter_id)) stop("Report Name is required.")
  start <- as.Date(period_start)
  end <- as.Date(period_end)
  if (length(start) != 1L || length(end) != 1L || is.na(start) || is.na(end) || end < start) {
    stop("Supply valid reporting dates with end on or after start.")
  }
  if (blank_value(general_ledger) || !file.exists(general_ledger)) stop("General ledger is missing.")
  rates <- c(mileage_rate, fringe_rate, indirect_rate)
  if (length(rates) != 3L || any(!is.finite(rates)) || any(rates < 0)) {
    stop("Rates must be finite, non-negative numbers.")
  }
  if (identical(delivery, "quarterly_report")) delivery <- "financial_report"
  delivery <- match.arg(delivery, c("financial_report", "smartsheet_draft", "both"))
  # A new name/root is required for each run, preserving prior input snapshots.
  if (dir.exists(quarter_path(quarter_id, root))) {
    stop("This report already exists. Use a new report name or a different root folder.")
  }
  paths <- prepare_quarter(
    quarter_id = quarter_id, period_start = start, period_end = end,
    included_projects = included_projects, general_ledger = general_ledger,
    receipt_folder = receipt_folder, mileage_log = mileage_log,
    mileage_sheet_map = mileage_sheet_map, mileage_rate = mileage_rate,
    fringe_rate = fringe_rate, indirect_rate = indirect_rate,
    root = root, project_output_root = project_output_root
  )
  if (isTRUE(prepare_only)) {
    # Match source inputs for review without creating report deliverables.
    run_reporting_engine(paths$settings)
    return(list(paths = paths, project_deliverables = NULL))
  }
  run_quarterly_pipeline(paths, delivery = delivery)
}

# Retain original filenames: extension and receipt metadata matter to the readers.
stage_upload <- function(upload, destination) {
  if (is.null(upload) || nrow(upload) == 0L) return("")
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  name <- basename(gsub("\\", "/", upload$name[1], fixed = TRUE))
  if (!nzchar(name) || name %in% c(".", "..")) stop("Invalid upload filename.")
  target <- file.path(destination, name)
  if (!file.copy(upload$datapath[1], target)) stop("Could not stage upload: ", name)
  target
}

stage_receipts <- function(upload, destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  if (is.null(upload) || nrow(upload) == 0L) return(destination)
  upload <- upload[tolower(tools::file_ext(upload$name)) %in%
    c("pdf", "png", "jpg", "jpeg", "tif", "tiff", "heic"), , drop = FALSE]
  for (index in seq_len(nrow(upload))) {
    # Separate folders preserve duplicate basenames. These folders are not project hints.
    folder <- file.path(destination, sprintf("upload_%05d", index))
    stage_upload(upload[index, , drop = FALSE], folder)
  }
  destination
}

export_report_bundle <- function(result, file) {
  staging <- tempfile("report_bundle_")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  copy_tree <- function(source, destination) {
    dir.create(destination, recursive = TRUE, showWarnings = FALSE)
    entries <- list.files(source, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(entries) && !all(file.copy(entries, destination, recursive = TRUE))) {
      stop("Could not create the report bundle.")
    }
  }
  copy_tree(result$project_deliverables$root, file.path(staging, "project_reports"))
  zip::zipr(file, files = "project_reports", root = staging)
  invisible(file)
}
