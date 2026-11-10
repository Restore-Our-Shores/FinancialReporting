# Shared receipt review logic. UI draft edits are applied to a new, isolated
# report version so failed rebuilds never replace the last downloadable result.
receipt_relative_paths <- function(files, root) {
  if (!length(files)) return(character())
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  if (any(!startsWith(files, paste0(root, "/")))) stop("Receipt is outside this report's input snapshot.")
  substring(files, nchar(root) + 2L)
}

validate_receipt_assignments <- function(assignments, receipt_paths, expense_ids) {
  required <- c("receipt_relative_path", "source_row", "match_method")
  if (!all(required %in% names(assignments))) stop("Receipt assignments are incomplete.")
  if (anyDuplicated(assignments$receipt_relative_path) ||
      !setequal(assignments$receipt_relative_path, receipt_paths)) stop("Receipt assignments must cover exactly this report's receipts.")
  assigned <- assignments$source_row[!is.na(assignments$source_row)]
  if (any(!assigned %in% expense_ids)) stop("A receipt is assigned to an unknown expense line.")
  if (anyDuplicated(assigned)) stop("Each expense line can have one receipt. Unmatch the existing receipt first.")
  if (anyNA(assignments$match_method) || any(!assignments$match_method %in% c("Automatic", "Manual"))) stop("Invalid receipt match method.")
  invisible(TRUE)
}

read_receipt_review <- function(result) {
  read_csv <- function(name) utils::read.csv(file.path(result$paths$internal, name), check.names = FALSE, stringsAsFactors = FALSE)
  expenses <- read_csv("Standardized_GL_Transactions.csv")
  expenses <- expenses[expenses$category == "Expenses", , drop = FALSE]
  receipts <- read_csv("Receipt_Reconciliation.csv")
  if (!all(c("source_row", "match_method") %in% names(receipts))) stop("Generate a new report to enable receipt review.")
  receipts$receipt_file <- as.character(receipts$receipt_file)
  receipts$match_method <- as.character(receipts$match_method)
  receipts$receipt_amount <- as.numeric(receipts$receipt_amount)
  receipts$source_row <- as.integer(receipts$source_row)
  receipts$receipt_id <- seq_len(nrow(receipts))
  assignments <- data.frame(
    receipt_relative_path = receipt_relative_paths(receipts$receipt_file, result$paths$receipts),
    source_row = receipts$source_row, match_method = receipts$match_method, stringsAsFactors = FALSE)
  lines <- data.frame(source_row = expenses$source_row, reviewed = rep(FALSE, nrow(expenses)), note = rep("", nrow(expenses)), reviewed_at = rep("", nrow(expenses)), stringsAsFactors = FALSE)
  saved <- file.path(result$paths$quarter, "review", "Expense_Review.csv")
  if (file.exists(saved)) {
    previous <- utils::read.csv(saved, stringsAsFactors = FALSE, na.strings = character())
    index <- match(lines$source_row, previous$source_row)
    found <- !is.na(index)
    lines[found, c("reviewed", "note", "reviewed_at")] <- previous[index[found], c("reviewed", "note", "reviewed_at")]
  }
  list(expenses = expenses, receipts = receipts, assignments = assignments, lines = lines)
}

review_expense_table <- function(state) {
  expenses <- state$expenses
  receipt <- match(expenses$source_row, state$assignments$source_row)
  difference <- state$receipts$receipt_amount[receipt] - expenses$net_amount
  data.frame(
    Line = expenses$source_row, Project = expenses$project_standard,
    Date = expenses$transaction_date, Vendor = expenses$vendor,
    Description = expenses$description, Amount = expenses$net_amount,
    Receipt = ifelse(is.na(receipt), "No receipt", basename(state$receipts$receipt_file[receipt])),
    Check = ifelse(is.na(receipt), "No receipt", ifelse(!is.finite(difference), "Receipt amount missing", ifelse(abs(difference) <= 0.01, "Matched", "Amount differs"))),
    Review = ifelse(state$lines$reviewed[match(expenses$source_row, state$lines$source_row)], "Reviewed", "Needs review"),
    stringsAsFactors = FALSE, check.names = FALSE)
}

edit_receipt_review <- function(state, source_row, receipt_id = NULL, reviewed = NULL, note = NULL, reassign = FALSE) {
  line <- match(source_row, state$lines$source_row)
  if (length(line) != 1L || is.na(line)) stop("Select an expense line first.")
  if (!is.null(receipt_id)) {
    if (length(receipt_id) != 1L || is.na(receipt_id) || !receipt_id %in% c(0L, state$receipts$receipt_id)) stop("Select a receipt from this report.")
    owner <- if (receipt_id > 0L) state$assignments$source_row[receipt_id] else NA_integer_
    if (!is.na(owner) && owner != source_row && !isTRUE(reassign)) stop("This receipt belongs to another line. Enable Move receipt to reassign it.")
    affected <- which(state$lines$source_row %in% c(source_row, owner))
    state$lines$reviewed[affected] <- FALSE
    state$lines$reviewed_at[affected] <- ""
    current <- which(!is.na(state$assignments$source_row) & state$assignments$source_row == source_row)
    state$assignments$source_row[current] <- NA_integer_
    state$assignments$match_method[current] <- "Manual"
    if (receipt_id > 0L) {
      state$assignments$source_row[receipt_id] <- source_row
      state$assignments$match_method[receipt_id] <- "Manual"
    }
  }
  if (!is.null(reviewed)) {
    state$lines$reviewed[line] <- isTRUE(reviewed)
    state$lines$reviewed_at[line] <- if (isTRUE(reviewed)) format(Sys.time(), tz = "UTC", usetz = TRUE) else ""
  }
  if (!is.null(note)) state$lines$note[line] <- as.character(note)[1]
  state
}

record_receipt_review_decision <- function(result, decision) {
  state <- read_receipt_review(result)
  folder <- file.path(result$paths$quarter, "review")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  summary <- data.frame(Decision = decision, Expense_lines = nrow(state$lines),
    Lines_reviewed = sum(state$lines$reviewed), Lines_without_receipts = sum(review_expense_table(state)$Check == "No receipt"),
    Recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE), stringsAsFactors = FALSE)
  utils::write.csv(summary, file.path(folder, "Receipt_Review_Status.csv"), row.names = FALSE)
  invisible(result)
}

save_receipt_review <- function(result, state, reviewer = "") {
  original <- read_receipt_review(result)
  validate_receipt_assignments(state$assignments, original$assignments$receipt_relative_path, original$expenses$source_row)
  if (anyDuplicated(state$lines$source_row) || !setequal(state$lines$source_row, original$lines$source_row) ||
      anyNA(state$lines$reviewed) || !is.logical(state$lines$reviewed) || anyNA(state$lines$note)) stop("Expense review entries are invalid.")
  settings <- utils::read.csv(result$paths$settings, check.names = FALSE, stringsAsFactors = FALSE)
  value <- function(name) read_setting(settings, name)
  mapping <- character()
  if (!blank_value(result$paths$mileage_mapping) && file.exists(result$paths$mileage_mapping)) {
    map <- utils::read.csv(result$paths$mileage_mapping, check.names = FALSE, stringsAsFactors = FALSE)
    mapping <- stats::setNames(map[["Mileage Worksheet"]], map[["GL Project"]])
  }
  version_parent <- if (!is.null(result$review_version_root)) result$review_version_root else dirname(result$paths$quarter)
  version_root <- tempfile("review_version_", tmpdir = version_parent)
  success <- FALSE
  on.exit(if (!success && dir.exists(version_root)) unlink(version_root, recursive = TRUE), add = TRUE)
  paths <- prepare_quarter(quarter_id = value("Report name"), period_start = value("Reporting period start"),
    period_end = value("Reporting period end"), included_projects = value("Included projects"),
    general_ledger = result$paths$general_ledger, receipt_folder = result$paths$receipts,
    mileage_log = if (blank_value(result$paths$mileage_workbook)) "" else result$paths$mileage_workbook,
    mileage_sheet_map = mapping, mileage_rate = as.numeric(value("Mileage reimbursement rate")),
    fringe_rate = as.numeric(value("Payroll fringe rate")), indirect_rate = as.numeric(value("Indirect rate")),
    root = version_root, project_output_root = file.path(version_root, "project_reports"))
  review_dir <- file.path(paths$quarter, "review"); dir.create(review_dir)
  utils::write.csv(state$assignments, file.path(review_dir, "Receipt_Match_Overrides.csv"), row.names = FALSE, na = "")
  utils::write.csv(state$lines, file.path(review_dir, "Expense_Review.csv"), row.names = FALSE, na = "")
  baseline <- file.path(result$paths$quarter, "review", "Original_Receipt_Reconciliation.csv")
  if (!file.exists(baseline)) baseline <- file.path(result$paths$internal, "Receipt_Reconciliation.csv")
  if (!file.copy(baseline, file.path(review_dir, "Original_Receipt_Reconciliation.csv"))) stop("Could not retain the original receipt matches.")
  receipt_for <- function(assignments, id) {
    match <- which(!is.na(assignments$source_row) & assignments$source_row == id)
    if (length(match)) assignments$receipt_relative_path[match[1]] else ""
  }
  log <- data.frame(source_row = integer(), previous_receipt = character(), receipt = character(), reviewed = logical(),
    note = character(), reviewer = character(), changed_at = character(), stringsAsFactors = FALSE)
  previous_log <- file.path(result$paths$quarter, "review", "Receipt_Review_Log.csv")
  if (file.exists(previous_log)) log <- utils::read.csv(previous_log, stringsAsFactors = FALSE, na.strings = character())
  for (id in state$lines$source_row) {
    row <- state$lines[state$lines$source_row == id, , drop = FALSE]
    old <- original$lines[original$lines$source_row == id, , drop = FALSE]
    before <- receipt_for(original$assignments, id); after <- receipt_for(state$assignments, id)
    if (!identical(before, after) || !identical(row$reviewed, old$reviewed) || !identical(row$note, old$note)) {
      log <- rbind(log, data.frame(source_row = id, previous_receipt = before, receipt = after, reviewed = row$reviewed,
        note = row$note, reviewer = reviewer, changed_at = format(Sys.time(), tz = "UTC", usetz = TRUE), stringsAsFactors = FALSE))
    }
  }
  utils::write.csv(log, file.path(review_dir, "Receipt_Review_Log.csv"), row.names = FALSE, na = "")
  rebuilt <- run_quarterly_pipeline(paths, delivery = "financial_report")
  rebuilt$review_version_root <- version_parent
  record_receipt_review_decision(rebuilt, if (all(state$lines$reviewed)) "Review completed" else "Review partially completed")
  success <- TRUE
  rebuilt
}
