# Shared functions for the quarterly financial reporting workflow.
# The Shiny app is the user-facing entry point.

reporting_project_root <- local({
  source_file <- sys.frame(1)$ofile
  if (!is.null(source_file)) dirname(dirname(normalizePath(source_file, mustWork = TRUE)))
  else normalizePath(getOption("financial_reporting.root", getwd()), mustWork = TRUE)
})

quarter_path <- function(quarter_id, root = "quarters") {
  report_name <- trimws(as.character(quarter_id[1]))
  if (!nzchar(report_name)) stop("Report Name is required.")
  safe_id <- gsub("[<>:\"/\\\\|?*]+", "_", report_name)
  safe_id <- gsub("\\s+", "_", safe_id)
  safe_id <- gsub("^[_\\.]+|[_\\.]+$", "", safe_id)
  if (!nzchar(safe_id)) stop("Report Name must include letters or numbers.")
  file.path(normalizePath(root, mustWork = FALSE), safe_id)
}

blank_value <- function(value) {
  is.null(value) || length(value) == 0 || is.na(value[1]) || !nzchar(trimws(as.character(value[1])))
}

source_path <- function(value) {
  if (blank_value(value)) return(NA_character_)
  value <- trimws(as.character(value[1]))
  if (grepl("^[A-Za-z]:[/\\\\]", value) || startsWith(value, "/")) return(value)
  normalizePath(value, mustWork = FALSE)
}

normalize_gl_headers <- function(headers) {
  normalized <- gsub("[^a-z0-9]+", "_", tolower(trimws(as.character(headers))))
  normalized <- gsub("^_+|_+$", "", normalized)
  aliases <- list(
    posted_dt = c("posted_dt", "posted_date", "posting_date", "transaction_date", "date"),
    amount = c("amount", "net_amount", "transaction_amount"),
    gl = c("gl", "gl_account", "g_l_account", "account", "account_number", "account_no"),
    grant_and_program_name = c("grant_and_program_name", "gl_and_program_name", "grant_program_name", "grant_program", "grant_and_program", "project", "project_name", "program_name"),
    doc_dt = c("doc_dt", "doc_date", "document_date"),
    debit = c("debit", "debits", "debit_amount"),
    credit = c("credit", "credits", "credit_amount"),
    vendor_name = c("vendor_name", "vendor", "payee", "employee_name"),
    memo_description = c("memo_description", "memo", "description"),
    doc = c("doc", "document", "document_number", "doc_no"),
    txn_no = c("txn_no", "transaction_number", "transaction_no", "transaction_id"),
    jnl = c("jnl", "journal", "journal_code")
  )
  for (key in names(aliases)) normalized[normalized %in% aliases[[key]]] <- key
  normalized
}

canonical_gl_columns <- function(gl) {
  canonical <- c(posted_dt = "Posted dt.", amount = "Amount", gl = "GL",
    grant_and_program_name = "Grant and Program name", doc_dt = "Doc dt.",
    debit = "Debit", credit = "Credit", vendor_name = "Vendor name",
    memo_description = "Memo/Description", doc = "Doc", txn_no = "Txn No", jnl = "JNL")
  found <- normalize_gl_headers(names(gl))
  for (key in names(canonical)) {
    index <- which(found == key)
    if (length(index) > 1) stop("The GL has multiple columns for ", canonical[[key]], ". Please use distinct column headings.")
    if (length(index) == 1) names(gl)[index] <- canonical[[key]]
    else gl[[canonical[[key]]]] <- rep(NA_character_, nrow(gl))
  }
  gl
}

read_gl_export <- function(path, required_columns = c("posted_dt", "amount", "gl", "grant_and_program_name")) {
  if (!file.exists(path)) stop("General ledger file is missing: ", path)
  extension <- tolower(tools::file_ext(path))
  if (extension == "csv") {
    gl <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    found <- normalize_gl_headers(names(gl))
    if (!all(required_columns %in% found)) {
      stop("The GL needs columns for: ", paste(required_columns, collapse = ", "), ".")
    }
    return(canonical_gl_columns(gl))
  }
  if (!extension %in% c("xlsx", "xls")) {
    stop("General ledger must be a CSV, XLSX, or XLS file.")
  }

  closest <- character()
  best_score <- -1L
  for (sheet in readxl::excel_sheets(path)) {
    preview <- tryCatch(
      readxl::read_excel(path, sheet = sheet, col_names = FALSE, n_max = 500, col_types = "text", .name_repair = "minimal"),
      error = function(error) NULL
    )
    if (is.null(preview) || nrow(preview) == 0) next
    for (header_row in seq_len(nrow(preview))) {
      headers <- unlist(preview[header_row, ], use.names = FALSE)
      found <- normalize_gl_headers(headers)
      score <- sum(required_columns %in% found)
      if (score > best_score) {
        best_score <- score
        closest <- paste0("Sheet '", sheet, "', row ", header_row, ": ", paste(headers[!is.na(headers) & nzchar(headers)], collapse = ", "))
      }
      if (all(required_columns %in% found)) {
        return(canonical_gl_columns(as.data.frame(readxl::read_excel(
          path, sheet = sheet, skip = header_row - 1L, col_types = "text", .name_repair = "minimal"
        ), stringsAsFactors = FALSE)))
      }
    }
  }
  stop(
    "No transaction table with the required GL columns was found. Expected: ",
    paste(required_columns, collapse = ", "), ". Closest header row: ", paste(closest, collapse = " ")
  )
}

parse_gl_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- trimws(as.character(x))
  raw_number <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA_real_, length(x)), origin = "1899-12-30")
  is_number <- is.finite(raw_number)
  result[is_number] <- as.Date(raw_number[is_number], origin = "1899-12-30")
  text_date <- !is_number & !is.na(x) & nzchar(x)
  iso_date <- text_date & grepl("^[0-9]{4}[-/]", x)
  result[iso_date] <- suppressWarnings(lubridate::ymd(substr(x[iso_date], 1L, 10L), quiet = TRUE))
  result[text_date & !iso_date] <- suppressWarnings(lubridate::mdy(x[text_date & !iso_date], quiet = TRUE))
  result
}

gl_reporting_dates <- function(gl) {
  dates <- parse_gl_date(gl[["Posted dt."]])
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) return(NULL)
  list(start = min(dates), end = max(dates))
}

mileage_workbook_sheets <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (!extension %in% c("xlsx", "xls")) return(character())
  readxl::excel_sheets(path)
}

extract_mileage_sheets <- function(workbook_path, project_to_sheet, destination_folder) {
  selected <- project_to_sheet[!is.na(project_to_sheet) & nzchar(project_to_sheet)]
  if (length(selected) == 0) {
    return(list(log = workbook_path, mapping = NA_character_))
  }
  available_sheets <- mileage_workbook_sheets(workbook_path)
  unavailable <- setdiff(unname(selected), available_sheets)
  if (length(unavailable) > 0) {
    stop("Selected mileage worksheet(s) are not in the uploaded workbook: ", paste(unavailable, collapse = ", "))
  }

  dir.create(destination_folder, recursive = TRUE, showWarnings = FALSE)
  mapping <- data.frame(
    "GL Project" = names(selected),
    "Mileage Worksheet" = unname(selected),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  mapping_path <- file.path(destination_folder, "Mileage_Worksheet_Mapping.csv")
  utils::write.csv(mapping, mapping_path, row.names = FALSE, na = "")

  logs <- lapply(seq_along(selected), function(index) {
    # Worksheets can mix numeric entries with notes or blanks in the same
    # column. Preserve source values as text; parse miles during calculation.
    sheet_data <- as.data.frame(readxl::read_excel(
      workbook_path, sheet = unname(selected[index]), col_types = "text", .name_repair = "minimal"
    ), stringsAsFactors = FALSE)
    if (nrow(sheet_data) == 0) return(NULL)
    sheet_data[["Project"]] <- names(selected)[index]
    sheet_data[["Mileage worksheet"]] <- unname(selected[index])
    sheet_data[c("Project", "Mileage worksheet", setdiff(names(sheet_data), c("Project", "Mileage worksheet")))]
  })
  logs <- Filter(Negate(is.null), logs)
  if (length(logs) == 0) {
    stop("The selected mileage worksheet(s) contain no rows.")
  }
  log_path <- file.path(destination_folder, "Selected_Mileage_Worksheet_Log.csv")
  utils::write.csv(dplyr::bind_rows(logs), log_path, row.names = FALSE, na = "")
  list(log = log_path, mapping = mapping_path)
}

copy_input_file <- function(value, destination_folder) {
  value <- source_path(value)
  dir.create(destination_folder, recursive = TRUE, showWarnings = FALSE)
  if (is.na(value) || !file.exists(value)) {
    existing <- list.files(destination_folder, full.names = TRUE, recursive = TRUE)
    return(if (length(existing) > 0) existing[1] else NA_character_)
  }
  destination <- file.path(destination_folder, basename(value))
  if (normalizePath(value, mustWork = FALSE) != normalizePath(destination, mustWork = FALSE)) {
    if (!file.copy(value, destination, overwrite = TRUE)) stop("Could not copy input file: ", value)
  }
  normalizePath(destination, mustWork = FALSE)
}

copy_receipt_folder <- function(value, destination_folder) {
  value <- source_path(value)
  dir.create(destination_folder, recursive = TRUE, showWarnings = FALSE)
  if (is.na(value) || !dir.exists(value)) return(normalizePath(destination_folder, mustWork = FALSE))
  if (normalizePath(value, mustWork = FALSE) == normalizePath(destination_folder, mustWork = FALSE)) {
    return(normalizePath(destination_folder, mustWork = FALSE))
  }
  files <- list.files(value, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  if (length(files) == 0) return(normalizePath(destination_folder, mustWork = FALSE))
  relative_files <- substring(files, nchar(normalizePath(value, mustWork = FALSE)) + 2)
  for (index in seq_along(files)) {
    destination <- file.path(destination_folder, relative_files[index])
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(files[index], destination, overwrite = TRUE)) stop("Could not copy receipt file: ", files[index])
  }
  normalizePath(destination_folder, mustWork = FALSE)
}

write_settings_file <- function(path, values) {
  settings <- data.frame(
    Setting = names(values),
    Value = unname(as.character(values)),
    Notes = c(
      "The user-provided report name.",
      "The first date included in this report.",
      "The last date included in this report.",
      "All projects in GL, or project names from Grant and Program name separated by semicolons.",
      "A copied snapshot of the GL supplied for this report.",
      "Receipt folders copied for this report.",
      "The original mileage workbook supplied for this report.",
      "Mileage rows extracted from the worksheet(s) selected in the app.",
      "The confirmed GL-project-to-mileage-worksheet mapping.",
      "Optional parent folder for user-facing project deliverables.",
      "Internal working output. Do not edit this path.",
      "Receipt packet total must tie to GL expenses within this tolerance.",
      "Approved reimbursement rate for this report.",
      "Approved payroll fringe rate for invoice estimates.",
      "Approved indirect rate for invoice estimates."
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  utils::write.csv(settings, path, row.names = FALSE, na = "")
}

prepare_quarter <- function(
  quarter_id,
  period_start,
  period_end,
  included_projects = "All projects in GL",
  general_ledger = "",
  receipt_folder = "",
  mileage_log = "",
  mileage_sheet_map = character(),
  mileage_rate = 0.765,
  fringe_rate = 0.18,
  indirect_rate = 0.10,
  root = "quarters",
  project_output_root = ""
) {
  quarter_dir <- quarter_path(quarter_id, root)
  input_dir <- file.path(quarter_dir, "inputs")
  output_dir <- file.path(quarter_dir, "outputs")
  mileage_workbook <- copy_input_file(mileage_log, file.path(input_dir, "mileage"))
  mileage_log_path <- mileage_workbook
  mileage_mapping_path <- NA_character_
  if (!is.na(mileage_workbook) && file.exists(mileage_workbook) &&
      tolower(tools::file_ext(mileage_workbook)) %in% c("xlsx", "xls") &&
      length(mileage_sheet_map) > 0) {
    mileage_extraction <- extract_mileage_sheets(
      mileage_workbook, mileage_sheet_map, file.path(input_dir, "mileage")
    )
    mileage_log_path <- mileage_extraction$log
    mileage_mapping_path <- mileage_extraction$mapping
  }
  paths <- list(
    quarter = quarter_dir,
    general_ledger = copy_input_file(general_ledger, file.path(input_dir, "general_ledger")),
    receipts = copy_receipt_folder(receipt_folder, file.path(input_dir, "receipts")),
    mileage_workbook = mileage_workbook,
    mileage = mileage_log_path,
    mileage_mapping = mileage_mapping_path,
    internal = file.path(output_dir, "internal"),
    quarterly = file.path(output_dir, "quarterly_report"),
    smartsheet = file.path(output_dir, "smartsheet"),
    project_output_root = source_path(project_output_root)
  )
  dir.create(paths$internal, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$quarterly, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$smartsheet, recursive = TRUE, showWarnings = FALSE)
  settings_values <- c(
    "Report name" = trimws(as.character(quarter_id[1])),
    "Reporting period start" = as.character(as.Date(period_start)),
    "Reporting period end" = as.character(as.Date(period_end)),
    "Included projects" = included_projects,
    "General ledger file" = paths$general_ledger,
    "Receipt folder" = paths$receipts,
    "Mileage workbook file" = paths$mileage_workbook,
    "Mileage log file" = paths$mileage,
    "Mileage worksheet mapping file" = paths$mileage_mapping,
    "Project deliverables folder" = paths$project_output_root,
    "Output folder" = normalizePath(paths$internal, mustWork = FALSE),
    "Expense receipt QC tolerance" = "0.01",
    "Mileage reimbursement rate" = as.character(mileage_rate),
    "Payroll fringe rate" = as.character(fringe_rate),
    "Indirect rate" = as.character(indirect_rate)
  )
  paths$settings <- file.path(quarter_dir, "settings.csv")
  write_settings_file(paths$settings, settings_values)
  paths
}

read_setting <- function(settings, name) {
  value <- settings$Value[settings$Setting == name]
  if (length(value) == 0) return(NA_character_)
  value[1]
}

refresh_rate_outputs <- function(output_dir, settings_path) {
  settings <- utils::read.csv(settings_path, check.names = FALSE, stringsAsFactors = FALSE)
  mileage_rate <- suppressWarnings(as.numeric(read_setting(settings, "Mileage reimbursement rate")))
  fringe_rate <- suppressWarnings(as.numeric(read_setting(settings, "Payroll fringe rate")))
  indirect_rate <- suppressWarnings(as.numeric(read_setting(settings, "Indirect rate")))
  category_path <- file.path(output_dir, "Quarterly_Report_Category_Draft.csv")
  mileage_path <- file.path(output_dir, "Mileage_Summary.csv")
  if (!file.exists(category_path)) return(invisible(NULL))

  categories <- utils::read.csv(category_path, check.names = FALSE, stringsAsFactors = FALSE)
  if (file.exists(mileage_path) && is.finite(mileage_rate)) {
    mileage <- utils::read.csv(mileage_path, check.names = FALSE, stringsAsFactors = FALSE)
    has_miles <- if ("miles" %in% names(mileage)) is.finite(mileage$miles) else logical(0)
    if (length(has_miles) > 0) {
      mileage[["Applied Mileage Rate"]] <- mileage_rate
      mileage$mileage_cost[has_miles] <- mileage$miles[has_miles] * mileage_rate
      mileage$status[has_miles] <- "Pass"
      utils::write.csv(mileage, mileage_path, row.names = FALSE, na = "")
      mileage_rows <- categories$category == "Mileage"
      matched <- match(categories$project_standard[mileage_rows], mileage$project_standard)
      categories$current_quarter_spend[mileage_rows] <- mileage$mileage_cost[matched]
      categories$readiness[mileage_rows] <- mileage$status[matched]
      categories$mileage_status[mileage_rows] <- mileage$status[matched]
      utils::write.csv(categories, category_path, row.names = FALSE, na = "")
    }
  }

  projects <- sort(unique(categories$project_standard))
  category_value <- function(category_name, column_name) {
    category_rows <- categories[categories$category == category_name, , drop = FALSE]
    values <- category_rows[[column_name]][match(projects, category_rows$project_standard)]
    as.numeric(values)
  }
  payroll <- category_value("Payroll", "current_quarter_spend")
  expenses <- category_value("Expenses", "current_quarter_spend")
  mileage <- category_value("Mileage", "current_quarter_spend")
  payroll_budget <- category_value("Payroll", "contracted_amount")
  expenses_budget <- category_value("Expenses", "contracted_amount")
  mileage_budget <- category_value("Mileage", "contracted_amount")
  direct_costs <- ifelse(is.finite(payroll) & is.finite(expenses) & is.finite(mileage), payroll + (payroll * fringe_rate) + expenses + mileage, NA_real_)
  invoice <- data.frame(
    Project = projects,
    Payroll = payroll,
    Expenses = expenses,
    Mileage = mileage,
    "Mileage Rate" = mileage_rate,
    "Payroll Fringe Rate" = fringe_rate,
    "Payroll Fringe" = ifelse(is.finite(payroll), payroll * fringe_rate, NA_real_),
    "Direct Costs" = direct_costs,
    "Indirect Rate" = indirect_rate,
    "Indirect Costs" = ifelse(is.finite(direct_costs), direct_costs * indirect_rate, NA_real_),
    "Invoice Estimate" = ifelse(is.finite(direct_costs), direct_costs * (1 + indirect_rate), NA_real_),
    "Invoice Status" = ifelse(is.finite(direct_costs), "Ready for review", "Inputs or QC pending"),
    check.names = FALSE
  )
  utils::write.csv(invoice, file.path(output_dir, "Invoice_Estimate_Draft.csv"), row.names = FALSE, na = "")

  spent <- ifelse(is.finite(payroll) & is.finite(expenses) & is.finite(mileage), payroll + expenses + mileage, NA_real_)
  total_budget <- ifelse(is.finite(payroll_budget) & is.finite(expenses_budget) & is.finite(mileage_budget), payroll_budget + expenses_budget + mileage_budget, NA_real_)
  period_start <- as.Date(read_setting(settings, "Reporting period start"))
  period_end <- as.Date(read_setting(settings, "Reporting period end"))
  period_label <- paste(format(period_start, "%Y-%m-%d"), "to", format(period_end, "%Y-%m-%d"))
  smartsheet <- data.frame(
    Project = projects,
    "Reporting Period" = period_label,
    Payroll = payroll,
    Expenses = expenses,
    Mileage = mileage,
    "Spent Amount" = spent,
    "Total Budget" = total_budget,
    "Percent Spent" = ifelse(is.finite(spent) & is.finite(total_budget) & total_budget != 0, spent / total_budget, NA_real_),
    "Report Status" = ifelse(is.finite(spent) & is.finite(total_budget), "Ready for review", "Inputs or QC pending"),
    check.names = FALSE
  )
  utils::write.csv(smartsheet, file.path(output_dir, "Smartsheet_Draft.csv"), row.names = FALSE, na = "")
}

safe_output_name <- function(value) {
  value <- trimws(as.character(value[1]))
  value <- gsub("[<>:\"/\\\\|?*]+", "_", value)
  value <- gsub("\\s+", "_", value)
  value <- gsub("^[_\\.]+|[_\\.]+$", "", value)
  if (!nzchar(value)) "Unnamed_Project" else value
}

write_project_xlsx <- function(sheets, path) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("The writexl package is required to create project Excel deliverables. Install it with install.packages('writexl').")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(sheets, path)
}

combine_receipt_pdfs <- function(receipt_files, output_path) {
  receipt_files <- receipt_files[file.exists(receipt_files)]
  if (length(receipt_files) == 0) return(NA_character_)
  qpdf <- Sys.which("qpdf")
  if (!nzchar(qpdf)) return(NA_character_)

  page_folder <- file.path(tempdir(), paste0("receipt_packet_", as.integer(Sys.time())))
  dir.create(page_folder, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(page_folder, recursive = TRUE, force = TRUE), add = TRUE)
  packet_pages <- vapply(seq_along(receipt_files), function(index) {
    source <- receipt_files[index]
    extension <- tolower(tools::file_ext(source))
    if (extension == "pdf") return(source)
    if (!requireNamespace("magick", quietly = TRUE)) return(NA_character_)
    page <- file.path(page_folder, paste0(sprintf("%04d", index), ".pdf"))
    converted <- tryCatch({
      image <- magick::image_read(source)
      magick::image_write(image, path = page, format = "pdf")
      page
    }, error = function(error) NA_character_)
    converted
  }, character(1))
  packet_pages <- packet_pages[!is.na(packet_pages) & file.exists(packet_pages)]
  if (length(packet_pages) == 0) return(NA_character_)

  args <- c("--empty", "--pages", shQuote(packet_pages), "--", shQuote(output_path))
  status <- suppressWarnings(system2(qpdf, args = args, stdout = FALSE, stderr = FALSE))
  if (!identical(status, 0L) || !file.exists(output_path)) return(NA_character_)
  output_path
}

format_smartsheet_summary <- function(path, project_width) {
  path <- normalizePath(path, mustWork = TRUE)
  staging <- tempfile("smartsheet_layout_")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = staging)
  sheet_path <- file.path(staging, "xl", "worksheets", "sheet1.xml")
  sheet <- paste(readLines(sheet_path, warn = FALSE), collapse = "")
  sheet <- sub("(<worksheet[^>]*>)", '\\1<sheetPr><pageSetUpPr fitToPage="1"/></sheetPr>', sheet)
  columns <- sprintf('<cols><col min="1" max="1" width="%s" customWidth="1"/><col min="2" max="7" width="18" customWidth="1"/></cols>', project_width)
  sheet <- sub("<sheetData>", paste0(columns, "<sheetData>"), sheet, fixed = TRUE)
  sheet <- sub('defaultRowHeight="15"', 'defaultRowHeight="20"', sheet, fixed = TRUE)
  sheet <- sub("</worksheet>", '<pageSetup paperSize="1" orientation="landscape" fitToWidth="1" fitToHeight="1"/></worksheet>', sheet, fixed = TRUE)
  writeLines(sheet, sheet_path, useBytes = TRUE)
  zip_binary <- Sys.which("zip")
  if (nzchar(zip_binary)) {
    previous_dir <- getwd()
    on.exit(setwd(previous_dir), add = TRUE, after = FALSE)
    setwd(staging)
    status <- utils::zip(path, files = "xl/worksheets/sheet1.xml", flags = "-q", zip = zip_binary)
    if (status != 0L) stop("Could not format the Smartsheet summary workbook.")
  } else if (.Platform$OS.type == "windows") {
    archive <- tempfile("smartsheet_layout_", fileext = ".zip")
    on.exit(unlink(archive), add = TRUE)
    quote_ps <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
    entries <- list.files(staging, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    command <- paste0("$ErrorActionPreference = 'Stop'; Compress-Archive -LiteralPath ",
      paste(vapply(entries, quote_ps, character(1)), collapse = ","),
      " -DestinationPath ", quote_ps(archive), " -Force")
    status <- system2("powershell.exe", c("-NoProfile", "-NonInteractive", "-Command", shQuote(command)), stdout = FALSE, stderr = FALSE)
    if (status != 0L || !file.exists(archive) || !file.copy(archive, path, overwrite = TRUE)) {
      stop("Could not format the Smartsheet summary workbook.")
    }
  } else {
    stop("The zip utility is required to format the Smartsheet summary workbook.")
  }
  invisible(path)
}

build_smartsheet_summary <- function(paths, destination_root) {
  categories <- utils::read.csv(file.path(paths$internal, "Quarterly_Report_Category_Draft.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  projects <- sort(unique(categories$project_standard))
  category_order <- c("Expenses", "Payroll", "Mileage")
  rows <- data.frame(
    project_standard = rep(projects, each = 3L),
    category = rep(category_order, times = length(projects)),
    stringsAsFactors = FALSE
  )
  source_key <- paste(categories$project_standard, categories$category, sep = "\r")
  if (anyDuplicated(source_key)) stop("Duplicate project/category totals found while creating the Smartsheet summary.")
  matched <- match(paste(rows$project_standard, rows$category, sep = "\r"), source_key)
  spend <- categories$current_quarter_spend[matched]
  budget <- categories$contracted_amount[matched]
  summary <- data.frame(
    Project = rows$project_standard,
    Payroll = ifelse(rows$category == "Payroll", spend, NA_real_),
    Expenses = ifelse(rows$category == "Expenses", spend, NA_real_),
    Mileage = ifelse(rows$category == "Mileage", spend, NA_real_),
    "Spent Amount" = spend,
    "Total Budget" = budget,
    "Percent Spent" = ifelse(is.finite(spend) & is.finite(budget) & budget != 0, spend / budget, NA_real_),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  summary_path <- file.path(destination_root, "SmartsheetInput.xlsx")
  write_project_xlsx(list(Sheet1 = summary), summary_path)
  format_smartsheet_summary(summary_path, max(32, min(70, max(nchar(projects), 0L) + 3L)))
  summary_path
}

build_project_deliverables <- function(paths) {
  settings <- utils::read.csv(paths$settings, check.names = FALSE, stringsAsFactors = FALSE)
  configured_root <- read_setting(settings, "Project deliverables folder")
  destination_root <- if (is.na(configured_root) || !nzchar(configured_root)) {
    file.path(paths$quarterly, "Project_Deliverables")
  } else {
    configured_root
  }
  report_root <- destination_root
  dir.create(report_root, recursive = TRUE, showWarnings = FALSE)

  invoice <- utils::read.csv(file.path(paths$internal, "Invoice_Estimate_Draft.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  receipts_path <- file.path(paths$internal, "Receipt_Reconciliation.csv")
  payroll_path <- file.path(paths$internal, "Payroll_Detail.csv")
  transactions_path <- file.path(paths$internal, "Standardized_GL_Transactions.csv")
  receipts <- if (file.exists(receipts_path)) utils::read.csv(receipts_path, check.names = FALSE, stringsAsFactors = FALSE) else data.frame()
  payroll <- if (file.exists(payroll_path)) utils::read.csv(payroll_path, check.names = FALSE, stringsAsFactors = FALSE) else data.frame()
  transactions <- if (file.exists(transactions_path)) utils::read.csv(transactions_path, check.names = FALSE, stringsAsFactors = FALSE) else data.frame()
  manifest <- data.frame(Project = character(), Folder = character(), Receipt_Packet = character(), stringsAsFactors = FALSE)

  for (project in invoice$Project) {
    project_folder <- file.path(report_root, safe_output_name(project))
    dir.create(project_folder, recursive = TRUE, showWarnings = FALSE)
    summary_row <- invoice[invoice$Project == project, , drop = FALSE]
    summary <- data.frame(
      Category = c("Payroll", "Payroll Fringe", "Mileage", "Expenses", "Direct Costs", "Indirect Costs", "Total Invoice Amount", "Invoice Status"),
      Amount = c(summary_row$Payroll, summary_row[["Payroll Fringe"]], summary_row$Mileage, summary_row$Expenses, summary_row[["Direct Costs"]], summary_row[["Indirect Costs"]], summary_row[["Invoice Estimate"]], NA_real_),
      Status = c(rep("", 7), summary_row[["Invoice Status"]]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    write_project_xlsx(list(Category_Spend_Summary = summary), file.path(project_folder, "Category_Spend_Summary.xlsx"))

    project_receipts <- if (nrow(receipts) > 0 && "project_standard" %in% names(receipts)) {
      receipts[receipts$project_standard == project & receipts$match_status == "Matched", , drop = FALSE]
    } else data.frame()
    if (nrow(project_receipts) == 0) {
      project_receipts <- data.frame(Message = "No confirmed receipt-to-GL matches for this project.", stringsAsFactors = FALSE)
      receipt_files <- character()
    } else {
      project_receipts <- project_receipts[order(project_receipts$packet_date, project_receipts$receipt_file, na.last = TRUE), , drop = FALSE]
      receipt_files <- project_receipts$receipt_file
    }
    project_expenses <- if (nrow(transactions) > 0 && all(c("project_standard", "category") %in% names(transactions))) {
      transactions[transactions$project_standard == project & transactions$category == "Expenses", , drop = FALSE]
    } else data.frame()
    if (nrow(project_expenses) > 0 && nrow(project_receipts) > 0 &&
        all(c("document_number", "receipt_file", "match_status") %in% names(project_receipts)) &&
        "document_number" %in% names(project_expenses)) {
      receipt_links <- unique(project_receipts[c("document_number", "receipt_file", "match_status")])
      names(receipt_links)[names(receipt_links) == "receipt_file"] <- "receipt_file_link"
      names(receipt_links)[names(receipt_links) == "match_status"] <- "receipt_match_status"
      project_expenses <- merge(project_expenses, receipt_links, by = "document_number", all.x = TRUE, sort = FALSE)
    }
    if (nrow(project_expenses) == 0) {
      project_expenses <- data.frame(Message = "No expense transactions were available in the General Ledger for this project.", stringsAsFactors = FALSE)
    }
    write_project_xlsx(list(Detailed_Expenses = project_expenses), file.path(project_folder, "Detailed_Expenses.xlsx"))
    packet <- combine_receipt_pdfs(receipt_files, file.path(project_folder, "Receipt_Packet.pdf"))

    project_payroll <- if (nrow(payroll) > 0 && "project_standard" %in% names(payroll)) {
      payroll[payroll$project_standard == project, , drop = FALSE]
    } else data.frame()
    if (nrow(project_payroll) == 0) {
      project_payroll <- data.frame(Message = "No dated payroll transactions were available in the General Ledger.", stringsAsFactors = FALSE)
    } else {
      project_payroll <- project_payroll[order(project_payroll$payroll_date, project_payroll$staff_name), , drop = FALSE]
    }
    write_project_xlsx(list(Hourly_Time_Entries = project_payroll), file.path(project_folder, "Hourly_Time_Entries.xlsx"))
    manifest <- rbind(manifest, data.frame(Project = project, Folder = project_folder, Receipt_Packet = ifelse(is.na(packet), "No PDF packet created", packet), stringsAsFactors = FALSE))
  }
  utils::write.csv(manifest, file.path(report_root, "Project_Deliverables_Manifest.csv"), row.names = FALSE, na = "")
  smartsheet_summary <- build_smartsheet_summary(paths, report_root)
  list(root = report_root, manifest = manifest, smartsheet_summary = smartsheet_summary)
}

copy_deliverables <- function(paths, delivery) {
  quarterly_files <- c(
    "Input_Readiness.csv", "Payroll_GL_Summary.csv", "Payroll_Detail.csv", "Expense_Receipt_QC.csv",
    "Receipt_Reconciliation.csv", "Mileage_Summary.csv", "Project_Scope.csv", "Quarterly_Report_Category_Draft.csv",
    "Invoice_Estimate_Draft.csv", "Reconciliation_Checks.csv", "RUN_NOTES.txt"
  )
  smartsheet_files <- c("Smartsheet_Draft.csv", "Reconciliation_Checks.csv")
  copy_group <- function(files, destination) {
    for (file_name in files) {
      source <- file.path(paths$internal, file_name)
      if (file.exists(source)) file.copy(source, file.path(destination, file_name), overwrite = TRUE)
    }
  }
  if (delivery %in% c("quarterly_report", "both")) copy_group(quarterly_files, paths$quarterly)
  if (delivery %in% c("smartsheet_draft", "both")) copy_group(smartsheet_files, paths$smartsheet)
}

run_quarterly_pipeline <- function(paths, delivery = c("both", "quarterly_report", "smartsheet_draft")) {
  delivery <- match.arg(delivery)
  if (is.na(paths$general_ledger) || !file.exists(paths$general_ledger)) {
    stop("General ledger is missing. Select it in the reporting workflow.")
  }
  old_options <- options(quarterly.settings_path = paths$settings)
  on.exit(options(old_options), add = TRUE)
  engine_path <- file.path(reporting_project_root, "RunQuarterlyReport.R")
  if (!file.exists(engine_path)) stop("Reporting script is missing: ", engine_path)
  sys.source(engine_path, envir = new.env(parent = globalenv()))
  refresh_rate_outputs(paths$internal, paths$settings)
  copy_deliverables(paths, delivery)
  project_deliverables <- if (delivery %in% c("quarterly_report", "both")) build_project_deliverables(paths) else NULL

  checks <- utils::read.csv(file.path(paths$internal, "Reconciliation_Checks.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  delivery_status <- data.frame(
    Deliverable = c("Quarterly report package", "Smartsheet dashboard draft"),
    Requested = c(delivery %in% c("quarterly_report", "both"), delivery %in% c("smartsheet_draft", "both")),
    Location = c(paths$quarterly, paths$smartsheet),
    Status = c(
      if (delivery %in% c("quarterly_report", "both")) "Created for review" else "Not requested",
      if (delivery %in% c("smartsheet_draft", "both")) "Created for review; do not publish until QC is resolved" else "Not requested"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  utils::write.csv(delivery_status, file.path(paths$quarter, "Delivery_Status.csv"), row.names = FALSE)
  log_line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "delivery=", delivery, "GL=", basename(paths$general_ledger))
  write(log_line, file.path(paths$quarter, "run_log.txt"), append = TRUE)
  list(paths = paths, checks = checks, delivery_status = delivery_status, project_deliverables = project_deliverables)
}
