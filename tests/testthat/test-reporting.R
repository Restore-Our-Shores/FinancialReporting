fixture <- function() {
  root <- normalizePath(testthat::test_path("..", "..", "simulated_data"), mustWork = TRUE)
  projects <- c("Meadow Restoration", "River Monitoring", "Woodland Outreach")
  list(quarter_id = "test_report", period_start = "2026-07-01", period_end = "2026-09-30",
    general_ledger = file.path(root, "Simulated_General_Ledger.xlsx"),
    receipt_folder = file.path(root, "receipts"), mileage_log = file.path(root, "Simulated_Mileage_Log.xlsx"),
    mileage_sheet_map = stats::setNames(projects, projects), root = tempfile("reports_"))
}

test_that("upload staging preserves metadata and duplicate basenames", {
  folder <- tempfile(); dir.create(folder)
  source <- tempfile(); writeLines("receipt", source)
  upload <- data.frame(name = c("2026-07-12 Vendor $10.00.pdf", "2026-07-12 Vendor $10.00.pdf"), datapath = source)
  stage_receipts(upload, folder)
  files <- list.files(folder, recursive = TRUE, full.names = TRUE)
  expect_length(files, 2L)
  expect_identical(basename(files), upload$name)
})

test_that("reports reconcile fictional controls and archive all deliverables", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  config <- fixture()
  result <- do.call(build_reports, config)
  expect_equal(nrow(result$project_deliverables$manifest), 3L)
  expect_true(all(file.exists(result$project_deliverables$manifest$Receipt_Packet)))
  qc <- read.csv(file.path(result$paths$internal, "Expense_Receipt_QC.csv"))
  expect_true(all(qc$status == "Pass"))
  expect_equal(sum(qc$receipt_count), 9)
  payroll <- read.csv(file.path(result$paths$internal, "Payroll_GL_Summary.csv"))
  expect_equal(sum(payroll$payroll_from_gl), 8190)
  mileage <- read.csv(file.path(result$paths$internal, "Mileage_Summary.csv"))
  expect_equal(sum(mileage$miles), 495)
  expect_equal(sum(mileage$mileage_cost), 378.675)
  summary <- readxl::read_xlsx(result$project_deliverables$smartsheet_summary)
  expect_equal(nrow(summary), 9L)
  expect_identical(names(summary), c("Project", "Payroll", "Expenses", "Mileage", "Spent Amount", "Total Budget", "Percent Spent"))
  expect_error(do.call(build_reports, config), "already exists")
  bundle <- tempfile(fileext = ".zip")
  export_report_bundle(result, bundle)
  entries <- utils::unzip(bundle, list = TRUE)$Name
  expect_true("project_reports/SmartsheetInput.xlsx" %in% entries)
  expect_true("audit/settings.csv" %in% entries)
  expect_equal(sum(grepl("^project_reports/.*/Receipt_Packet.pdf$", entries)), 3L)
  extracted <- tempfile(); utils::unzip(bundle, exdir = extracted)
  expect_equal(readxl::read_xlsx(file.path(extracted, "project_reports", "SmartsheetInput.xlsx")), summary)
})

test_that("Shiny runs the shared workflow with real browser upload paths", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  config <- fixture()
  upload <- function(files) data.frame(name = basename(files), datapath = files, size = file.info(files)$size, type = "")
  holder <- new.env()
  shiny::testServer(app_server, {
    session$setInputs(quarter_gl = upload(config$general_ledger), quarter_mileage = upload(config$mileage_log))
    projects <- names(config$mileage_sheet_map)
    session$setInputs(quarter_id = "browser_report", quarter_projects = projects,
      quarter_start = "2026-07-01", quarter_end = "2026-09-30",
      quarter_mileage_rate = 0.765, quarter_fringe_rate = 0.18, quarter_indirect_rate = 0.10,
      quarter_receipts = upload(list.files(config$receipt_folder, full.names = TRUE)))
    session$setInputs(quarter_mileage_sheet_1 = projects[1], quarter_mileage_sheet_2 = projects[2], quarter_mileage_sheet_3 = projects[3])
    session$setInputs(run_quarterly = 1)
    result <- quarter_result()
    expect_false(is.null(result))
    qc <- read.csv(file.path(result$paths$internal, "Expense_Receipt_QC.csv"))
    expect_true(all(qc$status == "Pass"))
    expect_equal(sum(qc$receipt_count), 9)
    expect_true(file.exists(result$project_deliverables$smartsheet_summary))
    holder$root <- session_root
  })
  expect_false(dir.exists(holder$root))
})

test_that("invalid dates, rates and missing inputs fail before running", {
  expect_error(build_reports("test", "2026-09-30", "2026-07-01", "missing"), "dates")
  expect_error(build_reports("test", "2026-07-01", "2026-09-30", "missing"), "ledger")
})


test_that("reporting dates filter posted GL transactions and dated mileage", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.csv")))
  config <- fixture()
  config$general_ledger <- sub("xlsx$", "csv", config$general_ledger)
  config$period_end <- "2026-07-31"
  mileage <- tempfile(fileext = ".csv")
  write.csv(data.frame(Project = "Meadow Restoration", Date = c("2026-07-12", "2026-08-12"), Miles = c(10, 200)), mileage, row.names = FALSE)
  config$mileage_log <- mileage
  config$mileage_sheet_map <- character()
  result <- do.call(build_reports, config)
  transactions <- read.csv(file.path(result$paths$internal, "Standardized_GL_Transactions.csv"))
  expect_equal(nrow(transactions), 9L)
  expect_true(all(as.Date(transactions$transaction_date) <= as.Date("2026-07-31")))
  excluded <- read.csv(file.path(result$paths$internal, "GL_Transactions_Outside_Period.csv"))
  expect_equal(nrow(excluded), 18L)
  miles <- read.csv(file.path(result$paths$internal, "Mileage_Summary.csv"))
  expect_equal(miles$miles, 10)
  expect_equal(miles$mileage_cost, 7.65)
  receipts <- read.csv(file.path(result$paths$internal, "Receipt_Reconciliation.csv"))
  expect_equal(sum(receipts$match_status == "Matched"), 3L)
})

test_that("missing mileage and receipts remain pending instead of zero", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  config <- fixture()
  config$receipt_folder <- ""
  config$mileage_log <- ""
  config$mileage_sheet_map <- character()
  result <- do.call(build_reports, config)
  categories <- read.csv(file.path(result$paths$internal, "Financial_Report_Category_Draft.csv"))
  expect_true(all(is.na(categories$current_period_spend[categories$category %in% c("Expenses", "Mileage")])))
  expect_false(any(file.exists(result$project_deliverables$manifest$Receipt_Packet)))
  config$root <- tempfile("reports_")
  mileage <- tempfile(fileext = ".csv")
  write.csv(data.frame(Project = "Meadow Restoration", Date = "2026-07-12", Miles = NA_real_), mileage, row.names = FALSE)
  config$mileage_log <- mileage
  result <- do.call(build_reports, config)
  miles <- read.csv(file.path(result$paths$internal, "Mileage_Summary.csv"))
  expect_true(is.na(miles$miles))
  expect_true(is.na(miles$mileage_cost))
  expect_match(miles$status, "Review")
})

test_that("dated receipts do not match a different date on amount alone", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  config <- fixture()
  receipt <- file.path(config$receipt_folder, "2026-07-12_Sample_Supply_$75.25.pdf")
  folder <- tempfile(); dir.create(folder)
  file.copy(receipt, file.path(folder, sub("2026-07-12", "2026-08-01", basename(receipt))))
  config$receipt_folder <- folder
  result <- do.call(build_reports, config)
  receipts <- read.csv(file.path(result$paths$internal, "Receipt_Reconciliation.csv"))
  expect_identical(receipts$receipt_date, "2026-08-01")
  expect_identical(basename(receipts$receipt_file), "2026-08-01_Sample_Supply_$75.25.pdf")
  expect_false(any(receipts$match_status == "Matched"))
})

test_that("colliding project folders fail before writing project workbooks", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.csv")))
  config <- fixture()
  ledger <- read.csv(sub("xlsx$", "csv", config$general_ledger), check.names = FALSE)
  ledger[["Grant and Program name"]] <- c("A/B", "A:B", "Other")[match(ledger[["Grant and Program name"]], names(config$mileage_sheet_map))]
  csv <- tempfile(fileext = ".csv"); write.csv(ledger, csv, row.names = FALSE)
  config$general_ledger <- csv
  config$mileage_log <- ""
  config$mileage_sheet_map <- character()
  config$receipt_folder <- ""
  expect_error(do.call(build_reports, config), "duplicate output folder names")
})


test_that("packets never silently omit missing files and support PNG receipts", {
  skip_if_not_installed("magick")
  png <- tempfile(fileext = ".png")
  grDevices::png(png, width = 200, height = 200)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Test receipt")
  grDevices::dev.off()
  output <- tempfile(fileext = ".pdf")
  expect_identical(combine_receipt_pdfs(png, output), output)
  expect_equal(qpdf::pdf_length(output), 1L)
  expect_true(is.na(combine_receipt_pdfs(c(png, tempfile(fileext = ".pdf")), tempfile(fileext = ".pdf"))))
})


test_that("Shiny launches the package without sourcing R files into a shared environment", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, "R", "_disable_autoload.R")))
  shared <- new.env(parent = globalenv())
  shiny::loadSupport(appDir = root, renv = shared, globalrenv = NULL)
  expect_length(ls(shared, all.names = TRUE), 0L)
})
