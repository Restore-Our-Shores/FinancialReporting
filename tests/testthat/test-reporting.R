test_that("upload staging preserves metadata and duplicate basenames", {
  folder <- tempfile(); dir.create(folder)
  source <- tempfile(); writeLines("receipt", source)
  upload <- data.frame(name = c("2026-07-12 Vendor $10.00.pdf", "2026-07-12 Vendor $10.00.pdf"), datapath = source)
  stage_receipts(upload, folder)
  files <- list.files(folder, recursive = TRUE, full.names = TRUE)
  expect_length(files, 2L)
  expect_identical(basename(files), upload$name)
  ignored <- data.frame(name = "notes.txt", datapath = source)
  stage_receipts(ignored, folder)
  expect_length(list.files(folder, recursive = TRUE), 2L)
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
  expect_equal(nrow(summary), 3L)
  expect_identical(names(summary), c("Project", "Payroll", "Fringe", "Expenses", "Mileage", "Total Direct Costs", "Indirect", "Invoice Total"))
  expect_equal(summary$`Total Direct Costs`, summary$Payroll + summary$Fringe + summary$Expenses + summary$Mileage)
  expect_equal(summary$`Invoice Total`, summary$`Total Direct Costs` + summary$Indirect)
  expect_error(do.call(build_reports, config), "already exists")
  bundle <- tempfile(fileext = ".zip")
  export_report_bundle(result, bundle)
  entries <- utils::unzip(bundle, list = TRUE)$Name
  dated <- "2026-07-01_to_2026-09-30"
  expect_true(paste0("project_reports/Smartsheet_Input_", dated, ".xlsx") %in% entries)
  expect_false(any(grepl("(^|/)audit(/|$)", entries, ignore.case = TRUE)))
  expect_false(any(grepl("Manifest", entries, ignore.case = TRUE)))
  expect_equal(sum(grepl(paste0("^project_reports/.*/Receipt_Packet_", dated, "\\.pdf$"), entries)), 3L)
  extracted <- tempfile(); utils::unzip(bundle, exdir = extracted)
  expect_equal(readxl::read_xlsx(file.path(extracted, "project_reports", paste0("Smartsheet_Input_", dated, ".xlsx"))), summary)
  project <- result$project_deliverables$manifest$Project[1]
  folder <- file.path(result$project_deliverables$root, safe_output_name(project))
  expenses <- readxl::read_xlsx(file.path(folder, paste0("Detailed_Expenses_", dated, ".xlsx")))
  expect_identical(names(expenses), c("Posted Dt.", "Doc Dt.", "Amount", "File name"))
  expect_identical(expenses[["Posted Dt."]][nrow(expenses)], "TOTAL")
  expect_equal(expenses$Amount[nrow(expenses)], sum(expenses$Amount[-nrow(expenses)]))
  hourly <- readxl::read_xlsx(file.path(folder, paste0("Hourly_Time_Entries_", dated, ".xlsx")))
  expect_false("status" %in% names(hourly))
  expect_identical(tail(hourly$payroll_date, 3), c("TOTAL", "Fringe (18%)", "TOTAL"))
  expect_equal(tail(hourly$payroll_amount, 1), sum(tail(hourly$payroll_amount, 3)[1:2]))
  category <- readxl::read_xlsx(file.path(folder, paste0("Category_Spend_Summary_", dated, ".xlsx")))
  expect_true(any(grepl("18%", category$Category)))
  expect_true(any(grepl("0.765/mile", category$Category)))
})

test_that("Shiny runs the shared workflow with real browser upload paths", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  config <- fixture()
  upload <- function(files) data.frame(name = basename(files), datapath = files, size = file.info(files)$size, type = "")
  holder <- new.env()
  shiny::testServer(app_server, {
    session$setInputs(quarter_gl = upload(config$general_ledger), quarter_mileage = upload(config$mileage_log))
    expect_match(output$quarter_date_inputs$html, "2026-07-12")
    expect_match(output$quarter_date_inputs$html, "2026-09-12")
    projects <- names(config$mileage_sheet_map)
    session$setInputs(quarter_id = "browser_report", quarter_projects = projects,
      review_receipts = TRUE,
      quarter_start = "2026-07-01", quarter_end = "2026-09-30",
      quarter_mileage_rate = 0.765, quarter_fringe_rate = 0.18, quarter_indirect_rate = 0.10,
      quarter_receipts = upload(list.files(config$receipt_folder, full.names = TRUE)))
    session$setInputs(quarter_mileage_sheet_1 = projects[1], quarter_mileage_sheet_2 = projects[2], quarter_mileage_sheet_3 = projects[3])
    session$setInputs(run_quarterly = 1)
    expect_null(quarter_result())
    expect_null(quarter_prepared()$project_deliverables)
    expect_length(list.files(quarter_prepared()$paths$quarterly), 0L)
    expect_equal(nrow(read_receipt_review(quarter_prepared())$expenses), 9L)
    session$setInputs(skip_review = 1)
    result <- quarter_result()
    expect_false(is.null(result))
    qc <- read.csv(file.path(result$paths$internal, "Expense_Receipt_QC.csv"))
    expect_true(all(qc$status == "Pass"))
    expect_equal(sum(qc$receipt_count), 9)
    expect_true(file.exists(result$project_deliverables$smartsheet_summary))
    session$setInputs(review_receipts = FALSE, quarter_receipts = NULL, run_quarterly = 2)
    expect_false(is.null(quarter_result()))
    expect_true(file.exists(quarter_result()$project_deliverables$smartsheet_summary))
    expect_equal(nrow(read_receipt_review(quarter_result())$receipts), 0L)
    expect_match(output$quarter_downloads$html, "Download project reports")
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
