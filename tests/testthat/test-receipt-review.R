test_that("prepared receipt review generates reports only after finishing or skipping", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  for (decision in c("apply_edits", "skip")) {
    initial <- do.call(build_reports, c(fixture(), list(prepare_only = TRUE)))
    expect_null(initial$project_deliverables)
    expect_length(list.files(initial$paths$quarterly), 0L)
    holder <- new.env(); holder$value <- reactiveVal(initial); holder$downloads <- 0L
    shiny::testServer(mod_receipt_review_server, args = list(result = holder$value,
      on_saved = function(result) holder$value(result),
      on_download = function() holder$downloads <- holder$downloads + 1L), {
      session$setInputs(project = "", open_only = FALSE)
      first <- read_receipt_review(initial)$expenses$source_row[1]
      selected_line(first)
      session$setInputs(receipt_choice = "0", unmatch = 1)
      expect_null(holder$value()$project_deliverables)
      if (decision == "apply_edits") session$setInputs(apply_edits = 1) else session$setInputs(skip = 1)
      expect_equal(holder$downloads, 1L)
      expect_true(file.exists(holder$value()$project_deliverables$smartsheet_summary))
      saved <- read_receipt_review(holder$value())
      expect_equal(sum(!is.na(saved$assignments$source_row)), if (decision == "apply_edits") 8L else 9L)
      expect_false(dirty())
    })
  }
})

test_that("receipt edits rebuild complete reports without altering the original run", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  result <- do.call(build_reports, fixture())
  original <- read_receipt_review(result)
  expect_equal(nrow(original$expenses), 9L)
  expect_equal(sum(!is.na(original$assignments$source_row)), 9L)
  receipt1 <- 1L; receipt2 <- 2L
  line1 <- original$assignments$source_row[receipt1]
  line2 <- original$assignments$source_row[receipt2]
  expect_error(edit_receipt_review(original, line1, receipt_id = receipt2), "belongs to another line")
  moved <- edit_receipt_review(original, line1, receipt_id = receipt2, reassign = TRUE)
  expect_true(is.na(moved$assignments$source_row[receipt1]))
  expect_equal(moved$assignments$source_row[receipt2], line1)
  expect_false(moved$lines$reviewed[match(line2, moved$lines$source_row)])
  unmatched <- edit_receipt_review(original, line1, receipt_id = 0L, note = "Wrong receipt")
  unmatched <- edit_receipt_review(unmatched, line1, reviewed = TRUE, note = "Awaiting corrected receipt")
  updated <- save_receipt_review(result, unmatched, reviewer = "Test reviewer")
  expect_false(identical(updated$paths$quarter, result$paths$quarter))
  expect_equal(sum(!is.na(read_receipt_review(result)$assignments$source_row)), 9L)
  expect_equal(sum(!is.na(read_receipt_review(updated)$assignments$source_row)), 8L)
  saved <- read_receipt_review(updated)
  expect_true(saved$lines$reviewed[match(line1, saved$lines$source_row)])
  expect_identical(saved$lines$note[match(line1, saved$lines$source_row)], "Awaiting corrected receipt")
  project <- original$expenses$project_standard[match(line1, original$expenses$source_row)]
  qc <- read.csv(file.path(updated$paths$internal, "Expense_Receipt_QC.csv"))
  expect_false(qc$status[qc$project_standard == project] == "Pass")
  workbook <- readxl::read_xlsx(file.path(updated$project_deliverables$root, safe_output_name(project), "Detailed_Expenses_2026-07-01_to_2026-09-30.xlsx"))
  expect_true(anyNA(workbook$`File name`))
  packet <- updated$project_deliverables$manifest$Receipt_Packet[updated$project_deliverables$manifest$Project == project]
  expect_equal(qpdf::pdf_length(packet), 2L)
  log <- read.csv(file.path(updated$paths$quarter, "review", "Receipt_Review_Log.csv"))
  expect_true(any(log$source_row == line1 & log$reviewer == "Test reviewer"))
  expect_true(file.exists(file.path(updated$paths$quarter, "review", "Original_Receipt_Reconciliation.csv")))
  restored <- edit_receipt_review(saved, line1, receipt_id = receipt1, note = "Correct receipt restored")
  restored <- edit_receipt_review(restored, line1, reviewed = TRUE)
  corrected <- save_receipt_review(updated, restored)
  qc <- read.csv(file.path(corrected$paths$internal, "Expense_Receipt_QC.csv"))
  expect_true(all(qc$status == "Pass"))
  expect_identical(dirname(dirname(corrected$paths$quarter)), dirname(result$paths$quarter))
  expect_equal(qpdf::pdf_length(corrected$project_deliverables$manifest$Receipt_Packet[corrected$project_deliverables$manifest$Project == project]), 3L)
  archive <- tempfile(fileext = ".zip"); export_report_bundle(corrected, archive)
  expect_false(any(grepl("(^|/)audit(/|$)", utils::unzip(archive, list = TRUE)$Name, ignore.case = TRUE)))
  broken <- restored; broken$assignments$source_row[2] <- broken$assignments$source_row[1]
  expect_error(save_receipt_review(corrected, broken), "one receipt")
  expect_error(edit_receipt_review(original, line1, receipt_id = 9999L), "from this report")
})

test_that("manual swaps cannot pass QC just because project totals cancel", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  config <- fixture()
  ledger <- read.csv(sub("xlsx$", "csv", config$general_ledger), check.names = FALSE)
  ledger$Doc <- "REPEATED-DOCUMENT"
  csv <- tempfile(fileext = ".csv"); write.csv(ledger, csv, row.names = FALSE)
  config$general_ledger <- csv
  result <- do.call(build_reports, config)
  workbook <- readxl::read_xlsx(file.path(result$project_deliverables$root, "Meadow_Restoration", "Detailed_Expenses_2026-07-01_to_2026-09-30.xlsx"))
  expect_equal(nrow(workbook), 4L)
  expect_equal(length(unique(stats::na.omit(workbook$`File name`))), 3L)
  state <- read_receipt_review(result)
  # Choose two receipts belonging to the same project, with different amounts.
  first <- 1L
  project <- state$expenses$project_standard[match(state$assignments$source_row[first], state$expenses$source_row)]
  choices <- which(state$receipts$project_standard == project & state$receipts$receipt_amount != state$receipts$receipt_amount[first])
  second <- choices[1]
  line1 <- state$assignments$source_row[first]; line2 <- state$assignments$source_row[second]
  state <- edit_receipt_review(state, line1, receipt_id = second, reassign = TRUE)
  state <- edit_receipt_review(state, line2, receipt_id = first)
  updated <- save_receipt_review(result, state)
  qc <- read.csv(file.path(updated$paths$internal, "Expense_Receipt_QC.csv"))
  expect_identical(qc$status[qc$project_standard == project], "Review receipt-to-line differences")
  expect_equal(qc$difference[qc$project_standard == project], 0)
})

test_that("the review module keeps drafts separate and can skip or apply", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  initial <- do.call(build_reports, fixture())
  holder <- new.env(); holder$value <- reactiveVal(initial); holder$downloads <- 0L
  shiny::testServer(mod_receipt_review_server, args = list(result = holder$value,
    on_saved = function(result) holder$value(result), on_download = function() holder$downloads <- holder$downloads + 1L), {
    session$setInputs(project = "", open_only = FALSE)
    first <- read_receipt_review(initial)$expenses$source_row[1]
    selected_line(first)
    session$setInputs(receipt_choice = "0", unmatch = 1)
    expect_true(dirty())
    expect_equal(sum(!is.na(read_receipt_review(initial)$assignments$source_row)), 9L)
    session$setInputs(skip = 1)
    expect_false(dirty())
    expect_equal(sum(!is.na(draft()$assignments$source_row)), 9L)
    expect_equal(holder$downloads, 1L)
    session$setInputs(unmatch = 2)
    expect_true(dirty())
    session$setInputs(apply_edits = 1)
    expect_false(dirty())
    expect_equal(holder$downloads, 2L)
    expect_equal(sum(!is.na(read_receipt_review(holder$value())$assignments$source_row)), 8L)
  })
})


test_that("expenses with no uploaded receipts can still be reviewed and downloaded", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.xlsx")))
  config <- fixture(); config$receipt_folder <- ""
  result <- do.call(build_reports, config)
  state <- read_receipt_review(result)
  expect_equal(nrow(state$receipts), 0L)
  expect_true(all(review_expense_table(state)$Check == "No receipt"))
  state <- edit_receipt_review(state, state$lines$source_row[1], reviewed = TRUE, note = "Receipt requested")
  updated <- save_receipt_review(result, state)
  saved <- read_receipt_review(updated)
  expect_equal(sum(saved$lines$reviewed), 1L)
  expect_true(all(review_expense_table(saved)$Check == "No receipt"))
  expect_false(any(file.exists(updated$project_deliverables$manifest$Receipt_Packet)))
  archive <- tempfile(fileext = ".zip"); export_report_bundle(updated, archive)
  expect_true(file.exists(archive))
})


test_that("a report containing only payroll can skip the empty expense review", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "simulated_data", "Simulated_General_Ledger.csv")))
  config <- fixture()
  ledger <- read.csv(sub("xlsx$", "csv", config$general_ledger), check.names = FALSE)
  ledger <- ledger[grepl("Gross Salary", ledger$GL), , drop = FALSE]
  csv <- tempfile(fileext = ".csv"); write.csv(ledger, csv, row.names = FALSE)
  config$general_ledger <- csv; config$receipt_folder <- ""
  result <- do.call(build_reports, config)
  value <- reactiveVal(result); holder <- new.env(); holder$download <- FALSE
  shiny::testServer(mod_receipt_review_server, args = list(result = value, on_saved = function(result) value(result),
    on_download = function() holder$download <- TRUE), {
    session$setInputs(project = "", open_only = FALSE)
    expect_equal(nrow(table_data()), 0L)
    expect_match(output$review_body$html, "Expense lines")
    session$setInputs(skip = 1)
    expect_true(holder$download)
    expect_false(dirty())
  })
})
