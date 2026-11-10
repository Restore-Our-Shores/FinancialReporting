fixture <- function() {
  root <- normalizePath(testthat::test_path("..", "..", "simulated_data"), mustWork = TRUE)
  projects <- c("Meadow Restoration", "River Monitoring", "Woodland Outreach")
  list(quarter_id = "test_report", period_start = "2026-07-01", period_end = "2026-09-30",
    general_ledger = file.path(root, "Simulated_General_Ledger.xlsx"),
    receipt_folder = file.path(root, "receipts"), mileage_log = file.path(root, "Simulated_Mileage_Log.xlsx"),
    mileage_sheet_map = stats::setNames(projects, projects), root = tempfile("reports_"))
}
