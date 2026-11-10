# Copy to config/report_config.R and supply your authorized paths and rates.
# Each report name/root combination must be new to preserve prior snapshots.
list(
  quarter_id = "2026_Q3",
  period_start = "2026-07-01",
  period_end = "2026-09-30",
  general_ledger = "path/to/General_Ledger.xlsx",
  receipt_folder = "path/to/receipts",
  mileage_log = "path/to/Mileage.xlsx",
  included_projects = "All projects in GL",
  # For a multi-sheet workbook, explicitly map GL projects to worksheet names:
  mileage_sheet_map = c("GL project name" = "Mileage worksheet name"),
  mileage_rate = 0.765,
  fringe_rate = 0.18,
  indirect_rate = 0.10,
  root = "reports",
  project_output_root = "",
  delivery = "financial_report"
)
