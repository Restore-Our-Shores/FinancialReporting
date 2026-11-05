# Financial Reporting Workflow Handoff

## Current entry point

The proof-of-concept is a local Shiny app. Open `FinancialReports.Rproj` in RStudio and run:

```r
shiny::runApp("shiny_app", launch.browser = TRUE)
```

This replaces the batch-file launcher. Keeping the entry point as one visible R command is simpler for users who can open the project in RStudio.

## Workflow design

The Quarterly report page creates the retained package in `quarters/<Report Name>/`, project folders, and an automatic `SmartsheetInput.xlsx` summary in the selected output folder. The separate Smartsheet refresh page has been removed. Each retained record includes copied inputs, settings, a run log, internal working outputs, and the requested delivery output.

## What users provide

- A report name and user-selected reporting start/end dates.
- General ledger with transaction detail; after upload, scope is selected from its `Grant and Program name` dropdown. Payroll is read directly from the GL.
- Receipt files in one flat folder or project folders. The filename must include its amount and should include a date and vendor, for example `2026-01-15_Vendor_$125.50.pdf`.
- Maggie's quarterly mileage workbook. When it contains project worksheets, the user confirms the GL-project-to-worksheet matches before the run.
- Mileage reimbursement, fringe, and indirect rates.

For a quarterly run, the user selects an output folder. The app creates `<Project>/` directly inside it with `Category_Spend_Summary.xlsx`, `Receipt_Packet.pdf`, `Detailed_Expenses.xlsx`, and `Hourly_Time_Entries.xlsx`.

Missing inputs remain visibly pending. Expenses are accepted only after the receipt/GL check passes. The Smartsheet workbook contains one worksheet with three consecutive rows per selected project: Expenses, Payroll, and Mileage. It uses the reference's seven columns and is intended for manual entry into Smartsheet. Budget fields stay blank without a budget source.

## Key files

- `shiny_app/app.R` - local user interface.
- `R/quarterly_reporting_core.R` - retained-record setup and delivery routing.
- `RunQuarterlyReport.R` - internal calculation engine.

## Remaining production decisions

1. Test with completed mileage and receipt inputs to demonstrate a fully passing run.
2. Confirm the GL account-description rules used to distinguish payroll from expenses, and any funder-specific presentation rules.
3. Confirm the manual entry workflow for `SmartsheetInput.xlsx` with the reporting team.
