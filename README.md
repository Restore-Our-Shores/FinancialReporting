# Financial Reporting Automation

## Start here

Open `FinancialReports.Rproj` in RStudio. In its Console, run:

```r
shiny::runApp("shiny_app", launch.browser = TRUE)
```

The app opens in a browser and is the normal entry point for this project. This assumes R plus the `shiny` and `shinyFiles` packages are installed. If needed, run `install.packages(c("shiny", "shinyFiles"))` once. If the project is opened another way, first set R's working directory to this folder.

> **Code-only repository:** Financial inputs, example GL data, receipts, mileage, and generated reports are intentionally excluded. Select the current authorized inputs in the app for each run.

## Build reports

For fictional inputs that work together, see [simulated_data/README.md](simulated_data/README.md). This set includes a GL, nine matching PDF receipts, a mileage workbook, and an output folder for July-September 2026.

- **Quarterly report** creates a retained, reviewable report package. Enter a report name and reporting dates; choose the GL, mileage workbook (up to 100 MB), a flat receipt folder or receipt subfolders; select the GL projects to include; confirm any mileage worksheet matches; set approved rates; and choose an output folder. The app creates one project-named folder directly inside the selected output folder. Each contains `Category_Spend_Summary.xlsx`, `Hourly_Time_Entries.xlsx`, and `Detailed_Expenses.xlsx`, plus a receipt packet when one can be created.
- Every run also writes **SmartsheetInput.xlsx** directly in the selected output folder. Its single worksheet uses the reference columns: Project, Payroll, Expenses, Mileage, Spent Amount, Total Budget, and Percent Spent. Each selected project appears three times, in Expenses, Payroll, then Mileage order. Only the corresponding category column and Spent Amount are populated per row. Budget fields remain blank without a budget source. Copy the desired values into Smartsheet by hand.

## Key safeguards

- After the GL uploads, select one or more projects from its `Grant and Program name` dropdown. All projects are selected by default.
- Reporting start and end dates are chosen by the user for every run.
- Every quarterly run snapshots its selected inputs, settings, and QC evidence so later work does not alter the previous report.
- The pipeline leaves a result blank when a required source or reconciliation is missing; it never replaces missing information with zero.
- Payroll is read from the GL. Expenses are used only after receipt totals tie to GL expenses. Per-project packets include only confirmed receipt/GL matches; uncertain matches stay in the review files.
- `SmartsheetInput.xlsx` is generated automatically for manual entry into Smartsheet.

See [QUARTERLY_REPORTING_GUIDE.md](QUARTERLY_REPORTING_GUIDE.md) for operating steps and [WORKFLOW_HANDOFF.md](WORKFLOW_HANDOFF.md) for the full handoff record.
