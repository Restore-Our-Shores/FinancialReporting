# Financial Reporting Automation

## Start here

Open `FinancialReports.Rproj` in RStudio. In its Console, run:

```r
shiny::runApp("shiny_app", launch.browser = TRUE)
```

The app opens in a browser and is the normal entry point for this project. This assumes R and the `shiny` package are installed. If the project is opened another way, first set R's working directory to this folder.

> **Code-only repository:** Financial inputs, project metadata, example GL data, receipts, payroll, mileage, and generated reports are intentionally excluded. Use authorized local copies of the required `data/Metadata` files and inputs when running the app.

## Two independent workflows

- **Quarterly report** creates a retained, reviewable report package. Choose the GL, mileage workbook (up to 100 MB), a flat receipt folder or receipt subfolders, Finance payroll summary, and prior-spend source; enter a report name, select the GL projects to include, confirm any mileage worksheet matches, set approved rates, and choose an optional project-deliverables folder. Its retained files are saved in `quarters/<Report name>/`; the chosen output folder receives one summary, receipt packet, itemized-expense workbook, and payroll-by-date-and-staff workbook per project.
- **Smartsheet refresh** creates an update draft for the dashboard whenever it is needed. It does not need to wait for quarter-end and is saved separately in `smartsheet_refreshes/<Refresh ID>/`.

Both workflows use the same validation and calculation rules, but they intentionally keep their inputs and output records separate.

## Key safeguards

- After the GL uploads, select one or more projects from its `Grant and Program name` dropdown. All projects are selected by default.
- Every quarterly run snapshots its inputs, metadata, settings, and QC evidence so later work does not alter the previous quarter.
- The pipeline leaves a result blank when a required source or reconciliation is missing; it never replaces missing information with zero.
- Payroll is used only after it ties to GL payroll, and expenses are used only after receipt totals tie to GL expenses. Per-project packets include only confirmed receipt/GL matches; uncertain matches stay in the review files.
- `Smartsheet_Draft.csv` is a review-ready import draft; the app does not publish directly to Smartsheet.

See [QUARTERLY_REPORTING_GUIDE.md](QUARTERLY_REPORTING_GUIDE.md) for operating steps and [WORKFLOW_HANDOFF.md](WORKFLOW_HANDOFF.md) for the full handoff record.
