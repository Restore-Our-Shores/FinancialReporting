# Financial Reporting Workflow Handoff

## Current entry point

The proof-of-concept is a local Shiny app. Open `FinancialReports.Rproj` in RStudio and run:

```r
shiny::runApp("shiny_app", launch.browser = TRUE)
```

This replaces the batch-file launcher. Keeping the entry point as one visible R command is simpler for users who can open the project in RStudio.

## Workflow design

The app has deliberately separate paths:

- **Quarterly report** creates the formal retained package in `quarters/<Report Name>/`.
- **Smartsheet refresh** creates a dashboard update draft in `smartsheet_refreshes/<Refresh ID>/`; it may be run more often than quarterly.

Both use the same core functions and safeguards. Each retained record includes copied inputs, a metadata snapshot, settings, a run log, internal working outputs, and the requested delivery output. Before the first run from a code-only clone, the user selects the local private metadata folder on the Setup tab.

## What users provide

- A report name and user-selected reporting start/end dates.
- General ledger with transaction detail; after upload, scope is selected from its `Grant and Program name` dropdown. Payroll is read directly from the GL.
- Receipt files in one flat folder or project folders. The filename must include its amount and should include a date and vendor, for example `2026-01-15_Vendor_$125.50.pdf`.
- Maggie's quarterly mileage workbook. When it contains project worksheets, the user confirms the GL-project-to-worksheet matches before the run.
- Mileage reimbursement, fringe, and indirect rates.

For a quarterly run, the user can choose an external deliverables folder. The app creates `<Report Name>/<Project>/` beneath it with `Project_Summary.xlsx`, `Receipt_Packet.pdf`, `Itemized_Expenses.xlsx`, and `Payroll_By_Date_and_Staff.xlsx`. Itemized expenses and their receipt packet share the same chronological order (receipt filename date, otherwise matched GL document date).

Missing inputs remain visibly pending. Expenses are accepted only after receipt/GL check passes. The Smartsheet output is an import draft, not a direct publication.

## Key files

- `shiny_app/app.R` - local user interface.
- `R/quarterly_reporting_core.R` - retained-record setup and delivery routing.
- `RunQuarterlyReport.R` - internal calculation engine.
- `data/Metadata/` - shared mappings, rules, and budgets to maintain deliberately.

## Remaining production decisions

1. Test with completed mileage and receipt inputs to demonstrate a fully passing run.
2. Confirm account-to-category mapping, budgets, and any funder-specific presentation rules.
3. Decide whether the accepted `Smartsheet_Draft.csv` process should later be replaced with direct Smartsheet integration.
