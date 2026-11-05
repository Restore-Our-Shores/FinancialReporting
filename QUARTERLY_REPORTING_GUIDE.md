# Quarterly Financial Reporting Guide

## Open the app

1. Open `FinancialReports.Rproj` in RStudio.
2. In the Console, run:

   ```r
   shiny::runApp("shiny_app", launch.browser = TRUE)
   ```

3. Use the browser tab that opens. Leave the R session open while using the app.

If RStudio is not used, set the working directory to the `FinancialReports` project folder before running the same line. R plus the `shiny` and `shinyFiles` packages must be installed.

## Quarterly report tab

Use this path for the formal reporting package.

1. Enter a **Report Name**.
2. Import the GL to fill **Reporting dates** with its earliest and latest posted dates. Both fields remain editable; change them to the desired reporting period. Importing a different GL refreshes these dates.
3. Choose the General Ledger (required), then upload Maggie's **Mileage workbook** when available. Workbooks up to 100 MB are accepted.
4. After the GL uploads, choose one or more projects from the `Grant and Program name` dropdown. All projects are selected by default.
5. If the mileage workbook has a worksheet per project, confirm the suggested **project-to-worksheet matches**. Worksheet names do not need to match the GL exactly. Leave a project blank when it has no mileage worksheet.
6. Select the **output folder**. The app creates one folder per selected project directly in this location.
7. Choose a receipts directory. You can put every receipt in one flat folder; project subfolders are optional. Each filename must include the amount, and should include the date and vendor, for example `2026-01-15_Vendor_$125.50.pdf`.
8. Enter the approved mileage reimbursement, fringe, and indirect rates.
9. Select **Build project reports**, then use **Open selected output folder** to view the reports. The app displays the saved location; supporting checks remain in the retained report files.

The complete retained record is in `quarters/<Report Name>/` (with unsafe folder characters replaced). Reuse a report name only to correct that same report.

## Per-project deliverables

For every selected project, the chosen output folder receives `<Project>/` containing:

- `Category_Spend_Summary.xlsx` - payroll from the GL, fringe, mileage, expenses, direct costs, indirect costs, invoice estimate, and invoice status.
- `Receipt_Packet.pdf` - confirmed matched receipt files, ordered by receipt filename date; when that is unavailable, the matched GL document date is used.
- `Detailed_Expenses.xlsx` - GL expense transactions with any confirmed linked receipt file.
- `Hourly_Time_Entries.xlsx` - GL payroll/time-entry transactions sorted by date and vendor/staff name.

Receipt matching uses the filename amount first, then uses any filename date and vendor words to narrow the GL match. Missing or ambiguous matches remain visible for review and are not added to the receipt packet.

## All-project Smartsheet summary

Every report run creates `SmartsheetInput.xlsx` directly in the selected output folder, alongside the project folders. It contains one worksheet with the same seven columns as the reference input. Each included project repeats three times: Expenses, Payroll, and Mileage, in that order. Each row fills its corresponding category column and Spent Amount; other category columns stay blank. These are category actuals, excluding calculated fringe and indirect costs. Missing or unconfirmed amounts stay blank. With no budget input, Total Budget and Percent Spent remain blank. Copy the values into Smartsheet manually as desired.

## Expected inputs

| Input | Expected format |
|---|---|
| General ledger | Finance transaction export with `Posted dt.`, `Amount`, `GL`, and `Grant and Program name`. Payroll is read from this file. |
| Receipts | PDF/image files in one folder or project-named subfolders. Filename includes amount; `YYYY-MM-DD` / `YYYYMMDD` date and vendor wording are strongly recommended. |
| Mileage | Maggie's file with `Project` and `Miles`. |

Optional input templates are in `data/input_*`.

## Review order

1. Confirm `Project_Scope.csv` reflects the intended scope.
2. Check `Input_Readiness.csv` and `Reconciliation_Checks.csv`.
3. Review `Payroll_GL_Summary.csv`, `Payroll_Detail.csv`, and `Expense_Receipt_QC.csv`.
4. Review `Quarterly_Report_Category_Draft.csv`, `Project_Deliverables_Manifest.csv`, and the all-project `SmartsheetInput.xlsx` summary.

The app does not treat a missing source as zero. Resolve source or QC issues, then rerun the same report name to refresh that record.
