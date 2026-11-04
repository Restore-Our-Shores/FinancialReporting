# Quarterly Financial Reporting Guide

## Open the app

1. Open `FinancialReports.Rproj` in RStudio.
2. In the Console, run:

   ```r
   shiny::runApp("shiny_app", launch.browser = TRUE)
   ```

3. Use the browser tab that opens. Leave the R session open while using the app.

If RStudio is not used, set the working directory to the `FinancialReports` project folder before running the same line. R and the `shiny` package must be installed.

## Quarterly report tab

Use this path for the formal reporting package.

1. Enter a **Name of report**. It can be any descriptive name, such as `Q2 2026 Financial Report`.
2. Choose the General Ledger (required), then upload Maggie's **Mileage workbook** when available. Workbooks up to 100 MB are accepted.
3. After the GL uploads, choose one or more projects from the `Grant and Program name` dropdown. All projects are selected by default.
4. If the mileage workbook has a worksheet per project, confirm the suggested **project-to-worksheet matches**. Worksheet names do not need to match the GL exactly. Leave a project blank when it has no mileage worksheet.
5. Select the optional **project deliverables output folder**. The app creates one report folder and one subfolder per selected project there. If it is left blank, the folders are created inside the retained report record.
6. Choose a receipt folder if packets are ready. You can put every receipt in one flat folder; project subfolders are optional. Each filename must include the amount, and should include the date and vendor, for example `2026-01-15_Vendor_$125.50.pdf`.
7. Add the Finance payroll summary and prior-spend file when available.
8. Enter the approved mileage reimbursement, fringe, and indirect rates.
9. Select **Build quarterly report package** and review the tables shown in the app.

The complete retained record is in `quarters/<Report name>/` (with unsafe folder characters replaced). Reuse a report name only to correct that same report.

## Per-project deliverables

For every selected project, the chosen output folder receives `<Report name>/<Project>/` containing:

- `Project_Summary.xlsx` - payroll, fringe, mileage, expenses, direct costs, indirect costs, invoice estimate, and invoice status.
- `Receipt_Packet.pdf` - confirmed matched receipt files, ordered by receipt filename date; when that is unavailable, the matched GL document date is used.
- `Itemized_Expenses.xlsx` - the same confirmed receipt/GL matches in the same packet order.
- `Payroll_By_Date_and_Staff.xlsx` - payroll detail sorted by date and staff name.

Receipt matching uses the filename amount first, then uses any filename date and vendor words to narrow the GL match. Missing or ambiguous matches remain visible for review and are not added to the receipt packet. For the payroll detail file, the Finance input also needs date and staff/employee-name columns.

## Smartsheet refresh tab

Use this path whenever the dashboard needs a new draft, including between quarter-end reporting cycles.

1. Enter a distinct refresh name, for example `2026_Q2_smartsheet-current`.
2. Choose the GL and the optional mileage workbook or other supporting files.
3. Choose one or more projects from the GL-populated dropdown. If the mileage workbook uses project worksheets, confirm the suggested matches.
4. Set the approved rates and select **Build Smartsheet draft**.
5. Review the readiness and QC checks, then use `Smartsheet_Draft.csv` from the displayed output folder.

The refresh record is saved separately in `smartsheet_refreshes/<Refresh ID>/`; it does not change a quarterly package.

## Expected inputs

| Input | Expected format |
|---|---|
| General ledger | Finance transaction export with `Posted dt.`, `Amount`, `GL`, and `Grant and Program name`. |
| Receipts | PDF/image files in one folder or project-named subfolders. Filename includes amount; `YYYY-MM-DD` / `YYYYMMDD` date and vendor wording are strongly recommended. |
| Mileage | Maggie's file with `Project` and `Miles`. |
| Finance payroll | File with `Project` and `Payroll Amount`; include payroll date and staff/employee name to create the by-date payroll workbook. |
| Prior spend | File with `Project`, `Category`, and `Prior Spend`. |

Optional input templates are in `data/input_*`.

## Review order

1. Confirm `Project_Scope.csv` reflects the intended scope.
2. Check `Input_Readiness.csv` and `Reconciliation_Checks.csv`.
3. Review `Payroll_GL_QC.csv` and `Expense_Receipt_QC.csv`.
4. Review `Quarterly_Report_Category_Draft.csv` and `Project_Deliverables_Manifest.csv` for the quarterly package, or `Smartsheet_Draft.csv` for a refresh.

The app does not treat a missing source as zero. Resolve source or QC issues, then rerun the same ID to refresh that record.
