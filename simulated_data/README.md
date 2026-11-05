# Simulated reporting inputs

All records, projects, staff, vendors, and purchases in this folder are fictional.
The files cover July 1 through September 30, 2026 (calendar Q3).

## Use in the application

1. Enter a report name such as `SIMULATED_2026_Q3`.
2. Set Start date to **2026-07-01** and End date to **2026-09-30**.
3. Upload `Simulated_General_Ledger.xlsx` as the General Ledger.
4. Upload `Simulated_Mileage_Log.xlsx` as the mileage workbook.
5. Include all three projects and match each to the worksheet with the same name.
6. Select this folder's `receipts` directory. It contains nine individual PDFs.
7. Select this folder's `output` directory, or another empty folder, for reports.
8. Use mileage rate **0.765**, fringe rate **0.18**, and indirect rate **0.10**.
9. Build project reports and compare the results with the controls below. Every run also creates `SmartsheetInput.xlsx` in the selected output folder: a single worksheet with nine rows (Expenses, Payroll, and Mileage for each of the three projects) for manual entry into Smartsheet.

## Contents and relationships

- The GL contains 27 transactions: 18 hourly payroll entries and nine expense entries. The Excel workbook follows the reference ledger's report headings, column order and widths, account-category labels, blank-row spacing, and subtotal layout. It has an account-grouped report and a transaction sheet for importing. Unused account categories have zero totals.
- The populated account categories are `633004 - Supplies- ROS`, `633012 - Printing and Copier -ROS`, `633039 - Equipment Rental - ROS`, and `633150 - Gross Salary - ROS`. The remaining reference categories are shown as empty groups. These are account labels from the reference; all transaction records remain fictional.
- `Simulated_General_Ledger.csv` is retained as a flat alternative with the same transactions and account categories.
- Every expense has exactly one receipt with the same document number, date, vendor, project, and amount.
- Receipt filenames contain date, vendor, and dollar amount for the app's matching logic.
- The mileage workbook has three project worksheets and nine trips. Trip dates match the expense and payroll dates, all within the reporting period. Miles calculate as ending odometer minus starting odometer.
- Payroll descriptions include hours and hourly rates. The flat CSV also includes separate Hours and Hourly Rate columns. The current app's payroll output lists dated payroll amounts and staff; it does not yet expose those extra fields as separate output columns.
- Source dates are July 12, August 12, and September 12, 2026. These inputs intentionally contain no out-of-period records or ambiguous receipt matches.
- No metadata files or budget sources are required. Budget amounts and budget percentages remain blank in the app.

## Expected controls

| Project | Payroll (USD) | Expenses / receipts (USD) | Miles | Mileage at 0.765 (USD) |
| --- | ---: | ---: | ---: | ---: |
| Meadow Restoration | 2,400.00 | 285.75 | 135 | 103.275 |
| River Monitoring | 2,730.00 | 405.75 | 165 | 126.225 |
| Woodland Outreach | 3,060.00 | 525.75 | 195 | 149.175 |
| Total | 8,190.00 | 1,217.25 | 495 | 378.675 |

Mileage costs retain three decimal places here to reflect the exact rate product.
Each project's expense receipt check should report **Pass**, with three matched receipts and zero difference. All nine receipts should have status **Matched**. Payroll plus GL expenses totals **9,407.25**; mileage is calculated separately from the mileage log to avoid double counting.

The output directory starts empty. Generated reports are excluded from Git; simulated inputs are safe to keep with the code.
