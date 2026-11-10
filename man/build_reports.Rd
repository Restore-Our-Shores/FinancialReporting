\name{build_reports}
\alias{build_reports}
\title{Build financial reports without launching the app}
\usage{build_reports(quarter_id, period_start, period_end, general_ledger,
 receipt_folder = "", mileage_log = "", included_projects = "All projects in GL",
 mileage_sheet_map = character(), mileage_rate = 0.765, fringe_rate = 0.18,
 indirect_rate = 0.10, root = "reports", project_output_root = "",
 delivery = "financial_report")}
\arguments{
\item{quarter_id}{A new report name.}
\item{period_start, period_end}{Reporting dates in YYYY-MM-DD format.}
\item{general_ledger}{Path to a GL CSV or Excel file.}
\item{receipt_folder}{Optional folder containing receipts, searched recursively.}
\item{mileage_log}{Optional mileage CSV or Excel workbook.}
\item{included_projects}{All projects in GL, or project names separated by semicolons.}
\item{mileage_sheet_map}{Named character vector mapping GL projects to mileage sheets.}
\item{mileage_rate, fringe_rate, indirect_rate}{Approved numeric rates.}
\item{root}{Persistent folder for report snapshots and internal outputs.}
\item{project_output_root}{Optional folder receiving project deliverables.}
\item{delivery}{financial_report, smartsheet_draft, or both.}}
\description{Validates settings, snapshots inputs, and runs the same calculations and exports used by the Shiny application.}
\value{A list containing paths, checks, delivery status, and project deliverables.}
