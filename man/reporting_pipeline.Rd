\name{reporting_pipeline}
\alias{prepare_quarter}
\alias{run_quarterly_pipeline}
\title{Prepare inputs and execute the reporting pipeline}
\usage{prepare_quarter(quarter_id, period_start, period_end,
 included_projects = "All projects in GL", general_ledger = "",
 receipt_folder = "", mileage_log = "", mileage_sheet_map = character(),
 mileage_rate = 0.765, fringe_rate = 0.18, indirect_rate = 0.10,
 root = "reports", project_output_root = "")
run_quarterly_pipeline(paths, delivery = c("both", "financial_report", "smartsheet_draft"))}
\arguments{
\item{quarter_id}{Report name.}
\item{period_start, period_end}{Reporting dates.}
\item{included_projects}{Project scope, separated by semicolons.}
\item{general_ledger, receipt_folder, mileage_log}{Input file or folder paths.}
\item{mileage_sheet_map}{Named project-to-worksheet mapping.}
\item{mileage_rate, fringe_rate, indirect_rate}{Approved numeric rates.}
\item{root}{Report storage folder.}
\item{project_output_root}{Optional project deliverable folder.}
\item{paths}{Paths returned by prepare_quarter.}
\item{delivery}{Requested deliverables.}}
\description{Lower-level orchestration functions. Use build_reports for a validated complete run.}
\value{prepare_quarter returns paths; run_quarterly_pipeline returns paths, QC checks, delivery status, and project deliverables.}
