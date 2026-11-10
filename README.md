# Financial Reporting Automation

This project is an R package named `financialreports` with a Shiny golem app and a separate script entry point. Both call `build_reports()`, so calculations, QC files, project workbooks, receipt packets, and Smartsheet summaries follow the same workflow.

## Install and run locally

Open `FinancialReports.Rproj` in RStudio. The project activates `renv` automatically. On a fresh checkout, allow the bootstrap to install renv, then restore the locked packages:

```r
renv::restore()
```

The committed `renv.lock` records R and package versions, including image receipt support, deployment tools, and tests. Packages are installed in the ignored `renv/library/` folder. ImageMagick system libraries and image/PDF delegates may also be required by your OS.

Launch from the project root:

```r
source("dev/run_dev.R")
```

Both the development launcher and standalone script activate the project environment. Restart your current R session once after this migration so it uses renv and clears functions from the earlier sourced app. Workspace restoration and saving are disabled for this project to prevent old global functions from masking package functions.

Upload a GL and optional mileage workbook, choose projects, confirm mileage worksheet matches, set reporting dates and approved rates, upload receipt files, and build reports. The app provides a full ZIP bundle and a separate `SmartsheetInput.xlsx` download. Receipt filenames must retain their dollar amount; a date helps disambiguate matches. You can select multiple receipts, including files from different project subfolders. Folder-based project hints remain available in the script workflow.

**Download the bundle before leaving the page.** Each session and run has separate temporary storage. Session files are removed on disconnect. The ZIP includes project deliverables plus an `audit` folder containing input snapshots, settings, internal QC outputs, delivery status, and the run log. Paths recorded inside the audit files describe the original run environment; use the copied files in the archive for review.

## Manage dependencies

Dependencies are declared in `DESCRIPTION`. The explicit snapshot includes this project's Imports and Suggests, plus their runtime dependencies; it excludes private financial inputs and report outputs. After intentionally changing dependencies or package versions:

```r
renv::install("package@version")
# Update DESCRIPTION if adding a dependency, then:
renv::snapshot()
renv::status()
```

Commit `renv.lock`, `.Rprofile`, `renv/activate.R`, and `renv/settings.json` with the code. Use `renv::restore()` to return to the recorded versions. The lockfile records the R version; install a compatible R version separately. [Posit Connect uses the lockfile to capture dependency versions](https://docs.posit.co/connect/how-to/use-renv-for-reproducible-r-deployments/).

## Run the standalone script

Copy `config/report_config.example.R` to `config/report_config.R` and edit the input paths, reporting dates, project names, worksheet mapping, and rates. Configuration files are trusted local R expressions returning a named list. From the project root:

```sh
Rscript RunFinancialReport.R config/report_config.R
```

The script runs the complete workflow and writes persistent input snapshots, QC evidence, project reports, receipt packets, and the Smartsheet workbook. Relative input paths resolve from the calling working directory. A new report name/root combination is required for every run to preserve earlier snapshots. Leave `project_output_root = ""` to retain deliverables inside that report's output folder. Setting an explicit project output folder writes project folders directly there and can replace previously generated project files at that destination.

You can also install this package and call `financialreports::build_reports(...)` directly, or use it after `pkgload::load_all()`. The lower-level `prepare_quarter()` and `run_quarterly_pipeline()` functions remain available.

## Deploy to Posit Connect

The root `app.R` loads the source package and runs the golem app, following the [golem source-package deployment pattern](https://thinkr-open.github.io/golem/articles/c-deploy.html). Connect needs R >= 4.1 and access to a package repository containing the declared dependencies. ZIP creation and PDF merging use the R packages `zip` and `qpdf`, without requiring PowerShell or command-line executables.

1. Configure your Connect account/server through RStudio's Publishing settings or `rsconnect::addServer()` and `rsconnect::connectUser()`.
2. From the project root, run:

```r
source("dev/deploy.R")
deploy_financialreports(account = "YOUR_ACCOUNT", server = "connect.example.org")
```

This helper uses an explicit file allowlist: `app.R`, package metadata, `renv.lock`, `R/`, and `inst/`. Connect uses the lockfile when preparing the deployment dependencies; the local renv library and activation files are excluded from the deployment bundle. Financial data, report outputs, examples, configuration files, credentials, and local libraries are excluded. The `.rscignore` file also protects common local folders when publishing from the IDE. Review the deployment file list if publishing through the IDE instead of the helper.

The lockfile includes `magick` for image receipts. Ensure your Connect environment supports ImageMagick and its image/PDF delegates (including HEIC if used). Without image conversion support, affected PDF packets are marked as not created in the deliverables manifest; receipt QC and workbooks still run. All-PDF packets need only the required `qpdf` dependency. Packet creation never drops unsupported image pages silently.

File uploads have a 100 MB request limit; your Connect/proxy configuration may impose a lower limit. The app uses browser uploads and downloads, with no browsing of the Connect server's filesystem. Connect server provisioning, account registration, and deployment are left to your configured environment.

## Validation and operating notes

After restoring packages, run `testthat::test_local()` from the project root. Integration tests use the fictional files in [simulated_data](simulated_data/README.md) and cover control totals, receipt packets, duplicate upload names, download archives, the Shiny server workflow, and session cleanup. Fixture-based tests skip when those unbundled workbooks are absent.

Financial inputs and generated reports are intentionally excluded from the code repository. Payroll comes from the GL; expenses require receipt reconciliation. Missing sources remain visible in QC and are not converted to zero. Budget values stay blank without a dedicated budget source. Review QC before distributing reports or copying the Smartsheet workbook values by hand. The workbook has Expenses, Payroll, and Mileage rows for each project.

GL totals are filtered inclusively by **posted date** using the selected reporting dates. Transactions outside those dates are retained in `GL_Transactions_Outside_Period.csv`. Dated mileage logs are filtered by trip date; undated Project/Miles summaries must already cover the selected period. Missing, invalid, or negative mileage stays pending review. A receipt with a filename date must match the GL document date (or posted date when no document date is available).

Review each run in this order:

1. Confirm project scope in `Project_Scope.csv`.
2. Check `Input_Readiness.csv` and `Reconciliation_Checks.csv`.
3. Review `Payroll_GL_Summary.csv`, `Payroll_Detail.csv`, `Expense_Receipt_QC.csv`, and `Mileage_Summary.csv`.
4. Review `Financial_Report_Category_Draft.csv` and `Project_Deliverables_Manifest.csv` before using the project reports or Smartsheet summary.

Each project receives `Category_Spend_Summary.xlsx`, `Detailed_Expenses.xlsx`, and `Hourly_Time_Entries.xlsx`, plus `Receipt_Packet.pdf` when all matched receipts can be converted and combined. Missing or ambiguous receipt matches stay in QC files. Receipt packets follow receipt date, falling back to the GL document date.

## Project layout

- `app.R`: Connect launch file.
- `RunFinancialReport.R`: standalone script.
- `R/`: shared reporting functions and app code.
- `inst/app/www/`: app stylesheet and logo.
- `dev/`: local launch and deployment helpers.
- `config/`: example batch settings; personal settings are ignored by Git.
- `tests/` and `simulated_data/`: automated checks and fictional inputs.
- `man/`: R package help files.
- `renv.lock` and `renv/`: locked dependencies, activation, and local package storage.

Local financial inputs in `data/` and saved reports in `reports/` (or legacy `quarters/`) are outside the application code and excluded from deployment.
