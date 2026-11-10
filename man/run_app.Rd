\name{run_app}
\alias{run_app}
\title{Run the financial reporting application}
\usage{run_app(onStart = NULL, options = list(), enableBookmarking = NULL, ...)}
\arguments{
\item{onStart}{Optional application startup callback.}
\item{options}{Options passed to shinyApp.}
\item{enableBookmarking}{Bookmarking configuration passed to shinyApp.}
\item{...}{Additional golem options.}}
\description{Runs the packaged Shiny golem application with browser uploads and downloadable reports.}
\value{A Shiny application object.}
