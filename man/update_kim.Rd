% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_kim.R
\name{update_kim}
\alias{update_kim}
\title{Update the package 'kim'}
\usage{
update_kim(force = TRUE, upgrade_other_pkg = FALSE, confirm = TRUE)
}
\arguments{
\item{force}{logical. If TRUE, force installing the update.
By default, TRUE.}

\item{upgrade_other_pkg}{passed to \code{remotes::install_github}.
By default, FALSE.}

\item{confirm}{logical. If TRUE, ask the user before updating.
By default, TRUE.}
}
\value{
Invisibly returns NULL. Called for side effects.
}
\description{
(Rewritten by ChatGPT on May 27, 2025.
May not function properly because of the rewrite.)
}
\details{
Updates the current package 'kim' by installing the
most recent version of the package from GitHub.
This function requires the 'remotes' package.
}
