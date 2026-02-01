\name{fit_sdt_glm}
\alias{fit_sdt_glm}
\title{Fit an SDT GLM for Old/New Recognition}
\description{
Fits a single-level GLM (binomial link) to estimate SDT metrics from
old/new recognition responses. With centered item coding, the coefficient
on item status corresponds to d' (probit) or ln(OR) (logit).
}
\usage{
fit_sdt_glm(data, is_old, said_old, covariates = NULL, link = "probit",
  center_isold = TRUE, interactions = FALSE)
}
\arguments{
  \item{data}{Data frame containing trial-level responses.}
  \item{is_old}{Column name indicating whether the item is old (1) or new (0).}
  \item{said_old}{Column name indicating whether the participant said "old" (1) or "new" (0).}
  \item{covariates}{Optional character vector of covariate column names.}
  \item{link}{Link function for the binomial model ("probit" or "logit").}
  \item{center_isold}{Logical. If TRUE, recodes is_old to -0.5 / +0.5.}
  \item{interactions}{Logical. If TRUE, include is_old interactions with covariates.}
}
\value{
A \code{glm} object with SDT attributes.
}
\examples{
\dontrun{
df <- data.frame(is_old = rbinom(200,1,0.5),
                 said_old = rbinom(200,1,0.5))
fit <- fit_sdt_glm(df, "is_old", "said_old", link = "probit")
}
}
