\name{fit_sdt_glmm}
\alias{fit_sdt_glmm}
\title{Fit an SDT GLMM for Old/New Recognition}
\description{
Fits a multilevel GLMM (binomial link) to estimate SDT metrics from
old/new recognition responses, using random effects for participants and
optionally items.
}
\usage{
fit_sdt_glmm(data, is_old, said_old, subject_id, covariates = NULL,
  item_id = NULL, link = "probit", center_isold = TRUE,
  random_slope = TRUE, interactions = FALSE)
}
\arguments{
  \item{data}{Data frame containing trial-level responses.}
  \item{is_old}{Column name indicating whether the item is old (1) or new (0).}
  \item{said_old}{Column name indicating whether the participant said "old" (1) or "new" (0).}
  \item{subject_id}{Column name identifying participants.}
  \item{covariates}{Optional character vector of covariate column names.}
  \item{item_id}{Optional column name for item-level random intercepts.}
  \item{link}{Link function for the binomial model ("probit" or "logit").}
  \item{center_isold}{Logical. If TRUE, recodes is_old to -0.5 / +0.5.}
  \item{random_slope}{Logical. If TRUE, include random slope for is_old by subject.}
  \item{interactions}{Logical. If TRUE, include is_old interactions with covariates.}
}
\value{
A \code{glmerMod} object with SDT attributes.
}
\examples{
\dontrun{
df <- data.frame(subject_id = rep(1:10, each = 20),
                 is_old = rbinom(200,1,0.5),
                 said_old = rbinom(200,1,0.5))
fit <- fit_sdt_glmm(df, "is_old", "said_old", subject_id = "subject_id")
}
}
