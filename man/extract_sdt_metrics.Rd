\name{extract_sdt_metrics}
\alias{extract_sdt_metrics}
\title{Extract SDT Metrics from a GLM/GLMM}
\description{
Extracts d' (probit) or ln(OR) (logit) from SDT GLM/GLMM fits.
Criterion is reported only when is_old was centered to -0.5/+0.5.
}
\usage{
extract_sdt_metrics(model, link = NULL)
}
\arguments{
  \item{model}{A glm or glmerMod object produced by fit_sdt_glm/fit_sdt_glmm.}
  \item{link}{Optional. If NULL, inferred from the model family.}
}
\value{
A list containing d' or ln(OR), criterion (if available), and metadata.
}
\examples{
\dontrun{
df <- data.frame(is_old = rbinom(200,1,0.5),
                 said_old = rbinom(200,1,0.5))
fit <- fit_sdt_glm(df, "is_old", "said_old", link = "probit")
extract_sdt_metrics(fit)
}
}
