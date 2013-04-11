\name{forrCoRegPredictor}
\alias{forrCoRegPredictor}
\title{Predict Forrester Co-Kriging Model}
\usage{
  forrCoRegPredictor(x, fit, pred.all = FALSE)
}
\arguments{
  \item{x}{design matrix to be predicted}

  \item{fit}{fit of the Co-Kriging model (settings and
  parameters), as created by \code{forrCoBuilder}}

  \item{pred.all}{if TRUE return all (RMSE and prediction,
  in a dataframe), else return only prediction}
}
\value{
  Returned value is dependent on the setting of
  \code{pred.all}\cr TRUE: data.frame with columns f
  (function values) and s (RMSE)\cr FALSE: vector of
  function values only
}
\description{
  Predict new samples on a Forrester Co-Kriging model.
}
\references{
  FORRESTER, A.I.J, SOBESTER A. & KEAN, A.J. (2007),
  Multi-Fidelity optimization via surrogate modelling.
  \emph{Proc. R. Soc. A} 463, 3251-3269. \cr Forrester,
  Alexander I.J.; Sobester, Andras; Keane, Andy J. (2008).
  Engineering Design via Surrogate Modelling - A Practical
  Guide. John Wiley & Sons.
}
\seealso{
  \code{\link{forrCoBuilder}}
}

