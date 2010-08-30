\name{spotMetaFlattenFbsRow}
\alias{spotMetaFlattenFbsRow}
\title{Spot Meta Flatten Fbs Row...}
\usage{spotMetaFlattenFbsRow(fbsRow)}
\description{Spot Meta Flatten Fbs Row}
\details{Help function that replaces vector-valued columns in a final best solution
file row with N "flat" values, where N is the dimension of the vector replaced.
Also, each column will be converted to a character string.}
\value{list \cr
- The FBS line, with vector-valued columns replaced with scalar columns.}
\arguments{\item{fbsRow}{The FBS row to flatten.}}
