\name{spotCalcNoise}
\alias{spotCalcNoise}
\title{Calculate Noise}
\usage{
  spotCalcNoise(y, noise = 0, noise.type = "weighted",
    spot.noise.minimum.at.value = 0)
}
\arguments{
  \item{y}{the function value where noise will be added to}

  \item{noise}{noise magnitude (absolute or weighted)}

  \item{noise.type}{type of noise can either be "weighted"
  or "constant"}

  \item{spot.noise.minimum.at.value}{Voice magnitude at
  minimum}
}
\value{
  numeric \cr holding the noise Value
}
\description{
  Calculates Noise for a given Function-Value (y) dependant
  on the noise intensity (noise) and the way of calculating
  noise (noise.type)
}

