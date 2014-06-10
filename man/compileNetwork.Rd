\name{compileNetwork}
\alias{compileNetwork}
\title{Compile network}
\usage{
compileNetwork(network)
}
\arguments{
  \item{nework}{An already existing netowrk}
}
\description{
Compiles the network
}
\details{
Function runtime in core i5, 4GB of memory computer: 9.82s
}
\examples{
netCompiled = compileNetwork(net)

Console:
  Compiled: TRUE
  P ( asia )
  P ( tub | asia )
  P ( smoke )
  P ( lung | smoke )
  P ( bronc | smoke )
  P ( either | lung tub )
  P ( xray | either )
  P ( dysp | bronc either )
}

