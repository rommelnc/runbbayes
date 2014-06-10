\name{queryNetwork}
\alias{queryNetwork}
\title{Query network}
\usage{
queryNetwork(net, event = c(), evidences = list())
}
\arguments{
  \item{net}{An existing network}

  \item{event}{The state you want to know}

  \item{evidences}{Set state to a node}
}
\description{
The network can be queried to return the priori
probabilities of all (or some specific) nodes.
}
\details{
Function runtime in core i5, 4GB of memory computer:
309.41s
}
\examples{
query = queryNetwork(netCompiled, c(), list(c("asia", "yes"), c("tub", "no")))

Console:
  $asia
  yes		no
  1		0
  
  $tub
  yes		no
  0		1
  
  $smoke
  yes		no
  0.5		0.5
  
  $lung
  yes		no
  0.055		0.945
  
  $bronc
  yes		no
  0.45		0.55
  
  $either
  yes		no
  0.055		0.945
  
  $xray
  yes		no
  0.1012		0.8989
  
  $dysp
  yes		no
  0.4323		0.5678
}

