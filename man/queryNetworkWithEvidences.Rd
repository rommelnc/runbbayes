\name{queryNetworkWithEvidences}
\alias{queryNetworkWithEvidences}
\title{Query network with evidences}
\usage{
queryNetworkWithEvidences(net, event, evidences)
}
\arguments{
  \item{net}{An existing network}

  \item{event}{The state you want to know}

  \item{evidences}{Set state to a node}
}
\description{
The network can return the posteriori probabilities of some
event given some evidences without modifying the current
network object
}
\details{
Function runtime in core i5, 4GB of memory computer: 97.98s
}
\examples{
nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
net = createNetwork(nodes, compile = T)
posterioriProb = queryNetworkWithEvidences(net, c("either", "yes"), list(c("asia", "yes"), c("smoker","no")))

Console:
  0.0645
}

