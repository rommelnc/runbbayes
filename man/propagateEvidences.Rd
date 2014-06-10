\name{propagateEvidences}
\alias{propagateEvidences}
\title{Propagate the evidences}
\usage{
propagateEvidences(network)
}
\arguments{
  \item{network}{An existing network}
}
\description{
Propagates the evidence through the network.
}
\examples{
nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
net = createNetwork(nodes, compile = T)
net = setEvidence(net, list(c("asia", "yes"), c("smoker", "no")))
net = propagateEvidences(net)
posterioriProb = queryNetwork(net, c("dysp", "yes"))
posterioriProb
}

