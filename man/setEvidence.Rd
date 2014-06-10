\name{setEvidence}
\alias{setEvidence}
\title{Set evidence}
\usage{
setEvidence(network, evidences, propagate = FALSE)
}
\arguments{
  \item{network}{An existing network}

  \item{evidences}{Set state to a node}

  \item{propagate}{if set to TRUE, propagates the evidence
  through the network}
}
\description{
Set and reset evidences in the network
}
\examples{
nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
net = createNetwork(nodes, compile = T)
net = setEvidence(net, list(c("asia", "yes"), c("smoker", "no")))
net = propagateEvidences(net)
posterioriProb = queryNetwork(net, c("dysp", "yes"))
posterioriProb
}

