\name{resetEvidences}
\alias{resetEvidences}
\title{Reset the evidences}
\usage{
resetEvidences(network)
}
\arguments{
  \item{network}{An existing network}
}
\description{
Resets the evidence through the network, changing to the
original values.
}
\details{
Function runtime in core i5, 4GB of memory computer: 21.98s
}
\examples{
nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
net = createNetwork(nodes, compile = T)
net = setEvidence(net, list(c("asia", "yes"), c("smoker", "no")))
net = propagateEvidences(net)
posterioriProb = queryNetwork(net, c("dysp", "yes"))
posterioriProb

# Reseting the evidences
net = resetEvidences(net)
prioriProb = queryNetwork(net, c("dysp", "yes"))
prioriProb
}

