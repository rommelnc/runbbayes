\name{addNode}
\alias{addNode}
\title{Add node}
\usage{
addNode(network, nodes, prob, states)
}
\arguments{
  \item{network}{An existing network}

  \item{nodes}{node with its parents}

  \item{prob}{list of node's probabilities}

  \item{states}{list of node's states}
}
\description{
Add node to an existing network.
}
\details{
Function runtime in core i5, 4GB of memory computer: 7.49s
}
\examples{
nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
net = createNetwork(nodes, compile = T)
# Adding a node
net = addNode(net, ~asthma|smoker, prob = c(0.6, 0.4, 0.85, 0.15), states = c("yes", "no"))
}

