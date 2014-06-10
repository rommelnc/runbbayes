\name{removeNode}
\alias{removeNode}
\title{Remove node}
\usage{
removeNode(network, name)
}
\arguments{
  \item{network}{An existing network}

  \item{name}{Node's name}
}
\description{
Removes node of an existing network.
}
\examples{
nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
net = createNetwork(nodes, compile = T)
# Adding a node
net = addNode(net, ~asthma|smoker, prob = c(0.6, 0.4, 0.85, 0.15), states = c("yes", "no"))
# Removing a node
net = removeNode(net, "asthma")
}

