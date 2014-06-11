\name{queryNetwork}
\alias{queryNetwork}
\title{Query network}
\usage{
  queryNetwork(net, nodesOfInterest = NA, evidence = NA)
}
\arguments{
  \item{net}{an existing network}

  \item{nodesOfInterest}{the nodes you want to know the
  probability of in the form <node, state>}

  \item{evidence}{a list of hard evidence to be set in the
  form <node, state>}
}
\description{
  The network can be queried to return the marginal
  probabilities of all (or some specific) nodes.
}
\details{
  Function runtime in core i5, 16GB of memory computer:
  0.04s [see the example below]
}
\examples{
node.a = createNodeInfo(~asia, prob=c(0.01, 0.99), states=c("yes","no"))
node.t = createNodeInfo(~tub|asia, prob=c(0.05, 0.95, 0.01, 0.99),states=c("yes","no"))
node.s = createNodeInfo(~smoke, prob=c(0.5,0.5), states=c("yes","no"))
node.l = createNodeInfo(~lung|smoke, prob=c(0.1, 0.9, 0.01, 0.99), states=c("yes","no"))
node.b = createNodeInfo(~bronc|smoke, prob=c(0.6, 0.4, 0.3, 0.7), states=c("yes","no"))
node.e = createNodeInfo(~either|lung:tub,prob=c(1,0,1,0,1,0,0,1),states=c("yes","no"))
node.x = createNodeInfo(~xray|either, prob=c(0.98, 0.02, 0.05, 0.95), states=c("yes","no"))
node.d = createNodeInfo(~dysp|bronc:either, prob=c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9), states=c("yes", "no"))

nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
# returning a network that is compiled
net = createNetwork(nodes, compile=TRUE)

## Return all the marginal probabilities given asia = yes and tub = no
marginals = queryNetwork(net, evidence = list(c("asia", "yes"), c("tub", "no")))
marginals

## Return the marginal probabilities of nodes dysp and bronc
marginalsOfInterest = queryNetwork(net, c("dysp", "bronc"), list(c("asia", "yes"), c("tub", "no")))
marginalsOfInterest
}

