\name{createNode}
\alias{createNode}
\title{Create a probabilistic node}
\usage{
createNode(node)
}
\arguments{
  \item{node}{a list returned by createNodeInfo function}
}
\description{
Creates a probabilistic node
}
\details{
Function runtime in core i5, 4GB of memory computer: 31.27s
}
\examples{
node = createNodeInfo(~asia, prob=c(0.01, 0.99), states=c("yes","no"))
probNode = createNode(node) 

Console:
  [1] "Java-Object{ (asia)}"
}

