\name{isParentsDefined}
\alias{isParentsDefined}
\title{Parents defined}
\usage{
  isParentsDefined(netJava, node)
}
\arguments{
  \item{netJava}{an existing network}

  \item{node}{a list with the node's information returned
  by createNodeInfo function}
}
\value{
  TRUE if all parents have been define, FALSE otherwise
}
\description{
  Verifies if the node's parents exist.
}

