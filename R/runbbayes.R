#' Query network
#' 
#' 
#' The network can be queried to return the marginal probabilities of all (or some specific) nodes.
#' 
#' Function runtime in core i5, 16GB of memory computer: 0.04s [see the example below]
#' 
#' @param net an existing network
#' @param nodesOfInterest the nodes you want to know the probability of in the form <node, state>
#' @param evidence a list of hard evidence to be set in the form <node, state>
#' 
#' @example examples/queryNetworkExample.R
queryNetwork <- function(net, nodesOfInterest = NA, evidence = NA) {
  if (!is.na(nodesOfInterest) && class(nodesOfInterest) != "character") {
    stop("Parameter 'nodesOfInterest' must be a tuple of strings: c(\"node1\", \"node2\")")
  }
  
  if (!is.na(evidence) && (class(evidence) != "list" | class(evidence[[1]]) != "character")) {
    stop("Parameter 'evidence' must be a list of tuples: list(c(\"node1\", \"state1\"), c(\"node2\", \"state2\"))")
  }
  
  ## If there is evidence to set
  if (!is.na(evidence)) {
    ## reset any previous evidence
    net = resetEvidence(net)
    
    ## set evidence
    net = setEvidence(net, evidence)
  }
  
  ## propagate evidence
  net = propagateEvidence(net)
  
  netJava = net$network
  
  ## Return marginal probabilities
  marginals = list()
  for (index in 1:.jcall(netJava,"I","getNodeCount")-1L) {
    #     node = netJava$getNodeAt(index)
    node = .jcall(netJava,"Lunbbayes/prs/Node;","getNodeAt",as.integer(index))
    nodeName = .jcall(node,"Ljava/lang/String;","getName")
    ## If we have to return just the nodes of interest and the current node is not of interest, then continue to next node
    if (!is.na(nodesOfInterest) && !(nodeName %in% nodesOfInterest)) {
      next
    }
    statesProb = list()
    for (i in 1:.jcall(node,"I","getStatesSize")-1L) {
      #       statesProb[[node$getStateAt(i)]] = round(.jcast(node, "unbbayes/prs/bn/ProbabilisticNode")$getMarginalAt(i), 4)
      statesProb[[.jcall(node,"Ljava/lang/String;","getStateAt",as.integer(i))]] = round(.jcall(.jcast(node, "unbbayes/prs/bn/ProbabilisticNode"),"F","getMarginalAt",as.integer(i)), 4)
    }
    class(statesProb) = "query"
    marginals[[nodeName]] = statesProb
  }
  class(marginals) = "query"
  return(marginals)
}

#' prints the result of a query in runbbayes.
#' 
#' @param x the result of a query in runbbayes to be printed.
#' @param ... further arguments passed to or from other methods.
#' 
print.query <- function(x, ...) {
  for (i in 1:length(x)) {
    cat(sprintf("$%s\n", attributes(x[i])$names))
    
    for (j in 1:length(x[[i]])) {
      cat(sprintf("\t\t%s", attributes(x[[i]][j])$names))
    }
    cat("\n")
    for (j in 1:length(x[[i]])) {
      cat(sprintf("\t\t%s", x[[i]][j]))
    }
    cat("\n\n")
  }
}

#' Create node info
#' 
#' 
#' Create node's information (name, parents, probabilities and states).
#' 
#' Function runtime in core i5, 16GB of memory computer: 0.04s [see the example below]
#' 
#' @param nodes node with its parents
#' @param prob list of node's probabilities 
#' @param states list of node's states
#' 
#' @example examples/createNodeInfoExample.R
createNodeInfo <- function(nodes, prob, states) {
  formula = as.character(nodes)
  node = (strsplit(formula[2], " | "))[[1]][1]
  parents = (strsplit(formula[2], " | "))[[1]][3]
  parents = strsplit(parents, "[:]")[[1]]
  
  table = list(node = node, parents=parents, prob=prob, states=states)
  class(table) = "nodeinfo"
  return(table)
}

#' prints the node information in runbbayes.
#' 
#' @param x the node information in runbbayes to be printed.
#' @param ... further arguments passed to or from other methods.
#' 
print.nodeinfo <- function(x, ...) {
  cat(sprintf("Node:           %s\n", x$node))
  cat(sprintf("Parents:        %s\n", paste(x$parents, collapse=", ")))
  cat(sprintf("Probabilities:  %s\n", paste(x$prob, collapse=", ")))
  cat(sprintf("States:         %s\n", paste(x$states, collapse=", ")))
}

#' prints the network information in runbbayes.
#' 
#' @param x the network information in runbbayes to be printed.
#' @param ... further arguments passed to or from other methods.
#' 
print.network <- function(x, ...) {
  cat(sprintf("Compiled: %s\n", x$compiled))
  netJava = x$network
#   for (i in 1:net$getNodeCount()-1L) {
  for (i in 1:.jcall(netJava,"I","getNodeCount")-1L) {
#     nodeName = netJava$getNodeAt(i)$getName()
    nodeName = .jcall(.jcall(netJava,"Lunbbayes/prs/Node;","getNodeAt",as.integer(i)),"Ljava/lang/String;","getName")
    cat(sprintf("P ( %s ",nodeName))
    if (length(x[[nodeName]]$parents) > 0 && !is.na(x[[nodeName]]$parents)) {
      cat(sprintf("| "))
      for (j in 1: length(x[[nodeName]]$parents)) {
        cat(sprintf("%s ", x[[nodeName]]$parents[j]))
      }
    }
    cat(sprintf(")\n"))
  }
}

#' Compile network
#' 
#' 
#' Compiles the network
#' 
#' Function runtime in core i5, 16GB of memory computer: 0.02s [see the example below]
#' 
#' @param network An already existing netowrk
#' 
#' @example examples/compileNetworkExample.R
compileNetwork <- function(network) {
#   attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
#   attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  network$compiled = TRUE
  net = network$network
#   algorithm = new(JunctionTreeAlgorithm)
  algorithm = .jnew("unbbayes/prs/bn/JunctionTreeAlgorithm")
#   algorithm$setNetwork(net)
  .jcall(algorithm,"V","setNetwork", .jcast(net, "unbbayes/prs/Graph"))
#   algorithm$run()
  .jcall(algorithm,"V","run")
  network$network = net
  return(network)
}

#' Set evidence
#' 
#' 
#' Set and reset evidence in the network
#' 
#' 
#' 
#' @param network An existing network
#' @param evidence Set state to a node
#' @param propagate if set to TRUE, propagates the evidence through the network
#' 
#' @example examples/setEvidenceExample.R
setEvidence <- function(network, evidence, propagate = FALSE) {
  if (!network$compiled) {
    network = compileNetwork(network)
  }
  
  netJava = network$network
  
  if (class(evidence) == "character" && length(evidence) == 2) {
    evidence = list(evidence)
  }
  
  if (length(evidence) > 0) {
    for (evidenceIndex in 1:length(evidence)) {
      ## find the node of this evidence
#       findingNode = .jcast(netJava$getNode(evidence[[evidenceIndex]][1]), "unbbayes/prs/bn/ProbabilisticNode")
      tryCatch((findingNode = .jcast(.jcall(netJava,"Lunbbayes/prs/Node;","getNode",evidence[[evidenceIndex]][1]), "unbbayes/prs/bn/ProbabilisticNode")),
               error=function(x) paste("Node ", evidence[[evidenceIndex]][1]," not found in Node." , sep = ""))  
      ##iterate over the states in findingNode and try to find the informed state
      foundState = FALSE
#       for (stateIndex in 1:findingNode$getStatesSize()-1L) {
      for (stateIndex in 1:.jcall(findingNode,"I","getStatesSize")-1L) {
#         if (findingNode$getStateAt(stateIndex) == evidence[[evidenceIndex]][2]) {
        if (.jcall(findingNode,"Ljava/lang/String;","getStateAt",as.integer(stateIndex)) == evidence[[evidenceIndex]][2]) {
          foundState = TRUE
#           findingNode$addFinding(stateIndex)
          .jcall(findingNode,"V","addFinding",as.integer(stateIndex))
          break
        }
      }
      #if no state was found for the findingNode throw an error
      if (foundState == FALSE) {
        stop(paste("State '", evidence[[evidenceIndex]][2],"' not found in Node '", evidence[[evidenceIndex]][1], "'.", sep = ""))
      }
      
    }
  }
  
#   network$network = netJava
  
  if (propagate == TRUE) {
    network = propagateEvidence(network)
  }
  
  return(network)
}

#' Propagate the evidence
#' 
#' 
#' Propagates the evidence through the network.
#' 
#' 
#' 
#' @param network An existing network
#' 
#' @example examples/setEvidenceExample.R

propagateEvidence <- function(network) {
#   net$updateEvidences()
  if (!network$compiled) {
    network = compileNetwork(network)
  }
  .jcall(network$network,"V","updateEvidences")
  return(network)
}

#' Reset the evidence
#' 
#' 
#' Resets the evidence through the network, changing to the original values.
#' 
#' Function runtime in core i5, 16GB of memory computer: 0.02s [see the example below]
#' 
#' @param network An existing network
#' 
#' @example examples/resetEvidenceExample.R
resetEvidence <- function(network) {
#   attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
#   attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  ## reset any previous evidence
#   netJava$resetEvidences()
  .jcall(network$network,"V","resetEvidences")
  
  ## recompile network to force evidence to be reset
  network = compileNetwork(network)
  
  return(network)
}

#' Add node
#' 
#' 
#' Add node to an existing network.
#' 
#' Function runtime in core i5, 16GB of memory computer: 0.04s [see the example below]
#' 
#' @param network An existing network
#' @param node node with its parents
#' @param prob list of node's probabilities 
#' @param states list of node's states
#' 
#' @example examples/addNodeExample.R
addNode <- function(network, node, prob, states) {
#   attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
#   attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  nodeInfo = createNodeInfo(node, prob, states)
  ## Add node info to R's network structure
  network[[nodeInfo$node]] = nodeInfo

  nodeJava = createNode(nodeInfo)
  
  netJava = network$network
  
  ## Add node to Java's network structure
#   netJava$addNode(nodeJava)
  .jcall(netJava,"V","addNode",.jcast(nodeJava,"unbbayes/prs/Node"))
  for (i in 1:length(nodeInfo$parents)) {
#     parentJava = .jcast(netJava$getNode(nodeInfo$parents[i]), "unbbayes/prs/bn/ProbabilisticNode")
    parentJava = .jcast(.jcall(netJava,"Lunbbayes/prs/Node;","getNode",nodeInfo$parents[i]), "unbbayes/prs/bn/ProbabilisticNode")
#     net$addEdge(new(Edge, parentJava, nodeJava))
    edgeJava = .jnew("unbbayes/prs/Edge", .jcast(parentJava, "unbbayes/prs/Node"), .jcast(nodeJava, "unbbayes/prs/Node"))
    .jcall(netJava,"V","addEdge", edgeJava)
  }
  
  network = compileNetwork(network)
  
  return(network)
}

#' Remove node
#' 
#' 
#' Removes node of an existing network.
#' 
#' 
#' 
#' @param network An existing network
#' @param name Node's name
#' 
#' @example examples/removeNodeExample.R
removeNode <- function(network, name) {
#   attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
#   attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  ## Remove node info from R's network structure
  network[[name]] = NA
  
  netJava = network$network
  
#   nodeJava = netJava$getNode(name)
  nodeJava = .jcall(netJava,"Lunbbayes/prs/Node;","getNode", name)
  
  ## Remove node info from Java's network structure
#   netJava$removeNode(nodeJava)
  .jcall(netJava,"V","removeNode",nodeJava)
  
  network = compileNetwork(network)
  
  return(network)
}

#' Create a probabilistic node
#' 
#' 
#' Creates a probabilistic node
#' 
#' Function runtime in core i5, 16GB of memory computer: 0.0s [see the example below]
#' 
#' @param node a list returned by createNodeInfo function
#' 
#' @example examples/createNodeExample.R
createNode <- function(node) {
#   attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
#   attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
#   newNode = new(ProbabilisticNode)
  newNode = .jnew("unbbayes/prs/bn/ProbabilisticNode")
#   newNode$setName(node$node)
  .jcall(newNode,"V","setName", node$node)
  
  for(i in 1:length(node$states)){
#     newNode$appendState(node$states[i])
    .jcall(newNode,"V","appendState", node$states[i])
  }
  
#   auxCPT = newNode$getProbabilityFunction()
  auxCPT = .jcall(newNode,"Lunbbayes/prs/bn/IProbabilityFunction;","getProbabilityFunction")
#   auxCPT$addVariable(newNode)
  .jcall(auxCPT,"V","addVariable", .jcast(newNode, "unbbayes/prs/INode"))
  
  for (i in 1:length(node$prob)) {
    ## filling CPT of new node
#     auxCPT$addValueAt(as.integer((i-1)), .jfloat(node$prob[i]))
    .jcall(auxCPT,"V","addValueAt", as.integer((i-1)), .jfloat(node$prob[i]))
  }
  return(newNode)
}

#' Parents defined
#' 
#' 
#' Verifies if the node's parents exist.
#' 
#' 
#' 
#' @param netJava an existing network
#' @param node a list with the node's information returned by createNodeInfo function
#' @return TRUE if all parents have been define, FALSE otherwise
isParentsDefined <- function(netJava, node) {
  parents = node$parents
  for(j in 1:length(parents)) {
    parentName = parents[j]
#     if (length(netJava$getNode(parentName)) == 0) {
    if (length(.jcall(netJava,"Lunbbayes/prs/Node;","getNode", parentName)) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Create network
#' 
#' 
#' Receives a node list and creates a probabilistic network.
#' 
#' Function runtime in core i5, 16GB of memory computer: 0.08s [see the example below]
#' 
#' @param nodeList a list of each node returned by createNodeInfo
#' @param compile if set to TRUE, returns a compiled network
#' @return the created network with a boolean compile variable which defines if the network has been compiles, the node information for every node, and the corresponding network representation in UnBBayes (Java version)
#' 
#' @example examples/createNetworkExample.R
createNetwork <- function(nodeList, compile=FALSE) {
  
#   attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
#   attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  nodeListAux = nodeList
  
  #Create a probabilistic network
#   netJava = new(ProbabilisticNetwork, "Net")
  netJava = .jnew("unbbayes/prs/bn/ProbabilisticNetwork", "Net")
  
  cont = 0
  
  while((length(nodeList) > 0) && (cont <= length(nodeList))) {
    node = nodeList[[1]]
    
    if(!is.na(nodeList[[1]]$parents) && !isParentsDefined(netJava, node)) {
      n = nodeList[[1]]
      nodeList = nodeList[-1]
      nodeList[[length(nodeList) + 1]] = n
      cont = cont + 1
    } else {
      cont = 0
      nodeJava = createNode(node)
      if(!is.na(nodeList[[1]]$parents) && isParentsDefined(netJava, node)) {
        parents = node$parents
        for(j in 1:length(parents)) {
          parentName = parents[j]
#           parentJava = netJava$getNode(parentName)
          parentJava = .jcall(netJava,"Lunbbayes/prs/Node;","getNode", parentName)
#           netJava$addEdge(new(Edge, parentJava, nodeJava))
          edgeJava = .jnew("unbbayes/prs/Edge", .jcast(parentJava, "unbbayes/prs/Node"), .jcast(nodeJava, "unbbayes/prs/Node"))
          .jcall(netJava,"V","addEdge", edgeJava)
        }  
      }
#       netJava$addNode(nodeJava)
      .jcall(netJava,"V","addNode", .jcast(nodeJava, "unbbayes/prs/Node"))
      nodeList = nodeList[-1]
    }
    
  }
  if (cont > 0) {
    print("Node does not exist!")
    print(cont)
  } else {
    result = list(network = netJava)
    class(result)="network"
    result$compiled = FALSE
    for  (i in 1:length(nodeListAux)) {
      result[[nodeListAux[[i]]$node]] = nodeListAux[[i]]
    }
    if (compile) {
      result = compileNetwork(result)
    }
    return(result)
  }
}

#' Check if the main functions of runbbyes are running correctly.
#' 
#' @return TRUE if everything runs smoothly, FALSE otherwise.
checkRUnBBayes <- function() {
  node.a = createNodeInfo(~asia, prob=c(0.01, 0.99), states=c("yes","no"))
  node.t = createNodeInfo(~tub|asia, prob=c(0.05, 0.95, 0.01, 0.99),states=c("yes","no"))
  node.s = createNodeInfo(~smoke, prob=c(0.5,0.5), states=c("yes","no"))
  node.l = createNodeInfo(~lung|smoke, prob=c(0.1, 0.9, 0.01, 0.99), states=c("yes","no"))
  node.b = createNodeInfo(~bronc|smoke, prob=c(0.6, 0.4, 0.3, 0.7), states=c("yes","no"))
  node.e = createNodeInfo(~either|lung:tub,prob=c(1,0,1,0,1,0,0,1),states=c("yes","no"))
  node.x = createNodeInfo(~xray|either, prob=c(0.98, 0.02, 0.05, 0.95), states=c("yes","no"))
  node.d = createNodeInfo(~dysp|bronc:either, prob=c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9), states=c("yes", "no"))
  
  nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
  
  net = createNetwork(nodes)
  
  net = compileNetwork(net)
  
  #checks if the net was correctly created
  if(net$asia[3]$prob[1] == 0.01) {} else {stop(return(FALSE))}
  if(net$asia[3]$prob[2] == 0.99) {} else {stop(return(FALSE))}
  if(net$tub[3]$prob[1] == 0.05) {} else {stop(return(FALSE))}
  if(net$tub[3]$prob[2] == 0.95) {} else {stop(return(FALSE))}
  if(net$tub[3]$prob[3] == 0.01) {} else {stop(return(FALSE))}
  if(net$tub[3]$prob[4] == 0.99) {} else {stop(return(FALSE))}
  if(net$smoke[3]$prob[1] == 0.5) {} else {stop(return(FALSE))}
  if(net$smoke[3]$prob[2] == 0.5) {} else {stop(return(FALSE))}
  if(net$lung[3]$prob[1] == 0.1) {} else {stop(return(FALSE))}
  if(net$lung[3]$prob[2] == 0.9) {} else {stop(return(FALSE))}
  if(net$lung[3]$prob[3] == 0.01) {} else {stop(return(FALSE))}
  if(net$lung[3]$prob[4] == 0.99) {} else {stop(return(FALSE))}
  if(net$bronc[3]$prob[1] == 0.6) {} else {stop(return(FALSE))}
  if(net$bronc[3]$prob[2] == 0.4) {} else {stop(return(FALSE))}
  if(net$bronc[3]$prob[3] == 0.3) {} else {stop(return(FALSE))}
  if(net$bronc[3]$prob[4] == 0.7) {} else {stop(return(FALSE))}
  if(net$either[3]$prob[1] == 1) {} else {stop(return(FALSE))}
  if(net$either[3]$prob[2] == 0) {} else {stop(return(FALSE))}
  if(net$either[3]$prob[3] == 1) {} else {stop(return(FALSE))}
  if(net$either[3]$prob[4] == 0) {} else {stop(return(FALSE))}
  if(net$either[3]$prob[5] == 1) {} else {stop(return(FALSE))}
  if(net$either[3]$prob[6] == 0) {} else {stop(return(FALSE))}
  if(net$either[3]$prob[7] == 0) {} else {stop(return(FALSE))}
  if(net$either[3]$prob[8] == 1) {} else {stop(return(FALSE))}
  if(net$xray[3]$prob[1] == 0.98) {} else {stop(return(FALSE))}
  if(net$xray[3]$prob[2] == 0.02) {} else {stop(return(FALSE))}
  if(net$xray[3]$prob[3] == 0.05) {} else {stop(return(FALSE))}
  if(net$xray[3]$prob[4] == 0.95) {} else {stop(return(FALSE))}
  if(net$dysp[3]$prob[1] == 0.9) {} else {stop(return(FALSE))}
  if(net$dysp[3]$prob[2] == 0.1) {} else {stop(return(FALSE))}
  if(net$dysp[3]$prob[3] == 0.7) {} else {stop(return(FALSE))}
  if(net$dysp[3]$prob[4] == 0.3) {} else {stop(return(FALSE))}
  if(net$dysp[3]$prob[5] == 0.8) {} else {stop(return(FALSE))}
  if(net$dysp[3]$prob[6] == 0.2) {} else {stop(return(FALSE))}
  if(net$dysp[3]$prob[7] == 0.1) {} else {stop(return(FALSE))}
  if(net$dysp[3]$prob[8] == 0.9) {} else {stop(return(FALSE))}
  
  
  query = queryNetwork(net, c(), list(c("asia", "yes"), c("smoke", "no")))
  #checks if the net was correctly created
  if(query$asia$yes == 1) {} else {stop(return(FALSE))}
  if(query$asia$no == 0) {} else {stop(return(FALSE))}
  if(query$tub$yes == 0.05) {} else {stop(return(FALSE))}
  if(query$tub$no == 0.95) {} else {stop(return(FALSE))}
  if(query$smoke$yes == 0) {} else {stop(return(FALSE))}
  if(query$smoke$no == 1) {} else {stop(return(FALSE))}
  if(query$lung$yes == 0.01) {} else {stop(return(FALSE))}
  if(query$lung$no == 0.99) {} else {stop(return(FALSE))}
  if(query$bronc$yes == 0.3) {} else {stop(return(FALSE))}
  if(query$bronc$no == 0.7) {} else {stop(return(FALSE))}
  if(query$either$yes == 0.0595) {} else {stop(return(FALSE))}
  if(query$either$no == 0.9405) {} else {stop(return(FALSE))}
  if(query$xray$yes == 0.1053) {} else {stop(return(FALSE))}
  if(query$xray$no == 0.8947) {} else {stop(return(FALSE))}
  if(query$dysp$yes == 0.3368) {} else {stop(return(FALSE))}
  if(query$dysp$no == 0.6632) {} else {stop(return(FALSE))}

     
  rm(list=ls())
     
  return(TRUE)
}