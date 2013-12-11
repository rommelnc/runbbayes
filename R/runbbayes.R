helloUnBBayes <- function() {
  
  ## Load Network
  file = .jnew("java/io/File", "./asia.net")
  ## .net
  io = .jnew("unbbayes/io/NetIO")
  ## .xml
  #   io = .jnew("unbbayes/io/XMLBIFIO")
  graph = io$load(file) 
  
  net = .jcast(graph, "unbbayes/prs/bn/ProbabilisticNetwork")
  attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
  attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  
  
  
  ## adding a new node manually
  newNode = new(ProbabilisticNode)
  newNode$setName("K")
  newNode$setDescription("A test node")
  newNode$appendState("State 0")
  newNode$appendState("State 1")
  auxCPT = newNode$getProbabilityFunction()
  auxCPT$addVariable(newNode)
  net$addNode(newNode)
  
  ## search for "HasVisitedAsia"
  asiaNode = .jcast(net$getNode("A"), "unbbayes/prs/bn/ProbabilisticNode")
  
  ## adding a new edge from "HasVisitedAsia" to new node
  net$addEdge(new(Edge, asiaNode, newNode))
  
  ## filling CPT of new node
  auxCPT$addValueAt(0L, .jfloat(0.99))
  auxCPT$addValueAt(1L, .jfloat(0.01))
  auxCPT$addValueAt(2L, .jfloat(0.1))
  auxCPT$addValueAt(3L, .jfloat(0.9))
  
  ## prepare the algorithm to compile network
  algorithm = new(JunctionTreeAlgorithm)
  algorithm$setNetwork(net)
  algorithm$run()
  
  ## print node's initial states
  prior = c()
  for (index in 1:net$getNodeCount()-1L) {
    print(index)
    node = net$getNodeAt(index)
    value = paste(node$getDescription(), ": [ ", sep="", collapse="")
    for (i in 1:node$getStatesSize()-1L) {
      value = paste(value, node$getStateAt(i), " : ", 
                    round(.jcast(node, "unbbayes/prs/bn/ProbabilisticNode")$getMarginalAt(i), 2),
                    " ", sep="", collapse="")
    }
    value = paste(value, "]", sep="", collapse="")
    prior = c(prior, value)
  }
  prior
  
  ## insert evidence (finding) to the 1st node of "net"
  findingNode = .jcast(net$getNodes()$get(0L), "unbbayes/prs/bn/ProbabilisticNode")
  findingNode$addFinding(0L) ## the 1st state is now 100%
  
  ## insert likelihood
  likelihood = .jarray(.jfloat(1:newNode$getStatesSize()))
  likelihood[[1]] = .jfloat(1)
  likelihood[[2]] = .jfloat(0.8)
  newNode$addLikeliHood(likelihood)
  
  ## propagate evidence
  net$updateEvidences()
  
  ## print updated node's states
  posterior = c()
  for (index in 1:net$getNodeCount()-1L) {
    node = net$getNodeAt(index)
    value = paste(node$getDescription(), ": [ ", sep="", collapse="")
    for (i in 1:node$getStatesSize()-1L) {
      value = paste(value, node$getStateAt(i), " : ", 
                    round(.jcast(node, "unbbayes/prs/bn/ProbabilisticNode")$getMarginalAt(i), 2),
                    " ", sep="", collapse="")
    }
    value = paste(value, "]", sep="", collapse="")
    posterior = c(posterior, value)
  }
  posterior
  
  return(rbind(prior, posterior))
}


queryNetworkWithEvidences <- function(net, event, evidences) {
  #network: network object
  #event: array of two strings (tuple)
  #evidences: list of arrays of two strings (list of tuples)
  
  ## Query example: queryNetwork(net, c("T", "yes"), list(c("E", "no"), c("D", "yes")))
  ## Query example: queryNetwork(net, c("S", "no"), list())
  
  network = net$network
  
  if (class(event) != "character") {
    stop("Parameter 'event' must be a tuple of strings: c(\"A\", \"yes\")")
  }
  
  if (class(evidences) != "list") {
    stop("Parameter 'evidences' must be a list of tuples: list(c(\"E\", \"no\"), c(\"D\", \"yes\"))")
  }
  
  
  ## reset any previous evidences
  network$resetEvidences()
  
  ## recompile network to force evidences to be reset
  algorithm = new(JunctionTreeAlgorithm)
  algorithm$setNetwork(network)
  algorithm$run()
  
  
  
  ## insert evidence (finding) to each of the nodes of "network" in evidences list  
  
  ## iterate over each evidence (finding)
  if (length(evidences) > 0) {
    for (evidenceIndex in 1:length(evidences)) {
      ## find the node of this evidence
      findingNode = .jcast(network$getNode(evidences[[evidenceIndex]][1]), "unbbayes/prs/bn/ProbabilisticNode")
      
      ##iterate over the states in findingNode and try to find the informed state
      foundState = FALSE
      for (stateIndex in 1:findingNode$getStatesSize()-1L) {
        if (findingNode$getStateAt(stateIndex) == evidences[[evidenceIndex]][2]) {
          foundState = TRUE
          findingNode$addFinding(stateIndex)
        }
      }
      #if no state was found for the findingNode throw an error
      if (foundState == FALSE) {
        stop(paste("State '", evidences[[evidenceIndex]][2],"' not found in Node '", evidences[[evidenceIndex]][1], "'.", sep = ""))
      }
      
    }
  }
  
  
  
  ## propagate evidence
  network$updateEvidences()
  
  
  ## find the posterior probability for the informed eventNode 
  
  # initialize variable
  posteriorProb = 0.0
  # find the event node in the network
  eventNode = network$getNode(event[1])
  
  ##iterate over the states in eventNode and try to find the informed state
  foundState = FALSE
  for (stateIndex in 1:eventNode$getStatesSize()-1L) {
    if (eventNode$getStateAt(stateIndex) == event[2]) {
      foundState = TRUE
      posteriorProb = round(.jcast(eventNode, "unbbayes/prs/bn/ProbabilisticNode")$getMarginalAt(stateIndex), 4)
    }
  }
  
  #if no state was found for the evenNode throw an error
  if (foundState == FALSE) {
    stop(paste("State '",event[2],"' not found in Node '", event[1], "'.", sep=""))
  }
  
  class(posteriorProb) = "query"
  return(posteriorProb)
}


hasNode <- function(event, name) {
  for (i in 1:length(event)) {
    if (event[[i]][1] == name) {
      return(TRUE)
    }
  }
  return(FALSE)
}

queryNetwork <- function(net, event = c(), evidences = list()) {
  
  if (length(event) > 0 && class(event) == "character") {
    return(queryNetworkWithEvidences(net, event, evidences))
  }
  network = net$network
  marginals = list()
  for (index in 1:network$getNodeCount()-1L) {
    node = network$getNodeAt(index)
    statesProb = list()
    for (i in 1:node$getStatesSize()-1L) {
      statesProb[[node$getStateAt(i)]] = round(.jcast(node, "unbbayes/prs/bn/ProbabilisticNode")$getMarginalAt(i), 2)
    }
    class(statesProb) = "query"
    if (class(event) == "list") {
      #check if statesProb is included in event before adding it to marginals
      if (hasNode(event, node$getName())) {
        marginals[[node$getName()]] = statesProb
      }
    }
    else {
      marginals[[node$getName()]] = statesProb
    }
  }
  class(marginals) = "query"
  return(marginals)
}


print.query <- function(self) {
  for (i in 1:length(self)) {
    cat(sprintf("$%s\n", attributes(self[i])$names))
    
    for (j in 1:length(self[[i]])) {
      cat(sprintf("\t\t%s", attributes(self[[i]][j])$names))
    }
    cat("\n")
    for (j in 1:length(self[[i]])) {
      cat(sprintf("\t\t%s", self[[i]][j]))
    }
    cat("\n\n")
  }
}



createNodeInfo <- function(nodes, prob, states) {
  formula = as.character(nodes)
  node = (strsplit(formula[2], " | "))[[1]][1]
  parents = (strsplit(formula[2], " | "))[[1]][3]
  parents = strsplit(parents, "[:]")[[1]]
  
  table = list(node = node, parents=parents, prob=prob, states=states)
  class(table) = "nodeinfo"
  return(table)
}


print.nodeinfo <- function(info) {
  cat(sprintf("Node:           %s\n", info$node))
  cat(sprintf("Parents:        %s\n", paste(info$parents, collapse=", ")))
  cat(sprintf("Probabilities:  %s\n", paste(info$prob, collapse=", ")))
  cat(sprintf("States:         %s\n", paste(info$states, collapse=", ")))
}

print.network <- function(info) {
  cat(sprintf("Compiled: %s\n", info$compiled))
  net = info$network
  for (i in 1:net$getNodeCount()-1L) {
    nameNode = net$getNodeAt(i)$getName()
    cat(sprintf("P ( %s ",nameNode))
    if (length(info[[nameNode]]$parents) > 0 && !is.na(info[[nameNode]]$parents)) {
      cat(sprintf("| "))
      for (j in 1: length(info[[nameNode]]$parents)) {
        cat(sprintf("%s ", info[[nameNode]]$parents[j]))
      }
    }
    cat(sprintf(")\n"))
  }
}


compileNetwork <- function(network) {
  attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
  attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  network$compiled = TRUE
  net = network$network
  algorithm = new(JunctionTreeAlgorithm)
  algorithm$setNetwork(net)
  algorithm$run()
  network$network = net
  return(network)
}



setEvidence <- function(network, evidences, propagate = FALSE) {
  net = network$network
  
  if (class(evidences) == "character" && length(evidences) == 2) {
    evidences = list(evidences)
  }
  
  if (length(evidences) > 0) {
    for (evidenceIndex in 1:length(evidences)) {
      print(evidenceIndex)
      ## find the node of this evidence
      findingNode = .jcast(net$getNode(evidences[[evidenceIndex]][1]), "unbbayes/prs/bn/ProbabilisticNode")
      ##iterate over the states in findingNode and try to find the informed state
      foundState = FALSE
      for (stateIndex in 1:findingNode$getStatesSize()-1L) {
        if (findingNode$getStateAt(stateIndex) == evidences[[evidenceIndex]][2]) {
          foundState = TRUE
          findingNode$addFinding(stateIndex)
        }
      }
      #if no state was found for the findingNode throw an error
      if (foundState == FALSE) {
        stop(paste("State '", evidences[[evidenceIndex]][2],"' not found in Node '", evidences[[evidenceIndex]][1], "'.", sep = ""))
      }
      
    }
  }
  
  network$network = net
  
  if (propagate == TRUE) {
    network = propagateEvidences(network)
  }
  
  return(network)
}


propagateEvidences <- function(network) {
  net = network$network
  net$updateEvidences()
  network$network = net
  return(network)
}


resetEvidences <- function(network) {
  net = network$network
  attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
  attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  ## reset any previous evidences
  net$resetEvidences()
  
  ## recompile network to force evidences to be reset
  algorithm = new(JunctionTreeAlgorithm)
  algorithm$setNetwork(net)
  algorithm$run()
  network$network = net
  return(network)
}


addNode <- function(network, nodes, prob, states) {
  net = network$network
  attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
  attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  node = createNodeInfo(nodes, prob, states)
  newNode = new(ProbabilisticNode)
  newNode$setName(node$node)
  for (i in 1:length(node$states)) {
    newNode$appendState(node$states[i])
  }
  auxCPT = newNode$getProbabilityFunction()
  auxCPT$addVariable(newNode)
  net$addNode(newNode)
  for (i in 1:length(node$parents)) {
    parent = .jcast(net$getNode(node$parents[i]), "unbbayes/prs/bn/ProbabilisticNode")
    net$addEdge(new(Edge, parent, newNode))
    
  }
  
  for (i in 1:length(node$prob)) {
    ## filling CPT of new node
    auxCPT$addValueAt(as.integer((i-1)), .jfloat(node$prob[i]))
    
  }
  
  network$network = net
  net = compileNetwork(network)
  
  return (net)
  
}


removeNode <- function(network, name) {
  net = network$network
  attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
  attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  node = net$getNode(name)
  
  net$removeNode(node)
  
  network$network = net
  network = compileNetwork(network)
  
  return (network)
  
}

#Creates a probabilistic node
createNode <- function(node) {
  attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
  attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  newNode = new(ProbabilisticNode)
  newNode$setName(node$node)
  
  for(i in 1:length(node$states)){
    newNode$appendState(node$states[i])
  }
  
  auxCPT = newNode$getProbabilityFunction()
  auxCPT$addVariable(newNode)
  
  for (i in 1:length(node$prob)) {
    ## filling CPT of new node
    auxCPT$addValueAt(as.integer((i-1)), .jfloat(node$prob[i]))
  }
  return(newNode)
}

#Verifies if the node's parents exists
parentsDefined <- function(netJava, node) {
  parents = node$parents
  for(j in 1:length(parents)) {
    parentName = parents[j]
    if (length(netJava$getNode(parentName)) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}


#Receives a node list and creates a probabilistic network
createNetwork <- function(nodeList, compile=FALSE) {
  
  nodeListAux = nodeList
  
  attach( javaImport( "unbbayes.prs" ), pos = 2 , name = "java:unbbayes.prs.bn" )
  attach( javaImport( "unbbayes.prs.bn" ), pos = 3 , name = "java:unbbayes.prs.bn" )
  
  #Create a probabilistic network
  netJava = new(ProbabilisticNetwork, "Net")
  
  while(length(nodeList) > 0) {
    node = nodeList[[1]]
    
    if(!is.na(nodeList[[1]]$parents) && !parentsDefined(netJava, node)) {
      n = nodeList[[1]]
      nodeList = nodeList[-1]
      nodeList[[length(nodeList) + 1]] = n
    } else {
      nodeJava = createNode(node)
      if(!is.na(nodeList[[1]]$parents) && parentsDefined(netJava, node)) {
        parents = node$parents
        for(j in 1:length(parents)) {
          parentName = parents[j]
          parentJava = netJava$getNode(parentName)
          netJava$addEdge(new(Edge, parentJava, nodeJava))
        }  
      }
      netJava$addNode(nodeJava)
      nodeList = nodeList[-1]
    }
    
  }
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