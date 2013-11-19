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


queryNetwork <- function(network, event, evidences) {
  #network: object of a ProbabilisticNetwork from rJava
  #event: array of two strings (tuple)
  #evidences: list of arrays of two strings (list of tuples)
  
  ## Query example: queryNetwork(net, c("T", "yes"), list(c("E", "no"), c("D", "yes")))
  ## Query example: queryNetwork(net, c("S", "no"), list())
  
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
  
  
  return(posteriorProb)
}