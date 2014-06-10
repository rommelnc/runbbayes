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