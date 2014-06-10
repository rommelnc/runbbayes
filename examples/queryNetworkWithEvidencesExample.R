nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
net = createNetwork(nodes, compile = T)
posterioriProb = queryNetworkWithEvidences(net, c("either", "yes"), list(c("asia", "yes"), c("smoker","no")))
posterioriProb

Console:
  0.0645