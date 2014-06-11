node.a = createNodeInfo(~asia, prob=c(0.01, 0.99), states=c("yes","no"))
node.t = createNodeInfo(~tub|asia, prob=c(0.05, 0.95, 0.01, 0.99),states=c("yes","no"))
node.s = createNodeInfo(~smoke, prob=c(0.5,0.5), states=c("yes","no"))
node.l = createNodeInfo(~lung|smoke, prob=c(0.1, 0.9, 0.01, 0.99), states=c("yes","no"))
node.b = createNodeInfo(~bronc|smoke, prob=c(0.6, 0.4, 0.3, 0.7), states=c("yes","no"))
node.e = createNodeInfo(~either|lung:tub,prob=c(1,0,1,0,1,0,0,1),states=c("yes","no"))
node.x = createNodeInfo(~xray|either, prob=c(0.98, 0.02, 0.05, 0.95), states=c("yes","no"))
node.d = createNodeInfo(~dysp|bronc:either, prob=c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9), states=c("yes", "no"))

nodes = list(node.a, node.t, node.s, node.l, node.b, node.e, node.x, node.d)
# returning a network that is not compiled
net = createNetwork(nodes)
net

# returning a compiled network
net = createNetwork(nodes, compile = TRUE)
net