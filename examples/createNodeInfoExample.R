node.a = createNodeInfo(~asia, prob=c(0.01, 0.99), states=c("yes","no"))
node.a

node.t = createNodeInfo(~tub|asia, prob=c(0.05, 0.95, 0.01, 0.99),states=c("yes","no"))
node.t

node.s = createNodeInfo(~smoke, prob=c(0.5,0.5), states=c("yes","no"))
node.s

node.l = createNodeInfo(~lung|smoke, prob=c(0.1, 0.9, 0.01, 0.99), states=c("yes","no"))
node.l

node.b = createNodeInfo(~bronc|smoke, prob=c(0.6, 0.4, 0.3, 0.7), states=c("yes","no"))
node.b

node.e = createNodeInfo(~either|lung:tub,prob=c(1,0,1,0,1,0,0,1),states=c("yes","no"))
node.e

node.x = createNodeInfo(~xray|either, prob=c(0.98, 0.02, 0.05, 0.95), states=c("yes","no"))
node.x

node.d = createNodeInfo(~dysp|bronc:either, prob=c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9), states=c("yes", "no"))
node.d
