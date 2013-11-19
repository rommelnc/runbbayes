Sys.setenv(NOAWT= "true")

library(rJava)

.jinit()

cl = .jclassLoader()
cl$verbose=FALSE

.jaddClassPath("./inst/java/avalon-framework-4.1.3.jar")
.jaddClassPath("./inst/java/commons-logging-1.1.jar")
.jaddClassPath("./inst/java/icu4j-3.8.jar")
.jaddClassPath("./inst/java/jaxme2-0.5.1.jar")
.jaddClassPath("./inst/java/jaxme2-rt-0.5.1.jar")
.jaddClassPath("./inst/java/jaxmeapi-0.5.1.jar")
.jaddClassPath("./inst/java/jaxmejs-0.5.1.jar")
.jaddClassPath("./inst/java/jaxmexs-0.5.1.jar")
.jaddClassPath("./inst/java/jpf-1.5.jar")
.jaddClassPath("./inst/java/junit-4.1.jar")
.jaddClassPath("./inst/java/log4j-1.2.12.jar")
.jaddClassPath("./inst/java/logkit-1.0.1.jar")
.jaddClassPath("./inst/java/servlet-api-2.3.jar")
.jaddClassPath("./inst/java/unbbayes-4.11.5.jar")
.jaddClassPath("./inst/java/xalan-2.7.0.jar")
.jaddClassPath("./inst/java/xml-apis-1.0.b2.jar")