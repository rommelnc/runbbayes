# runbbayes
It provides integration to UnBBayes probabilistic framework using rJava.

# How to Install

In order to install runbbayes run the following code

```r
library(devtools)
install_github('rommelnc/runbbayes')
```

If you get the following error message

```r
Downloading github repo rommelnc/runbbayes@master
Error in curl::curl_fetch_memory(url, handle = handle) : 
  Peer certificate cannot be authenticated with given CA certificates
```

Try running the code

```r
library(httr)
set_config( config( ssl_verifypeer = 0L ) )
```

Then try installing runnbayes again

```r
install_github('rommelnc/runbbayes')
```
