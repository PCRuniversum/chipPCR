![chipPCR](https://github.com/michbur/chipPCR/blob/master/vignettes/logo.png)

Installation
------------

chipPCR is available [on CRAN](http://cran.r-project.org/web/packages/chipPCR/), so installation is as simple as:

```
install.packages("chipPCR")
```

You can install the latest development version of the code using the `devtools` R package.

```
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("chipPCR", "michbur")
```

Problems with viginette
------------

Some distributions like Debian sid might fail to compile the package.

Messages:
```
sh: 1: /usr/bin/texi2dvi: not found
Calls: <Anonymous> -> texi2pdf -> texi2dvi
```

In this case try to install "texinfo" (apt-get install texinfo).