This package constitutes an interactive R problem set based on the RTutor package (https://github.com/skranz/RTutor). 

Evaluation of Free Trade Effects, Exemplified by NAFTA -- Discussion on Free Trade Literature, Relevancy for Negotiations like TTIP

Based on a Publication by Caliendo and Parro (2015): Estimates of the Trade and Welfare Effects of NAFTA (http://restud.oxfordjournals.org/content/82/1/1.abstract)

## 1. Installation

RTutor and this package is hosted on Github. To install everything, run the following code in your R console.
```s
if (!require(devtools))
  install.packages("devtools")
source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4")
install.rtutor(update.github=TRUE)
  
install_github("fischeruu/RTutorNAFTAfreetrade")
```

## 2. Show and work on the problem set
To start the problem set first create a working directory in which files like the data sets and your solution will be stored. Then adapt and run the following code.
```s
library(RTutorNAFTAfreetrade)

# Adapt your working directory to an existing folder
setwd("C:/problemsets/RTutorNAFTAfreetrade")
# Adapt your user name
run.ps(user.name="Jon Doe", package="RTutorNAFTAfreetrade",
       load.sav=TRUE, sample.solution=FALSE)
```
If everything works fine, a browser window should open, in which you can start exploring the problem set.
