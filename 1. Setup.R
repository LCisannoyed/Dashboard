# Create response data

install.packages("TAM")
library(TAM)
library(tidyverse)
data("data.cqc01")
dat <- data.cqc01
names(dat) <- gsub("BSM","",names(dat))

questions <- names(dat)   # questions / columns
candidates <- row.names(dat)  #  candidates / rows

num_questions <- ncol(dat)   # number of questions / columns
num_candidates <- nrow(dat)   # number of questions / columns

# facilities for data.cqc01 items
sapply(questions, function(x){round( 100*(sum(dat[,x])/ num_candidates),2) } )

# Percentage correct for data.cqc01 candidates
matrix(lapply(candidates, function(x){round( 100*(sum(dat[x,])/ num_questions),2) } ))




#********************************************
#*** Model 01: Estimate Rasch model

# start Tam analysis
mod01 <- TAM::tam(dat, item.elim=FALSE)
# Ability estimate - Weighted Likelihood Estimate
Abil <- TAM::tam.wle(mod01)
PersonAbility <- Abil$theta
meanitemdiff <- mean(mod01$xsi$xsi)


install.packages("flexdashboard")
library(flexdashboard)

library(devtools)
install_github("david-ti/wrightmap",ref = "master", force = TRUE)

library(WrightMap)


# Ability estimate - Weighted Likelihood Estimate
Abil <- TAM::tam.wle(mod01)
PersonAbility <- Abil$theta
## @knitr wrightmap
thr <- TAM::tam.threshold(mod01)


dev.off(dev.list()["RStudioGD"])
WrightMap::wrightMap(thetas=PersonAbility, 
                     thresholds= thr, 
                     mainTitle = "Wright Map", 
                     item.side = itemClassic.LC, 
                     show.thr.lab = TRUE, 
                     show.thr.sym = FALSE, 
                     width = 6, 
                     height = 2, 
                     dim.color = "lightgreen")

#myPaths <- .libPaths()   # get the paths
#myPaths <- c(myPaths[2], myPaths[1])  # switch them
#.libPaths(myPaths)  # reassign them
#.libPaths()

# facilities for data.cqc01 items
sapply(questions, function(x){round( 100*(sum(dat[,x])/ num_candidates),2) } )

itemStats<-cbind(mod01$item,mod01$xsi)  %>%
  select(item ,  N  ,       M  ,    xsi    , se.xsi)%>% 
  mutate(M=round(100*.$M,2))

names(itemStats) <- c("item","N","facility","threshold","se")
itemStats

library(DT)
install.packages('DT')
