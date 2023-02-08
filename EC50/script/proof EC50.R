library (drc)
library(readxl)
for_R <- read_excel("C:/Users/paredes.24/Dropbox/carbon_mani/EC50/data/for R.xlsx")
View(for_R)

proof_1 <- drm(for_R, fct = LL.4())
## Calculating EC/ED values
#ED(proof_1, c(50), interval = "fls")

ED(proof_1, c(10, 50, 90), interval = "delta")

plot(proof_1)

EC50 <- for_R

library(multcomp)
summary(glht(proof_1))

ED(proof_1, c(5, 10, 50), interval = "delta")
##
## Estimated effective doses
## (Delta method-based confidence interval(s))
##
## Estimate Std. Error Lower Upper
## e:1:5 0.99088 0.17808 0.62053 1.3612
## e:1:10 1.34086 0.18904 0.94773 1.7340
## e:1:50 3.26336 0.19641 2.85491 3.6718

plot(proof_1, broken = TRUE, type = "all",
     xlab = "Ferulic acid (mM)", xlim = c(0, 100),
     ylab = "Root length (cm)")

#For more isolates

library(readxl)
proof_isolate_r <- read_excel("C:/Users/paredes.24/Dropbox/carbon_mani/EC50/data/proof isolate r.xlsx")
View(proof_isolate_r)

proof_1.m1 <- drm(rg~conc, isolate, data=proof_isolate_r, fct = LL.4())

summary(proof_1.m1)
plot(proof_1.m1, xlim=c(0,100), ylim=c(0,100))

ED(proof_1.m1, c(50), interval = "delta")


#proof_1.m2 <- drm(rg~conc, data=for_R, fct = LL.3())

#plot(proof_1.m2, xlim=c(0,50), conLevel=1e-4, add=TRUE, type="none", col="red")


