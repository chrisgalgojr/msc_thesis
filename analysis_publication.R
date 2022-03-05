##FinalCodeThesis

getwd()
library(lavaan)
library(psych)
library(semPlot)

###Exploratory Factor Score Analysis
data<-data.frame(read.csv("Data_analysis.csv", sep=";", header=TRUE)) 
indicators<-data[1:105,119:159]
indicators<-na.omit(indicators)
psych::scree(indicators)
psych::fa.parallel(indicators)


fac1<-factanal(indicators,factors=8,rotation="varimax")
print(fac1, cutoff=.50)

###Cron bach Many sources say above 0.70 is acceptable. 0.80 or greater is preferred. Higher is better.
##Source: Cortina, J. M. (1993). What is coefficient alpha? An examination of theory and applications. Journal of applied psychology, 78(1), 98.


###1. Now we fit the model with variables who have high factor loadings to avoid positive definiteness

model1<-' #latent variable definitions
so=~SO1+SO2+SO3+SO4
info=~Info1+Info2+Info3+Info4+Info5+Info6
dv=~Diversity1+DIversity2+Diversity3+Diversity4+Diversity5
fc=~FC1+FC2+FC3+FC5
bc=~BC3+BC4+BC5+BC6
vs=~Visibility1+Visibility2+Visibility3+Visibility4+Visibility5+Visibility6+Visibility7
'
fit<-sem(model1,data=indicators,estimator="MLR")
summary(fit, fit.measures=TRUE, standardized=TRUE)
semPaths(fit,what="paths",whatLabels="std",edge.label.cex = 1.0, rotation=2, style="lisrel", residScale = 10, layout = "tree2", sizeMan = 8)

###allfitmeasures
fitmeasures(fit)
fitmeasures(fit,c("chisq","cfi","rmsea","tli"))


##new model with fit indices
model2<-paste0(model1, "\n","Visibility6~~Visibility7")
model4<-paste0(model2,"\n","Visibility4~~Visibility5")
model5<-paste0(model4,"\n","Visibility2~~Visibility3")
model6<-paste0(model5,"\n","Visibility2~~Visibility4")

fit2<-sem(model2,data=indicators,estimator="MLR")
fit6<-sem(model6,data=indicators,estimator="MLR")

summary(fit2, fit.measures=TRUE, standardized=TRUE)
fitmeasures(fit6,c("chisq","cfi","rmsea","tli"))

semPaths(fit6,what="paths",whatLabels="std",edge.label.cex = 1.0, rotation=2, style="lisrel", residScale = 10, layout = "tree2", sizeMan = 8)



resid(fit, type="cor")
mod_ind<-modificationindices(fit)

#top 10 softed by size 
head(mod_ind[order(mod_ind$mi,decreasing=TRUE),],10)



###The fit was poor so let's try to make small structural relationships betweeen latent variables

##1. FC and Buffering capacity
model1<-' #latent variable definitions
fc=~FC1+FC2+FC3+FC5
bc=~BC3+BC4+BC5+BC6
#factor covariance
fc~~bc
'
#1.2
###Fitting the structural models - using ML estimator
fit<-sem(model1,data=indicators)
fit01<-sem(model1,data=indicators,estimator="MLR")
summary(fit01, fit.measures=TRUE, standardized=TRUE)

##1.3 Diagram
semPaths(fit01,what="paths",whatLabels="std",edge.label.cex = 1.2, rotation=2, style="lisrel", residScale = 8)

###2. Financial capital and visibility
model2<-' #latent variable definitions
fc=~FC1+FC2+FC3+FC5
vs=~Visibility1+Visibility2+Visibility3+Visibility4+Visibility5+Visibility6+Visibility7
#factor covariance
fc~~vs
'
fit02<-sem(model2,data=indicators,estimator="MLR")
summary(fit02, fit.measures=TRUE, standardized=TRUE)
semPaths(fit02,what="paths",whatLabels="std",edge.label.cex = 1)


###3. Diversity and financial capital

model3<-'#latent variable definitions
dv=~Diversity1+DIversity2+Diversity3+Diversity4+Diversity5
fc=~FC1+FC2+FC3+FC5
#factor covariance
dv~~fc
'
fit03<-sem(model3,data=indicators,estimator="MLR")
summary(fit03, fit.measures=TRUE, standardized=TRUE)
semPaths(fit03,what="paths",whatLabels="std",edge.label.cex = 1.2, rotation=2, style="lisrel", residScale = 8)

###4. diversity and buffering capacity
model4<-'#latent variable definitions
dv=~Diversity1+DIversity2+Diversity3+Diversity4+Diversity5
bc=~BC3+BC4+BC5+BC6
#factor covariance
dv~~bc
'
fit4<-sem(model4,data=indicators,estimator="MLR")
summary(fit4, fit.measures=TRUE, standardized=TRUE)
semPaths(fit4,what="paths",whatLabels="std",edge.label.cex = 1)

###diversity and visibility
model5<-'#latent variable definitions
dv=~Diversity1+DIversity2+Diversity3+Diversity4+Diversity5
vs=~Visibility1+Visibility2+Visibility3+Visibility4+Visibility5+Visibility6+Visibility7
#factor covariance
dv~~vs
'
fit5<-sem(model5,data=indicators,estimator="MLR")
summary(fit5, fit.measures=TRUE, standardized=TRUE)
semPaths(fit5,what="paths",whatLabels="std",edge.label.cex = 1)


###Information sharing and financial capital
model6<-'#latent variable definitions
info=~Info1+Info2+Info3+Info4+Info5+Info6
fc=~FC1+FC2+FC3+FC5
#factor covariance
info~~fc
'

fit6<-sem(model6,data=indicators,estimator="MLR")
summary(fit6, fit.measures=TRUE, standardized=TRUE)
semPaths(fit6,what="paths",whatLabels="std", edge.label.cex = 1.2, rotation=2, style="lisrel", residScale = 8)


###Information sharing and diversity

model7<-'#latent variable definitions
info=~Info1+Info2+Info3+Info4+Info5+Info6
dv=~Diversity1+DIversity2+Diversity3+Diversity4+Diversity5
#factor covariance
info~~dv
'
fit7<-sem(model7,data=indicators,estimator="MLR")
summary(fit7, fit.measures=TRUE, standardized=TRUE)
semPaths(fit7,what="paths",whatLabels="std",edge.label.cex = 1.2, rotation=2, style="lisrel", residScale = 8)

##information sharing and buffering capacity
model8<-'#latent variable definitions
info=~Info1+Info2+Info3+Info4+Info5+Info6
bc=~BC3+BC4+BC5+BC6
#factor covariance
info~~bc
'
fit8<-sem(model8,data=indicators,estimator="MLR")
summary(fit8, fit.measures=TRUE, standardized=TRUE)
semPaths(fit8,what="paths",whatLabels="std",edge.label.cex = 1)

###information sharing and visibility
model9<-'#latent variable definitions
info=~Info1+Info2+Info3+Info4+Info5+Info6
vs=~Visibility1+Visibility2+Visibility3+Visibility4+Visibility5+Visibility6+Visibility7
#factor covariance
info~~vs
'

fit9<-sem(model9,data=indicators,estimator="MLR")
summary(fit9, fit.measures=TRUE, standardized=TRUE)
semPaths(fit9,what="paths",whatLabels="std",edge.label.cex = 1)


###Self-organisation and vsibility

model10<-'#latent variable definitions
so=~SO1+SO2+SO3+SO4
vs=~Visibility1+Visibility2+Visibility3+Visibility4+Visibility5+Visibility6+Visibility7
#factor covariance
so~~vs
'

fit10<-sem(model10,data=indicators,estimator="MLR")
summary(fit10, fit.measures=TRUE, standardized=TRUE)
semPaths(fit10,what="paths",whatLabels="std",edge.label.cex = 1)

##self organisation and buffering capacity

model11<-'#latent variable definitions
so=~SO1+SO2+SO3+SO4
bc=~BC3+BC4+BC5+BC6
#factor covariance
so~~bc
'

fit11<-sem(model11,data=indicators,estimator="MLR")
summary(fit11, fit.measures=TRUE, standardized=TRUE)
semPaths(fit11,what="paths",whatLabels="std",edge.label.cex = 1)


###self-organisation and diversity

model12<-'#latent variable definitions
so=~SO1+SO2+SO3+SO4
dv=~Diversity1+DIversity2+Diversity3+Diversity4+Diversity5
#factor covariance
so~~dv
'
fit12<-sem(model12,data=indicators,estimator="MLR")
summary(fit12, fit.measures=TRUE, standardized=TRUE)
semPaths(fit12,what="paths",whatLabels="std",edge.label.cex = 1.3, rotation=2, style="lisrel", residScale = 8)


##self-organisation and financial capital

model13<-'#latent variable definitions
so=~SO1+SO2+SO3+SO4
fc=~FC1+FC2+FC3+FC5
#factor covariance
so~~fc
'
fit13<-sem(model13,data=indicators,estimator="MLR")
summary(fit13, fit.measures=TRUE, standardized=TRUE)
semPaths(fit13,what="paths",whatLabels="std",edge.label.cex = 1)

##self-organisation and information sharing

model14<-'#latent variable definitions
so=~SO1+SO2+SO3+SO4
info=~Info1+Info2+Info3+Info4+Info5+Info6
#factor covariance
so~~info
'
fit14<-sem(model14,data=indicators,estimator="MLR")
summary(fit14, fit.measures=TRUE, standardized=TRUE)
semPaths(fit14,what="paths",whatLabels="std",edge.label.cex = 1.2, rotation=2, style="lisrel", residScale = 8)

###Graphical representation of the analytical model 
library(psych)
lavaan.diagram(fit0)
lavaan.diagram(fit20)

##Syntax for SEMPLOT
library(semPlot)
semPaths(fit012,what="paths",whatLabels="par",edge.label.cex = 1)
semPaths(fit20,what="paths",whatLabels="std",edge.label.cex = 1)

###Getting the parameter esttimates
parameterestimates(fit)

fitMeasures(fit,c("cfi","rmsea"))
standardizedsolution(fit)
sfit<-standardizedsolution(fit)

fitmeasures(fit)
