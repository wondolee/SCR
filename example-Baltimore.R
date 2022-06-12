###   Example: Boltimore data   ###
rm(list=ls())

## dataset
library(spdep)
K <- 5    # number of nearest neighbour
Y <- as.vector(baltimore$PRICE)
X <- as.matrix(cbind(baltimore$NROOM, baltimore$DWELL, baltimore$NBATH, baltimore$PATIO, 
                     baltimore$FIREPL, baltimore$AC, baltimore$BMENT, baltimore$NSTOR,
                     baltimore$GAR, baltimore$AGE, baltimore$CITCOU, baltimore$LOTSZ, baltimore$SQFT))
Sp <- as.matrix(cbind(baltimore$X, baltimore$Y))
knn <- knn2nb(knearneigh(Sp, k=K))
listw_10nn_dates <- nb2listw(knn)
W <- as(as_dgRMatrix_listw(listw_10nn_dates), "CsparseMatrix")*K


## fitting
source("SCR-function.R")
select <- SCR.select(Y, X, W, Sp, G.set=2:10)   # selection of the number of clusters
select$G

fit <- SCR(Y, X, W, Sp, G=select$G)  # estimation with the selected G

plot(Sp, col=fit$group)  

require(ggplot2)
require(lubridate)
require(tseries)
require(forecast)
require(scales)


plot.BIC.re<-ggplot(data=subset(df.ml.melt,G<9&iter<=200),aes(x=G,y=BIC))+
  labs(x="Number of groups",y="BIC criterion",title = "Distribution of BIC value")+
  geom_line(fill='grey20',color='grey20',width=0.3,outlier.color = 'red',outlier.shape = 2)+
  stat_summary(fun="mean", geom="point", shape=22, size=3, fill="blue")+
  theme_bw()+
  geom_smooth(method ="loess")+
  facet_grid(~iter,margins=TRUE)+
  theme(plot.title=element_text(size = rel(4)),
        text = element_text(size = rel(4)),
        strip.text=element_text(size = rel(4)))+
  geom_vline(xintercept = 6,linetype = 'dotted', color='red', size = 1)
  #geom_hline(yintercept = min(BIC),linetype = 'dotted', color='red', size = 1)

ggsave("print.BIC.phi1.0.pdf",plot.BIC.re,width=250, height=150, units = "mm", dpi = 300, bg = "white")
save.image(file = "experiment.phi=0.6.RData")

  