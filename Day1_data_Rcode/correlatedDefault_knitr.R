
## ----loadPackages, echo=F, warning=F,message=F---------------------------
rm(list=ls())
require(dplyr)
require(ggplot2)
require(xtable)
require(copula)
require(knitr)


## ----mertonPlot, echo=F,results='asis', fig.align ='center', out.width='0.5\\linewidth', cache = T----
# Modified from http://stackoverflow.com/questions/16893579/color-scale-for-curves-in-ggplot2

set.seed(8)
n <- 100 # number of observations

time <- seq(0, 1, length = n) # x-axis
firm.value <- cumsum(rnorm(n)) +10# Brownian motion

ts <- data.frame(time, firm.value) # data
# Add default threshold
def.thresh <- 1
ts <- ts %>% mutate( default = firm.value <= def.thresh)

# Plot
# Define default region

default.df <- data.frame(x = c(0,1, 1, 0), y=c(def.thresh,def.thresh, min(firm.value), min(firm.value)), value = rep('default',4))
#(default.region <- ggplot(default.df, aes(x=x, y=y)) + geom_polygon(aes(fill=value)))
#ggplot(ts, aes(time, firm.value)) + geom_line() + geom_polygon(data=default.df, aes(x=x, y=y, fill=value))#geom_area(aes(fill=default))
ggplot(data=default.df, aes(x=x, y=y)) + geom_polygon(aes(fill=value))+ geom_line(data=ts, aes(time, firm.value)) +
  labs( x='time', y='firm-value')+
  theme(axis.title=element_text(size=25),
         legend.position = 'bottom',legend.text = element_text(size = 12),
         plot.title = element_text(size = 30), axis.text = element_text(size = 20))+
  guides(fill=F)



## ----EllipticCopulas2d, echo=F, results='hide', cache = T----------------
 rho <- 0.0
degf <- 2
n.samples <- 5e5

pd.a <- 0.1
pd.b <- pd.a
#pd.b <- 0.05


myCop.norm <- normalCopula(param=rho,dim = 2, dispstr = "ex")
u <- data.frame(rCopula(n.samples,myCop.norm))
colnames(u) <- c('a', 'b')
u.def <- u

# Add counts
u.def <- u.def %>% mutate(
 def.a = (a <= pd.a),
 def.b = (b <= pd.b),
 default.count = def.a + def.b
)

# Compute PDs
emp.pd.a <- sum(u.def$a <= pd.a) / n.samples
emp.pd.b <- sum(u.def$b <= pd.b) / n.samples
emp.pd.ab <- sum(u.def$a <= pd.a & u.def$b <= pd.b) / n.samples


## ----defaultDist, echo=F, results='asis'---------------------------------
# Contingency tables
table.2d <- table(u.def$default.count)
table.2d <- table.2d / n.samples
#names(table.2d) <- c('Independent defaults')
out.table <- xtable(t(table.2d), caption='Default count distribution')
print(out.table,  caption.placement = 'top', include.rownames=F)


## ----gaussBMMExample, echo=F, results='hide', cache = T------------------
rho <- 0.5
c1 <- qnorm(0.1)
c2 <- qnorm(0.002)
p1 <- pnorm(c1)
 p2 <- pnorm(c2)

n.samples <- 1e7
#set.seed(123)
phis <- rnorm(n.samples)

r1 <- sqrt(rho)*phis + sqrt(1-rho)*rnorm(n.samples)
def.1 <- (r1 < c1)
r2 <- sqrt(rho)*phis + sqrt(1-rho)*rnorm(n.samples)
def.2 <- (r2 < c2)

emp.pd.1.bmm <- sum(def.1)/n.samples
emp.pd.2.bmm <- sum(def.2)/n.samples
bmm.gauss <- data.frame(default.1 = def.1, default.2 = def.2)
bmm.gauss <- bmm.gauss %>% mutate(default.count = def.1 + def.2)


## ----gaussTable, echo=F, results = 'asis'--------------------------------
table.bmm.gauss <- table(bmm.gauss$default.count)/n.samples
out.table <- xtable(t(table.bmm.gauss), caption='Default count distribution', digits=4)
print(out.table,  caption.placement = 'top', include.rownames=F)


## ----bmm2gauss, echo=F, results='hide', cache = T------------------------
#n.samples <- 1e6
myCop.norm <- normalCopula(param=rho,dim = 2, dispstr = "ex")
u <- data.frame(rCopula(n.samples,myCop.norm))
colnames(u) <- c('a', 'b')

# Add counts
u.gc <- u %>% mutate(
 def.1 = (a < p1),
 def.2 = (b < p2),
 default.count = def.1 + def.2
)
emp.pd.1 <- sum(u.gc$def.1)/n.samples
emp.pd.2 <- sum(u.gc$def.2)/n.samples
table.gauss <- table(u.gc$default.count)/n.samples
#(table.gauss)


## ----tcopula, echo=F, results = 'asis'-----------------------------------
degf <- 3
myCop.t <- ellipCopula(family = "t", dim = 2, dispstr = "ex", param = rho, df = degf)
ut <- data.frame(rCopula(n.samples,myCop.t))
colnames(ut) <- c('a', 'b')
ut.def <- ut

ut.def <- ut.def %>% mutate(
  def.1 = (a < p1),
  def.2 = (b <= p2),
  default.count = def.1 + def.2
)
table.t <- table(ut.def$default.count)/n.samples
table.2d <- rbind(table.bmm.gauss, table.t)
rownames(table.2d) <- c('Gaussian copula', 't-copula')
out.table <- xtable(table.2d, caption='Default count distribution, m=2', digits = 4)
print(out.table,  caption.placement = 'top')


## ----bigPortfolio, echo=F, results = 'asis', fig.show = 'hold',out.width='0.5\\linewidth', cache = T, warning=F, message = F, fig.align='center'----
m <- 125
n.samples <- 1e5
gc <- normalCopula(param=rho,dim = m, dispstr = "ex")
u <- data.frame(rCopula(n.samples,gc))

p <- 0.05
gc.def.df <- (u < p)
gc.default.counts<- data.frame(default.counts = rowSums(gc.def.df))
ggplot(gc.default.counts, aes(x=default.counts)) + geom_histogram() + xlim(c(20,125))+ylim(c(0,2000))+
  theme(axis.title=element_text(size=25),
         legend.position = 'bottom',legend.text = element_text(size = 12),
         plot.title = element_text(size = 30), axis.text = element_text(size = 20)) +
   ggtitle(paste0('Gaussian copula, rho = ', as.character(rho)))

tc <- ellipCopula(family = "t", dim = m, dispstr = "ex", param = rho, df = degf)
ut <- data.frame(rCopula( n.samples, tc))
tc.def.df <- (ut < p)
tc.default.counts <- data.frame(default.counts = rowSums(tc.def.df))
ggplot(tc.default.counts, aes(x=default.counts)) + geom_histogram() + xlim(c(20,125))+ylim(c(0,2000))+
  theme(axis.title=element_text(size=25),
         legend.position = 'bottom',legend.text = element_text(size = 12),
         plot.title = element_text(size = 30), axis.text = element_text(size = 20)) +
   ggtitle(paste0('t-copula, rho = ', as.character(rho), ' df =  ', as.character(degf)))


## ----graphicalPDF, echo=F, results = 'hide'------------------------------
# Values from Graphical Models paper, p21
eta_D <- c(-2.2, -2.2)
eta_DS <- 9.2
eta_S <- 0.2
# Non-normalized probability function
gm.fn.base <- function(d, s, eta_D, eta_S, eta_DS){
  arg <- sum(eta_D*d) + eta_S*s + eta_DS*s*sum(d)
  return(exp(arg))
}

#gm.fn.base(d, s, ets_D, eta_S, eta_DS)

partition.fn <- function(eta_D, eta_S, eta_DS){
  N <- length(eta_D)

  d.list <- list()
  for(i  in 1:N){
    d.list[[i]] <- c(0,1)
  }
  all.d <- expand.grid(d.list)
  s0.sum <- sum(apply(all.d, MARGIN = 1, function(d){gm.fn.base(d, 0, eta_D, eta_S, eta_DS)}))
  s1.sum<- sum(apply(all.d, MARGIN = 1, function(d){gm.fn.base(d, 1, eta_D, eta_S, eta_DS)}))
  return(s0.sum + s1.sum)
}

partition.fn(eta_D, eta_S, eta_DS)
# Check partition function for N=2
1 + exp(eta_D[1]) + exp(eta_D[2]) + exp(eta_S) + exp(sum(eta_D)) + exp(eta_D[1] + eta_DS + eta_S) + exp(eta_D[2] + eta_DS+eta_S) + exp(sum(eta_D) + eta_S + 2*eta_DS)

#Normalized probability funciton
gm.pdf <- function(d, s, eta_D, eta_S, eta_DS){gm.fn.base(d, s, eta_D, eta_S, eta_DS) / partition.fn(eta_D, eta_S, eta_DS)}

gm.pdf.D <- function(d, eta_D, eta_S, eta_DS){gm.pdf(d, s=0, eta_D, eta_S, eta_DS)+ gm.pdf(d, s=1, eta_D, eta_S, eta_DS)}


## ----gmcdTests, echo=F, results='hide'-----------------------------------
# Values from Graphical Models paper, p21
eta_D <- c(-2.2, -2.2)
eta_DS <- 9.2
eta_S <- 0.2
#d <- c(0,1)
#s <- 1

gm.pdf.D(d=c(0,0), eta_D, eta_S, eta_DS)
gm.pdf.D(d=c(0,1), eta_D, eta_S, eta_DS)
gm.pdf.D(d=c(1,0), eta_D, eta_S, eta_DS)
gm.pdf.D(d=c(1,1), eta_D, eta_S, eta_DS)

# Try with other parameters
eta_D <- rep(-2,2)
eta_DS <- -2.1
eta_S <- 15
gm.pdf.D(d=c(0,0), eta_D, eta_S, eta_DS)
gm.pdf.D(d=c(0,1), eta_D, eta_S, eta_DS)
gm.pdf.D(d=c(1,0), eta_D, eta_S, eta_DS)
gm.pdf.D(d=c(1,1), eta_D, eta_S, eta_DS)

# Marginals
pd.1 <- gm.pdf.D(d=c(1,0), eta_D, eta_S, eta_DS) + gm.pdf.D(d=c(1,1), eta_D, eta_S, eta_DS)
pd.2 <- gm.pdf.D(d=c(0,1), eta_D, eta_S, eta_DS) + gm.pdf.D(d=c(1,1), eta_D, eta_S, eta_DS)

# N=1 example
eta_D <- 1
eta_DS <- 2
eta_S <- 3
gm.pdf(0,0, eta_D, eta_S, eta_DS) + gm.pdf(0,1, eta_D, eta_S, eta_DS) + gm.pdf(1,0, eta_D, eta_S, eta_DS) +gm.pdf(1,1, eta_D, eta_S, eta_DS) # should be 1
gm.pdf.D(0, eta_D, eta_S, eta_DS) + gm.pdf.D(1, eta_D, eta_S, eta_DS) # should be 1



## ----gmExampleParams, echo=F, results = 'hide'---------------------------
# #As in Figure 6, Section 3.1
# m <- 2
# eta_D <- rep(-2.5,m)
# eta_DS <- -2.1
# eta_S <- 7

# Other try
m <- 15
eta_D <- rep(-0.7,m)

eta_S <- 5.5
eta_DS <- -2.1


## ----gmDefaultDistn, echo=F, results = 'asis', out.width='0.5\\linewidth', cache = T, warning=F, message = F, fig.align='center', fig.cap = 'Graphical Model Default Count Distribution'----

def.ct.fn <- function(n, eta_D, eta_S, eta_DS){
  m <- length(eta_D)
  if (m>1){
  # Check that all eta_D components equal
  if(!all.equal(eta_D, rep(eta_D[1], m))){ stop('Not all entries of eta_D equal') }
  }
  Z <- partition.fn(eta_D, eta_S, eta_DS)

  eta_Di <- eta_D[1]
  exps <- exp(eta_Di*n) + exp(eta_S + n*(eta_Di + eta_DS))
  return(choose(m,n)/Z*exps)
}

#eta_DS <- -2.1

# Loop over increasing ms
#calc.times <- rep(0,length(ms))
#for (i in 1:length(ms)){

  max.n <- min(10, m)
  start.time <- proc.time()
  def.ct.ps <- sapply(0:max.n, function(n) def.ct.fn(n, eta_D, eta_S, eta_DS))
  calc.time <- as.numeric(proc.time() - start.time)[3]
  gm.def.df <- data.frame(default.count = 0:max.n, probability = def.ct.ps)
  #out.table <- xtable(t(gm.def.df$probability), caption=paste0('Default distribution, m = ', as.character(m)), digits = -2)
  #print(out.table,  caption.placement = 'top', include.rownames=F)
#}
qplot(default.count, probability, data = gm.def.df, geom = c("point","line"))
#gm.def.df



