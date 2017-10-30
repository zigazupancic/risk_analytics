
## ----loadPackages, echo=F, warning=F,message=F---------------------------
require(dplyr)
require(ggplot2)
require(xtable)
require(copula)


## ----EllipticCopulas2d, echo=F, results='asis', fig.show = 'hold',out.width='0.5\\linewidth', cache = T----
 rho <- 0.5
degf <- 2
n.samples <- 5e3

pd.a <- 0.1
pd.b <- 0.05


myCop.norm <- normalCopula(param=rho,dim = 2, dispstr = "ex")
u <- data.frame(rCopula(n.samples,myCop.norm))
colnames(u) <- c('a', 'b')
u.def <- u
def.fn <- function(a,b){
  if(a <= pd.a & b <= pd.b){
    'Default A and B'
  } else if (a <= pd.a & b > pd.b){
    'Default A only'
  } else if (a > pd.a & b <= pd.b){
    'Default B only'
  } else {'No Default'}
}

u.def$defaults <- apply(u, 1, FUN = function(x) def.fn(x[1], x[2]))
u.def$defaults <- factor(u.def$defaults)

# Add counts
u.def <- u.def %>% mutate(
 def.a = (a <= pd.a),
 def.b = (b <= pd.b),
 default.count = def.a + def.b
)

ggplot(u.def, aes(x=a, y=b)) + geom_point(aes(colour= defaults)) +
     theme(axis.title=element_text(size=25),
         legend.position = 'bottom',legend.text = element_text(size = 12),
         plot.title = element_text(size = 30), axis.text = element_text(size = 20)) +
   ggtitle(paste0('Gaussian copula, rho = ', as.character(rho)))
# Compute PDs
emp.pd.a <- sum(u.def$a <= pd.a) / n.samples
emp.pd.b <- sum(u.def$b <= pd.b) / n.samples
emp.pd.ab <- sum(u.def$a <= pd.a & u.def$b <= pd.b) / n.samples

#
# # With t-copula
 myCop.t <- ellipCopula(family = "t", dim = 2, dispstr = "ex", param = rho, df = degf)
ut <- data.frame(rCopula(n.samples,myCop.t))
colnames(ut) <- c('a', 'b')
ut.def <- ut

ut.def$defaults <- apply(ut, 1, FUN = function(x) def.fn(x[1], x[2]))
ut.def$defaults <- factor(ut.def$defaults)

ut.def <- ut.def %>% mutate(
 def.a = (a <= pd.a),
 def.b = (b <= pd.b),
 default.count = def.a + def.b
)

ggplot(ut.def, aes(x=a, y=b)) + geom_point(aes(colour= defaults)) +
     theme(axis.title=element_text(size=25),
         legend.position = 'bottom',legend.text = element_text(size = 12),
         plot.title = element_text(size = 30), axis.text = element_text(size = 20)) +
   ggtitle(paste0('t-copula, rho = ', as.character(rho), ' df =  ', as.character(degf)))
# Compute PDs
emp.pd.t.a <- sum(ut.def$a <=pd.a) / n.samples
emp.pd.t.b <- sum(ut.def$b <= pd.b) / n.samples
emp.pd.t.ab <- sum(ut.def$a <= pd.a & ut.def$b <= pd.b) / n.samples




## ----CountsCopulas2d, echo=F, results = 'asis', cache = T----------------
# Contingency tables
table.2d <- rbind(table(u.def$default.count), table(ut.def$default.count))
rownames(table.2d) <- c('Gaussian copula', 't-copula')
out.table <- xtable(table.2d, caption='Default counts')
print(out.table,  caption.placement = 'top')


## ----knitrExample, echo=T, warning=F, message=FALSE, out.width='0.5\\linewidth', size='tiny'----
# Example of knitr chunk
require(MASS)
n <- 1000
x <- mvrnorm(n,
      mu = c(0,0),
      Sigma = matrix(c(1, 0.25, 0.25, 1), 2,2))
plot(x)


