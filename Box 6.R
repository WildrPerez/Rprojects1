# The BHSBVAR package was created by Paul Richardson in January, 2020
# And it is based on the MATLAB programs created by Baumeister and Hamilton (2015/2017/2018).

############################################################

setwd("C:/Users/enriq/Dropbox/Bases de primarios/Supply and Demand Shocks during")

############################################################

rm(list = ls())
#install.packages("BHSBVAR")
library(BHSBVAR)
set.seed(123)

#install.packages("readxl")

library(readxl)
Data <- read_excel("C:/Users/enriq/Dropbox/Bases de primarios/Supply and Demand Shocks during/Excel/Data.xlsx")

lmodel1 <- lm(h ~ i, data = Data)
lmodel1$coefficients

lmodel2 <- lm(l ~ i, data = Data)
lmodel2$coefficients

lmodel3 <- lm(hp ~ i, data = Data)
lmodel3$coefficients


y0 <- matrix(data = c(Data$i, Data$hp), ncol = 2)
#y0 <- matrix(data = c(Data$i, Data$h), ncol = 2)
#y0 <- matrix(data = c(Data$i, Data$l), ncol = 2)
#y0 <- matrix(data = c(Data$i_prim, Data$hp_prim), ncol = 2)
#y0 <- matrix(data = c(Data$i_manu, Data$hp_manu), ncol = 2)
#y0 <- matrix(data = c(Data$i_construc, Data$hp_construc), ncol = 2)
#y0 <- matrix(data = c(Data$i_comer, Data$hp_comer), ncol = 2)
#y0 <- matrix(data = c(Data$i_serv, Data$hp_serv), ncol = 2)

y <- y0 - (matrix(data = 1, nrow = nrow(y0), ncol = ncol(y0)) %*%
               + diag(x = colMeans(x = y0, na.rm = FALSE, dims = 1)))
colnames(y) <- c("Salario", "Empleo")

nlags <- 8
itr <- 200000
burn <- 0
thin <- 20
acc_irf <- TRUE
h1_irf <- 20
cri <- 0.95

pA <- array(data = NA, dim = c(ncol(y), ncol(y), 8))
pA[, , 1] <- c(0, NA, 0, NA)
pA[, , 2] <- c(1, NA, -1, NA)
pA[, , 3] <- c(0.25, 1, -0.25, 1)
#pA[, , 3] <- c(0.22, 1, -0.22, 1)
#pA[, , 3] <- c(0.08, 1, -0.08, 1)
pA[, , 4] <- c(0.5, NA, 0.3, NA)
pA[, , 5] <- c(3, NA, 3, NA)
pA[, , 6] <- c(NA, NA, NA, NA)
pA[, , 7] <- c(NA, NA, 1, NA)
pA[, , 8] <- c(2, NA, 2, NA)

############################################################

pP <- matrix(data = 0, nrow = ((nlags * ncol(pA)) + 1), ncol = ncol(pA))
pP[1:nrow(pA), 1:ncol(pA)] <-
  + diag(x = 1, nrow = nrow(pA), ncol = ncol(pA))

x1 <- matrix(data = NA, nrow = (nrow(y) - nlags),ncol = (ncol(y)*nlags))

for (k in 1:nlags) {
  x1[, (ncol(y) * (k - 1) + 1):(ncol(y) * k)] <- 
  y[(nlags - k + 1):(nrow(y) - k),]
}

x1 <- cbind(x1, 1)
colnames(x1)<-c(paste(rep(colnames(y), nlags),"_L",
              sort(rep(seq(from = 1, to = nlags, by = 1),
              times = ncol(y)),
              decreasing = FALSE), sep = ""), "cons")

y1 <- y[(nlags + 1):nrow(y),]
ee <- matrix(data = NA, nrow = nrow(y1), ncol = ncol(y1))

for (i in 1:ncol(y1)) {
  xx <- cbind(x1[, seq(from = i, to = (ncol(x1) - 1), by = ncol(y1))], 1)
  yy <- matrix(data = y1[, i], ncol = 1)
  phi <- solve(t(xx) %*% xx, t(xx) %*% yy)
  ee[, i] <- yy - (xx %*% phi) 
}

somega <- (t(ee) %*% ee)/nrow(ee)
lambda0 <- 0.2
lambda1 <- 1
lambda3 <- 100
v1 <- matrix(data = (1:nlags), nrow = nlags, ncol = 1)
v1 <- v1^((-2) * lambda1)
v2 <- matrix(data = diag(solve(diag(diag(somega)))), ncol = 1)
v3 <- kronecker(v1, v2)
v3 <- (lambda0^2) * rbind(v3, (lambda3^2))
v3 <- 1/v3
pP_sig <- diag(x = c(v3), nrow = nrow(v3), ncol = nrow(v3))


pR_sig <- array(data = 0, dim = c(((nlags * ncol(y)) + 1),
((nlags * ncol(y)) + 1),
ncol(y)))

Ri <- cbind(kronecker(matrix(data = 1, nrow = 1, ncol = nlags),
                    + matrix(data = c(1, 0), nrow = 1)),0)

pR_sig[, , 2] <- (t(Ri) %*% Ri) / 0.1

############################################################

library(BHSBVAR)

kappa1 <- matrix(data = 2, nrow = 1, ncol = ncol(y))
results1 <- BH_SBVAR(y = y, nlags = nlags, pA = pA, pP = pP, pP_sig = pP_sig,
pR_sig = pR_sig, kappa1 = kappa1, itr = itr, burn = burn, thin = thin, acc_irf = acc_irf,
h1_irf = h1_irf, cri = cri)


varnames <- c("Salary","Employment")
shocknames <- c("Labor Demand","Labor Supply")

par(cex.axis = 0.8, cex.main = 1, font.main = 1, family = "serif", 
      mfrow = c(2, 2), mar = c(2, 2.2, 2, 1), las = 1)
irf_results <- IRF_Plots(results = results1, varnames = varnames, shocknames = shocknames)

############################################################

freq <- 12
start_date <- c(2002,11)
par(cex.axis = 0.8, cex.main = 1, font.main = 1, family = "serif",
mfrow = c(2, 2), mar = c(2, 2.2, 2, 1), las = 1)
hd_results <- HD_Plots(results = results1, varnames = varnames,
         shocknames = shocknames, freq = freq,
         start_date = start_date)

A_titles <- matrix(data = NA_character_, nrow = dim(pA)[1], ncol = dim(pA)[2])
A_titles[1, 1] <- "Elasticidad salario - demanda laboral"
A_titles[1, 2] <- "Elasticidad salario - oferta laboral"
par(cex.axis = 0.8, cex.main = 1, font.main = 1, family = "serif",
mfrow = c(1, 2), mar = c(3, 3, 2, 1), las = 1)
Dist_Plots(results = results1, A_titles = A_titles)
legend("topright",legend=c("Posterior", "Prior"),
      col=1:2,lty=rep(1,2),ncol=1,cex=0.8)

#install.packages("writexl")
library(writexl)
df <- as.data.frame(hd_results)
#write_xlsx(df, "data3.xlsx")
#write_xlsx(df, "data4.xlsx")
write_xlsx(df, "data5.xlsx")
#write_xlsx(df, "dataprim.xlsx")
#write_xlsx(df, "dataservc.xlsx")
#write_xlsx(df, "datacomer.xlsx")
#write_xlsx(df, "dataconstruc.xlsx")
#write_xlsx(df, "dataindustry.xlsx")









