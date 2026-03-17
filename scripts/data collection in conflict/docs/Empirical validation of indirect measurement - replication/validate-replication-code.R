#####################################
##  AN EMPIRICAL VALIDATION STUDY  ##   
## OF POPULAR SURVEY METHODOLOGIES ##
##     FOR SENSITIVE QUESTIONS     ##
#####################################

rm(list = ls())

# Set working directory
# Note: need to insert a path below
#setwd("")

# Set seed for replication
set.seed(44)

# Install necessary packages
# Note: the package versions used in the analysis are included in 
# the replication archive and, once saved locally, can be installed from source. 
# You will need to specify the full path to the local file in place
# of "file_path" below.

install.packages(file_path/list_7.1.tar.gz, repos = NULL, type="source")
install.packages(file_path/endorse_1.4.1.tar.gz, repos = NULL, type="source")
install.packages(file_path/rr_1.3.tar.gz, repos = NULL, type="source")
install.packages(file_path/lme4_1.1-7.tar.gz, repos = NULL, type="source")
install.packages(file_path/arm_1.7-07.tar.gz, repos = NULL, type="source")
install.packages(file_path/MASS_7.3-39.tar.gz, repos = NULL, type="source")
install.packages(file_path/coda_0.17-1.tar.gz, repos = NULL, type="source")


# Load necessary packages
library("list")
library("endorse")
library("rr")
library("lme4")
library("arm")
library("MASS")
library("coda")
library("xtable")
library("stargazer")
library("car")
library("calibrate")

# Run code to clean and weight data
source(file="validate-replication-code-data-setup-weighting.R", echo=T)


###########################
##      FIGURE 1         ##
## SAMPLE SELECTION PLOT ##
###########################

# Plot relationship between republican gubernatorial vote share and measure 26 w/ linear fit
# Load county-level election results 
load("ms-g11-county-vote-data.RData")

require(car)
pdf("county-selection.pdf", 5, 5) 
par(cex=.8,
    mar=c(4,4,1,1),
    oma=c(0,0,0,0))
scatterplot((1-yes26) ~ repgov | insample, data=vote,
            xlim=c(0,1),
            ylim=c(0,1),
            reg.line=FALSE,
            xlab="Republican Gubernatorial Vote Share", 
            ylab="Proportion of 'no' votes on Personhood",
            smooth=FALSE,
            pch=c(21,19),
            grid=FALSE,
            legend.plot=FALSE) 
abline(lm((1-vote$yes26)~vote$repgov))
legend('bottomright', c("Sampled County", "Not in Sample"), 
       pch=c(19,21), col=c('red', 'black'), bty='n', cex=1)
dev.off()


############################
##        TABLE 1         ##
## DESCRIPTIVE STATISTICS ##
############################

#Direct
sum(table(dat$qd))
prop.table(table(dat$qd))
prop.table(table(dat$qd))[2]+prop.table(table(dat$qd, useNA=("ifany")))[3]

#List
sum(table(dat$q2a))
prop.table(table(dat$q2a))
prop.table(table(dat$q2a))[1]+prop.table(table(dat$q2a))[2]
sum(table(dat$q2b))
prop.table(table(dat$q2b))

#Endorsement
sum(table(dat$q1aa))
sum(table(dat$q1ba))
prop.table(table(dat$q1aa))
prop.table(table(dat$q1ba))

#RRT
sum(table(dat$qc3))
prop.table(table(dat$qc3))

##########################
##       FIGURE 2       ##
## PLOT DIRECT QUESTION ##
##########################
##Plot direct question on support for referendum,
#and actual voting on the referendum (y-axis) by county 
#(ordered by actual vote share on x-axis)

# Estimates and SEs from direct question
dir.est <- data.frame(tapply(dat$dir, dat$county, mean, na.rm=T))
names(dir.est)[1] <- "Direct Estimate"

dir.se <- NULL
for (i in levels(dat$county)){
  county <- subset(dat, county == i)
  dir.se[i] <- sd(county$dir, na.rm=T)/sqrt(nrow(as.matrix(na.omit(county$dir)))) 
}

direct.unweighted.results <- cbind("county"=rownames(dir.est), 1-dir.est, "Direct Unweighted SE" = dir.se) #'No' vote estimate

# Load election results by county
load("ms-g11-county-vote-data.RData")
vote <- subset(vote, insample==1)
vote.comp <- data.frame(cbind(vote, direct.no=direct.unweighted.results[,2], se=direct.unweighted.results[,3]))
vote.comp.order <- vote.comp[order(vote.comp$no26, vote.comp$direct.no), ] 
up <- (vote.comp.order$direct.no+qnorm(.975)*vote.comp.order$se)
low <- (vote.comp.order$direct.no-qnorm(.975)*vote.comp.order$se)

# Create county axis lables with n=XXX
xlabels <- subset(vote.comp.order, select=COUNTY)
xlabels$id  <- 1:nrow(xlabels)
dir <- read.table("dir-weight.txt") 
dir.n <- data.frame(table(dir$county))
countyfreq <- merge(xlabels, dir.n, by.x="COUNTY", by.y="Var1")
countyfreq <- countyfreq[order(countyfreq$id), ]
# Now paste variables together for the label
xlabels <- paste(paste(countyfreq$COUNTY, countyfreq$Freq, sep=" ("), ")", sep="")

pdf("comparison-govvote-vs-directq.pdf", 8, 6)
par(mar=c(10,4,1,1))
plot(1:19, vote.comp.order$no26, type="p", pch=19, col='red', ylim=c(0,1), ylab="Proportion of 'no' votes on Personhood", xlab="", xaxt="n", las=2)
segments(1:19, low, 1:19, up)
points(vote.comp.order$direct.no, pch=21)
axis(1,at=1:length(vote.comp.order$COUNTY), label=xlabels, las=2) 
text(8.3,.66, "Actual vote")
text(8,.12, "Direct question")
dev.off()

#######################
##    SECTION 4.2    ##
##  POOLED ANALYSIS  ##
## STATEWIDE RESULTS ##
#######################

#################
##   DIRECT    ##
#################

dir <- read.table("dir-weight.txt")

# UNWEIGHTED

dir.est <- mean(dir$dir)
se.dir <- sd(dir$dir, na.rm=T)/sqrt(nrow(as.matrix(na.omit(dir$dir))))

# WEIGHTED

dir.est.weighted <- sum(dir$dir*dir$weight, na.rm=T)
se.dir.weighted <- sqrt(sum(dir$weight^2, na.rm=T)*var(dir$dir, na.rm=T))

# REGRESSION ADJUSTED

dir.logit <- glm(dir ~ dem + gop + maleNA + age55plus + ageNA, family=binomial(link=logit), data=dir)

# Load population data (this data is used in all regression-adjusted estimates)
pop.full <- read.table("population_data_final.txt", header=T)

# Sample from full population data to simplify computation
pop.samp <- pop.full[sample(1:nrow(pop.full), 2000, replace=FALSE),] 

# Create model matrix
pop.full.model.matrix <- cbind(rep(1,nrow(pop.samp)), subset(pop.samp, select=c(dem, gop, maleNA, age55plus, ageNA)))
names(pop.full.model.matrix) <- colnames(model.matrix(dir.logit)) 

predict <- predict(dir.logit, newdata=pop.full.model.matrix, type="response", se.fit=TRUE)

dir.reg.adj <- mean(predict$fit)
dir.reg.adj.se <- mean(predict$se.fit)

dir.statewide.results <- rbind(unweighted=c(1-dir.est, se.dir), weighted=c(1-dir.est.weighted, se.dir.weighted ), regadj=c(1-dir.reg.adj, dir.reg.adj.se))
write.table(dir.statewide.results, "dir-statewide-results.txt")

###############
##   LIST    ##
###############

list <- read.table("list-weight.txt")
list <- subset(list, is.na(list$list.y)==F)

# UNWEIGHTED

# Calculate list experiment difference-in-means
mean.t <- with(list, sum(list.treat*list.y)/sum(list.treat))
mean.c <- with(list, sum(list.control*list.y)/sum(list.control))
list.diff.in.means <- mean.t - mean.c
se <- with(list, sqrt(var(list.treat*list.y)/sum(list.treat) + var(list.control*list.y)/sum(list.control)))
diff.in.means.no.est.unweighted <- cbind(est=1-list.diff.in.means, 
                                         se=se,
                                         low=(1-list.diff.in.means)-qnorm(.975)*se, 
                                         up=(1-list.diff.in.means)+qnorm(.975)*se)
print(diff.in.means.no.est.unweighted)

# Use ictreg() to get posterior probabilities of saying "yes" to sensitive item, using MLE
ml.nocov <- ictreg(list.y ~ 1, 
                   data = list, 
                   treat = "treat", 
                   J=4, 
                   method = "ml",
                   constrained = TRUE,
                   fit.start = "lm")
summary(ml.nocov)
list.est <- mean(ml.nocov$pred.post)
predict(ml.nocov, avg=T, se.fit=T) # Gives equivalent results

# Bootstrap SEs for weighted and unweighted results
n <- 1000 
bootstrap.average.predicted <- rep(NA, n) 
bootstrap.average.predicted.weight <- rep(NA, n)  
counter <- 0

for (j in 1:n){ 
  # taking bootstrap sample from the sample, of same sample size, but with replacement
  bootstrap.sample <- list[sample(nrow(list), nrow(list), replace = TRUE, prob = NULL), ] 
  # ensure all weights sum to 1
  bootstrap.sample$weight <- bootstrap.sample$weight/sum(bootstrap.sample$weight)
  # fitting model   
  bootstrap.fit <- ictreg(list.y ~ 1, 
                          data = bootstrap.sample, 
                          treat = "treat", 
                          J=4, 
                          method = "ml",
                          constrained = TRUE,
                          fit.start = "lm")
  
  bootstrap.average.predicted[j] <- mean(bootstrap.fit$pred.post)
  bootstrap.average.predicted.weight[j] <- sum(bootstrap.fit$pred.post*bootstrap.sample$weight)
  counter <- counter + 1
  if (counter == 1) cat("Iteration: \n", counter) else cat(", ", counter)
}

list.se <- sqrt(var(bootstrap.average.predicted, na.rm = TRUE)) 
list.unweighted <- quantile(1-bootstrap.average.predicted, prob=c(.5, .025, .975))

# WEIGHTED

# Set sum of list weights to 1 for each condition
list$weight <- with(list, ifelse(list.treat==1, list$weight/sum(list$weight[list$list.treat==1]), list$weight/sum(list$weight[list$list.control==1])))

# Calculate list experiment difference-in-means (weighted)
mean.t.weight <- with(list, sum(list.treat*list.y*weight))
mean.c.weight <- with(list, sum(list.control*list.y*weight))
list.diff.in.means.weight <- mean.t.weight - mean.c.weight
se <- with(list, sqrt((sum(list.treat)*var(list.treat*list.y*weight)) + (sum(list.control)*var(list.control*list.y*weight))))
diff.in.means.no.est.weighted <- cbind(est=1-list.diff.in.means.weight, 
                                       se=se,
                                       low=(1-list.diff.in.means.weight)-qnorm(.975)*se, 
                                       up=(1-list.diff.in.means.weight)+qnorm(.975)*se)
print(diff.in.means.no.est.weighted)

# Set sum of list weights to 1
list$weight <- list$weight/sum(list$weight)

# Calculate weighted average for mle with no covariates
list.est.weighted <- sum(ml.nocov$pred.post*list$weight)

# Bootstrap weighted SEs from loop above
list.se.weighted <- sqrt(var(bootstrap.average.predicted.weight, na.rm = TRUE))
list.weighted <- quantile(1-bootstrap.average.predicted.weight, prob=c(.5, .025, .975))

# REGRESSION ADJUSTED

ml <- ictreg(list.y ~ dem
             + gop
             + maleNA
             + ageNA,
             data = list, 
             treat = "treat", 
             J=4, 
             method = "ml",
             constrained = TRUE,
             fit.start = "lm")
summary(ml)

#Predict using population data
predict.ml <- predict(ml, newdata=pop.samp, avg=T, se.fit=T)

list.reg.adj <- predict.ml$fit
list.reg.adj.se <- predict.ml$se.fit

list.statewide.results <- rbind(unweighted=c(1-list.est, list.se), weighted=c(1-list.est.weighted, list.se.weighted ), regadj=c(1-list.reg.adj, list.reg.adj.se))
write.table(list.statewide.results, "list-statewide-results.txt")

######################
##   ENDORSEMENT    ##
######################

end <- read.table("end-weight.txt")

Y <- list(Q1 = c("end.bryant.c", "end.bryant.t"))

# BASELINE MODEL WITH NO COVARIATES 
MCMC <- 2000000
burn <- 200000
thin <- 360

start.value <- rnorm(4, sd = .6)
chains <- 1:4
endorse.out <- vector('list', length(chains))

end.nocov <- subset(end, select=c(end.bryant.c, end.bryant.t, weight))

for (i in chains){
  endorse.out[[i]] <- endorse(Y = Y,
                              data = end.nocov,
                              s.out = TRUE,
                              MCMC = MCMC,
                              burn = burn,
                              thin = thin,
                              prop = .03,
                              identical.lambda = TRUE,
                              covariates = FALSE,
                              hierarchical = FALSE,
                              s0.omega2 = 1,
                              nu0.omega2 = 10,
                              s0.phi2 = 1,
                              nu0.phi2 = 10,
                              s0.psi2 = 1,
                              nu0.psi2 = 10,
                              s0.rho2 = 1,
                              nu0.rho2 = 10,
                              beta.start = matrix(start.value[i], nrow = 1, ncol = 2))
  holder <- endorse.out[[i]]
  save(holder, file=paste("endorse_out_all_", i, ".RData", sep=""))
}

# COMBINE CHAINS

# Intercept only (full sample)
load("endorse_out_all_1.RData")
endorse.out.a <- holder
load("endorse_out_all_2.RData")
endorse.out.b <- holder
load("endorse_out_all_3.RData")
endorse.out.c <- holder
load("endorse_out_all_4.RData")
endorse.out.d <- holder

# Combine second half of all four chains for 10k MCMC draws

endorse.out <-NULL

endorse.out$beta <- rbind(as.matrix(endorse.out.a$beta[2501:5000,]), as.matrix(endorse.out.b$beta[2501:5000,]), 
                          as.matrix(endorse.out.c$beta[2501:5000,]), as.matrix(endorse.out.d$beta[2501:5000,]))

endorse.out$lambda <- rbind(as.matrix(endorse.out.a$lambda[2501:5000,]), as.matrix(endorse.out.b$lambda[2501:5000,]), 
                            as.matrix(endorse.out.c$lambda[2501:5000,]), as.matrix(endorse.out.d$lambda[2501:5000,]))

endorse.out$omega2 <- rbind(as.matrix(endorse.out.a$omega2[2501:5000,]), as.matrix(endorse.out.b$omega2[2501:5000,]), 
                            as.matrix(endorse.out.c$omega2[2501:5000,]), as.matrix(endorse.out.d$omega2[2501:5000,]))

endorse.out$s <- rbind(as.matrix(endorse.out.a$s[2501:5000,]), as.matrix(endorse.out.b$s[2501:5000,]), 
                       as.matrix(endorse.out.c$s[2501:5000,]), as.matrix(endorse.out.d$s[2501:5000,]))

endorse.out$x <- rbind(as.matrix(endorse.out.a$x[2501:5000,]), as.matrix(endorse.out.b$x[2501:5000,]), 
                       as.matrix(endorse.out.c$x[2501:5000,]), as.matrix(endorse.out.d$x[2501:5000,]))

endorse.out$model.matrix.indiv <- endorse.out.a$model.matrix.indiv

# SAVE/RUN LOCALLY
save(endorse.out, file="endorse_out_nocov_combined_chain_final.RData")

## UNWEIGHTED

# Load combined chain
load("endorse_out_nocov_combined_chain_final.RData")

pred <- pnorm(endorse.out$lambda/sqrt(endorse.out$omega2))
end.unweighted <- cbind(mean=mean(pred), low=mean(pred)-qnorm(.975)*sd(pred), up=mean(pred)+qnorm(.975)*sd(pred))

# Same estimate based on posterior distributiion of s, the support parameter
# S matrix is 10,000x1841 (rows are MCMC draws, columns are respondents).
bryant <- endorse.out$s
dim(bryant)

# Find share of negative support parameter in each row, take average for population support estimate 
# This is in-sample from the posterior distribution of the support parameter
# More negative = more support and that's our yes estimate.
bryant.nonzero <- bryant[, colSums(bryant) != 0] 
dim(bryant.nonzero) #leaves 900 obs (number of obs in treatment condition)

# How many negatives in each row? that is, in each MCMC draw
neg <- apply(bryant.nonzero, 1, function(x) length(x[x<0])) #Note: can flip inequality for no est

# Divide by 900 to get share negative in each MCMC draw
share.neg <- neg/ncol(bryant.nonzero)

# Keep unweighted 'no' estimate and CI
end.unweighted <- quantile(1-share.neg, prob=c(.5, .025, .975))

## WEIGHTED

# Using the posterior distribution of s, the support parameter
# 'No' vote estimate is weighted share of positive s_ijk 
# S matrix is 10k x 1841 (full data) 
# rows are MCMC draws, columns are respondents
bryant <- endorse.out$s
dim(bryant)

# Find share of negative support parameter in each row, take average for population support estimate 
# This is in-sample estimate
# More negative = more support and that's our yes estimate.
bryant.nonzero <- bryant[, colSums(bryant) != 0] 
dim(bryant.nonzero) #leaves 900
# Let negatives = 1, positives = 0
bryant.neg1 <- as.matrix(bryant.nonzero)
bryant.neg1[bryant.neg1<0] <- 1
bryant.neg1[bryant.neg1!=1] <- 0
bryant.neg1 <- as.data.frame(bryant.neg1) 

# Create vector of weights for treatment condition only that sum to 1, excluding item nonresponse
end.t.weight <- subset(end, end$treat==1 & is.na(end$end.bryant.t)==F, select=weight)

# Multiply each row by vector of weights
bryant.neg1.weighted <- t(as.matrix(bryant.neg1))*as.vector(t(end.t.weight))
dim(bryant.neg1.weighted)

# Get weighted negative MCMC draws (1x10k)
neg1.weight <- colSums(bryant.neg1.weighted)

# Let negatives = 0, positives = 1
bryant.pos1 <- as.matrix(bryant.nonzero)
bryant.pos1[bryant.pos1<0] <- 0 
bryant.pos1[bryant.pos1!=0] <- 1
bryant.pos1 <- as.data.frame(bryant.pos1) 

# Use vector of weights for treatment condition only, multiply each row by vector of weights
bryant.pos1.weighted <- t(as.matrix(bryant.pos1))*as.vector(t(end.t.weight))
dim(bryant.pos1.weighted)

# Get weighted positive MCMC draws (1x10k)
pos1.weight <- colSums(bryant.pos1.weighted)

# Calculate weighted share of negatives as sum(-)/[sum(+) + sum(-)]
# NOTE: length((neg1.weight/(neg1.weight+pos1.weight))) is 10,000 so we're taking the SD across all posterior draws
draws <- neg1.weight/(neg1.weight+pos1.weight)

# Keep weighted 'no' estimate and CI
end.weighted <- quantile(1-draws, prob=c(.5, .025, .975)) 


## REGRESSION ADJUSTED

MCMC <- 2000000
burn <- 200000
thin <- 360

start.value <- rnorm(4, sd = .6)
cov.formula <- formula( ~ dem + gop + maleNA + age55plus + ageNA)
chains <- 1:4
endorse.out <- vector('list', length(chains))

for (i in chains){
  endorse.out[[i]] <- endorse(Y = Y,
                              data = end,
                              s.out = TRUE,
                              MCMC = MCMC,
                              burn = burn,
                              thin = thin,
                              prop = .03,
                              identical.lambda = TRUE,
                              covariates = TRUE,
                              hierarchical = FALSE,
                              formula.indiv = cov.formula,
                              s0.omega2 = 1,
                              nu0.omega2 = 10, 
                              s0.phi2 = 1,
                              nu0.phi2 = 10,
                              s0.psi2 = 1,
                              nu0.psi2 = 10,
                              s0.rho2 = 1,
                              nu0.rho2 = 10,
                              beta.start = matrix(start.value[i], nrow = 1, ncol = 2))
  holder <- endorse.out[[i]]
  save(holder, file=paste("endorse_out_all_cov_", i, ".RData", sep=""))
}

# COMBINE CHAINS

# With covariates (all observations)
load("endorse_out_all_cov_1.RData")
endorse.out.a <- holder
load("endorse_out_all_cov_2.RData")
endorse.out.b <- holder
load("endorse_out_all_cov_3.RData")
endorse.out.c <- holder
load("endorse_out_all_cov_4.RData")
endorse.out.d <- holder

# Combine second half of all four chains for 10k MCMC draws

endorse.out <-NULL

endorse.out$beta <- rbind(as.matrix(endorse.out.a$beta[2501:5000,]), as.matrix(endorse.out.b$beta[2501:5000,]), 
                          as.matrix(endorse.out.c$beta[2501:5000,]), as.matrix(endorse.out.d$beta[2501:5000,]))

endorse.out$lambda <- rbind(as.matrix(endorse.out.a$lambda[2501:5000,]), as.matrix(endorse.out.b$lambda[2501:5000,]), 
                            as.matrix(endorse.out.c$lambda[2501:5000,]), as.matrix(endorse.out.d$lambda[2501:5000,]))

endorse.out$omega2 <- rbind(as.matrix(endorse.out.a$omega2[2501:5000,]), as.matrix(endorse.out.b$omega2[2501:5000,]), 
                            as.matrix(endorse.out.c$omega2[2501:5000,]), as.matrix(endorse.out.d$omega2[2501:5000,]))

endorse.out$s <- rbind(as.matrix(endorse.out.a$s[2501:5000,]), as.matrix(endorse.out.b$s[2501:5000,]), 
                       as.matrix(endorse.out.c$s[2501:5000,]), as.matrix(endorse.out.d$s[2501:5000,]))

endorse.out$delta <- rbind(as.matrix(endorse.out.a$delta[2501:5000,]), as.matrix(endorse.out.b$delta[2501:5000,]), 
                           as.matrix(endorse.out.c$delta[2501:5000,]), as.matrix(endorse.out.d$delta[2501:5000,]))

endorse.out$x <- rbind(as.matrix(endorse.out.a$x[2501:5000,]), as.matrix(endorse.out.b$x[2501:5000,]), 
                       as.matrix(endorse.out.c$x[2501:5000,]), as.matrix(endorse.out.d$x[2501:5000,]))

endorse.out$model.matrix.indiv <- endorse.out.a$model.matrix.indiv

# SAVE/RUN LOCALLY
save(endorse.out, file="endorse_out_cov_combined_chain_final.RData")
load("endorse_out_cov_combined_chain_final.RData")

#Create model matrix
pop.full.model.matrix <- cbind(rep(1,nrow(pop.samp)), subset(pop.samp, select=c(dem, gop, maleNA, age55plus, ageNA)))
names(pop.full.model.matrix) <- colnames(endorse.out$model.matrix.indiv)
out.of.sample.no.prediction <- colMeans(pnorm(sweep((as.matrix(pop.full.model.matrix)%*%t(endorse.out$lambda)), 2, sqrt(endorse.out$omega2), "/")))
end.reg.adj <- quantile(out.of.sample.no.prediction, probs=c(0.50, 0.025, 0.975))

end.statewide.results <- rbind(end.unweighted, end.weighted, end.reg.adj)
rownames(end.statewide.results) <- c("unweighted", "weighted", "regadj")
write.table(end.statewide.results, "endorse-statewide-results.txt")

##############################
##   RANDOMIZED RESPONSE    ##
##############################

rr <- read.table("rr-weight.txt")

# Forced choice design parameters
p <- .5
p1 <- .5

# UNWEIGHTED

## NOTE: Results shown two ways, but they are similar estimates of true 'no'
## Basic estimator

rr.est <- 1-1/p*(mean(rr$rr.dum) - p1)
rr.se <- 2*sd(rr$rr.dum)/sqrt(length(rr$rr.dum))

## Based on logistic Regression
# Get starting values
fit <- glm(rr.dum ~ 1, data=rr, family=binomial)
start <- fit$coefficients

# Intercept Only
rr.mod <- rrreg(rr.dum ~ 1, data = rr, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")
summary(rr.mod)

# In-sample prediction (unweighted) - based on posterior
rr.pred <- predict(rr.mod, given.y = TRUE, avg = FALSE, quasi.bayes = TRUE)
rr.est <- mean(1-rr.pred$est) #Share 'no'
rr.se <- mean(rr.pred$se)

# WEIGHTED

## Based on Posterior
rr.est.weighted <- 1-sum(rr.pred$est*rr$weight) #Share 'no'

# Bootstrap SEs
n <- 1000 
bootstrap.average.predicted <- rep(NA, n) 
bootstrap.average.predicted.weight <- rep(NA, n) 
counter <- 0

for (j in 1:n){ # loop for bootstrap
  # taking bootstrap sample from the sample, of same sample size, but with replacement
  bootstrap.sample <- rr[sample(nrow(rr), nrow(rr), replace = TRUE, prob = NULL), ] 
  # ensure all weights sum to 1
  bootstrap.sample$weight <- bootstrap.sample$weight/sum(bootstrap.sample$weight)
  # fitting model   
  rr.mod <- rrreg(rr.dum ~ 1, data = bootstrap.sample, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")
  # In-sample prediction (unweighted) - based on posterior
  rr.pred <- predict(rr.mod, given.y = TRUE, avg = FALSE, quasi.bayes = TRUE)
  bootstrap.average.predicted[j] <- mean(1-rr.pred$est) #Share 'no'
  bootstrap.average.predicted.weight[j] <- 1-sum(rr.pred$est*bootstrap.sample$weight)
  counter <- counter + 1
  if (counter == 1) cat("Iteration: \n", counter) else cat(", ", counter)
}

rr.se.weighted <- sqrt(var(bootstrap.average.predicted.weight, na.rm = TRUE)) # calc S.E. from vector of bootstrapped quantity of interest
rr.weighted <- quantile(bootstrap.average.predicted.weight, prob=c(.5, .025, .975))

# REGRESSION ADJUSTED

fit <- glm(rr.dum ~ dem 
           + gop 
           + maleNA 
           + age55plus 
           + ageNA, data=rr, family=binomial)
start <- fit$coefficients

# Full MLE with covariates from the voter file
rr.mod <- rrreg(rr.dum ~ dem 
                + gop 
                + maleNA 
                + age55plus 
                + ageNA, data = rr, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")

pred.ml <- predict(rr.mod, newdata=pop.samp, given.y=FALSE, avg=TRUE, quasi.bayes=TRUE)

rr.reg.adj <- 1-pred.ml$est
rr.reg.adj.se <- pred.ml$se

rr.statewide.results <- rbind(unweighted=c(rr.est, rr.se), weighted=c(rr.est.weighted, rr.se.weighted), regadj=c(rr.reg.adj, rr.reg.adj.se))
write.table(rr.statewide.results, "rrt-statewide-results.txt")

##########################
##       FIGURE 3       ##
##    POOLED RESULTS    ##
##########################

# All obs
dir.state <- read.table("dir-statewide-results.txt")
list.state <- read.table("list-statewide-results.txt")
end.state <- read.table("endorse-statewide-results.txt")
rrt.state <- read.table("rrt-statewide-results.txt")

# Calculate CIs for plot using results tables 
dir.state <- data.frame(dir.state, low=dir.state[,1]-qnorm(.975)*dir.state[,2], up=dir.state[,1]+qnorm(.975)*dir.state[,2])
list.state <- data.frame(list.state, low=list.state[,1]-qnorm(.975)*list.state[,2], up=list.state[,1]+qnorm(.975)*list.state[,2])
end.state <- data.frame(end.state[,1], rep(NA, 3), low=end.state[,2], up=end.state[,3])
rrt.state <- data.frame(rrt.state, low=rrt.state[,1]-qnorm(.975)*rrt.state[,2], up=rrt.state[,1]+qnorm(.975)*rrt.state[,2])

pdf("pooled-results-fig.pdf") 
par(mfrow=c(1,1), las=1, mar=c(0,2,2,3.5), oma=c(2,3,2,0), cex=1) 

x.coords <- c(1.33, 1.66, 1.99, 2.99, 3.33, 3.66, 4.66, 4.99, 5.33, 6.33, 6.66, 6.99)

plot(0:11, seq(from = -2, to = 1.5, length = 12), type='n', xlim=c(1,7.33), ylim=c(0,1), axes=FALSE,
     main="")
axis(2, at=seq(0, 1, .1), las=1, cex.lab=1.2)
abline(h=0.653, lty="dashed")
par(las=0)
mtext("Estimated proportion of 'no' votes on Personhood", outer=TRUE, side=2, cex=1, at=0.5, line=1.5, padj=.1)

est <- c(dir.state[,1], list.state[,1], end.state[,1], rrt.state[,1])

low <- c(dir.state[,3], list.state[,3], end.state[,3], rrt.state[,3])

up <- c(dir.state[,4], list.state[,4], end.state[,4], rrt.state[,4])

segments(x.coords, low, x.coords, up)
points(x.coords, est, pch=rep(c(19,15,17), 3), col=rep(c("black","black","black"), 3), cex=1.5)

text(1.7, .67, "actual vote share", cex=.9, font=3)
mtext("Direct\nQuestion\n(n = 2,655)", outer=TRUE, cex=1, at=0.18, line=-3.5, font=2)
mtext("List\nExperiment\n(n = 1,352)", outer=TRUE, cex=1, at=0.37, line=-3.5, font=2)
mtext("Endorsement\nExperiment\n(n = 1,841)", outer=TRUE, cex=1, at=0.57, line=-3.5, font=2)
mtext("Randomized\nResponse\n(n = 943)", outer=TRUE, cex=1, at=0.78, line=-3.5, font=2)

legend('bottomright', c("Unweighted", "Weighted", "Regression Adjusted"), 
       pch=c(19,15,17), col=c("black","black","black"), bty='n', cex=1)
dev.off()

#############################
##       SECTION 4.3       ##
##  COUNTY-LEVEL RESULTS   ##
#############################

#################
##   DIRECT    ##
#################

#Random Intercepts Model
#With covariates
dir.ml <- glmer(dir ~ dem + gop + maleNA + age55plus + ageNA + (1|county), family=binomial(link=logit), data=dir, control=glmerControl(optimizer="bobyqa"))
summary(dir.ml)
fixcoef <- fixef(dir.ml)
rancoef <- unlist(ranef(dir.ml, condVar=T)$county)

## UNWEIGHTED 

x <- model.matrix(dir.ml) 
# x is 2016 x 6 (individual-level covariates with intercept)
# fixcoef is 6 x 1 (individual-level fixed effects)
# rancoef is 19 x 1 (random intercept for each county)

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=dir$county, x)

#Setup for in-sample prediction loop
n <- 1:19
mean <- NULL
ci.low <- NULL
ci.up <- NULL
counties <- levels(dir$county)
fixcoef.draws <- mvrnorm(10000, fixcoef, vcov(dir.ml))

for (i in n) {
  
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(rancoef[i] + as.matrix(x) %*% t(fixcoef.draws)) /
    (1 + exp(rancoef[i] + as.matrix(x) %*% t(fixcoef.draws)))
  
  # Take average across all obs to get county-level prediction for each MCMC draw (vector of length equal to number of MCMC draws)
  mcmc.pr <- colMeans(pr.yes)           
  
  # Take 50%, 2.5%, and 97.5% across all MCMC draws to get county-level prediction and Bayesian CI
  mean[i] <- quantile(1-mcmc.pr, probs=0.50)
  ci.low[i] <- quantile(1-mcmc.pr, probs=0.025)
  ci.up[i] <- quantile(1-mcmc.pr, probs=0.975)
  dir.unweighted.results <- data.frame(county.pr.no=mean, low=ci.low, up=ci.up)
}

#Store results
dir.unweighted.no.results <- data.frame(county=levels(dir$county), dir.unweighted.results)
write.table(dir.unweighted.no.results, file="dir-unweighted-results-rf.txt")

## WEIGHTED 

for (i in n) {
  
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Create weights that sum to number of obs in each county
  county <- subset(dir, as.numeric(county)==i)
  county$weight <- county$weight/sum(county$weight)*nrow(county)
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(rancoef[i] + as.matrix(x) %*% t(fixcoef.draws)) /
    (1 + exp(rancoef[i] + as.matrix(x) %*% t(fixcoef.draws)))
  
  # Apply county weights to individual-level probabilities
  weighted.pr.yes <- county$weight*pr.yes
  
  # Take average across all obs to get county-level prediction for each MCMC draw (vector of length equal to number of MCMC draws)
  mcmc.pr <- colMeans(weighted.pr.yes)           
  
  # Take 50%, 2.5%, and 97.5% across all MCMC draws to get county-level prediction and Bayesian CI
  mean[i] <- quantile(1-mcmc.pr, probs=0.50)
  ci.low[i] <- quantile(1-mcmc.pr, probs=0.025)
  ci.up[i] <- quantile(1-mcmc.pr, probs=0.975)
  dir.weighted.results <- data.frame(county.pr.no=mean, low=ci.low, up=ci.up)
}

#Store results
dir.weighted.no.results <- data.frame(county=levels(dir$county), dir.weighted.results)
write.table(dir.weighted.no.results, file="dir-weighted-results-rf.txt")

## REG ADJ 

#Setup for out-of-sample prediction loop
n <- 1:19
reg.adj.mean <- NULL
ci.low <-NULL
ci.up <- NULL
counties <- levels(pop.full$county)

for (i in n) {
  # Subset county data
  pop.county <- subset(pop.full, as.numeric(county)==i)
  
  # Sample from full population data for each county
  pop.county <- pop.county[sample(1:nrow(pop.county), 2000, replace=FALSE),] 
  
  # Create model matrix
  pop.county.model.matrix <- cbind(rep(1,nrow(pop.county)), subset(pop.county, select=c(dem, gop, maleNA, age55plus, ageNA)))
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(rancoef[i] + as.matrix(pop.county.model.matrix) %*% t(fixcoef.draws)) /
    (1 + exp(rancoef[i] + as.matrix(pop.county.model.matrix) %*% t(fixcoef.draws)))
  
  # Take average across all obs to get county-level prediction and standard errors
  mcmc.pr <- colMeans(pr.yes)           
  
  # Take 50%, 2.5%, and 97.5% across all MCMC draws to get county-level prediction and Bayesian CI
  reg.adj.mean[i] <- quantile(1-mcmc.pr, probs=0.50)
  ci.low[i] <- quantile(1-mcmc.pr, probs=0.025)
  ci.up[i] <- quantile(1-mcmc.pr, probs=0.975)
  dir.reg.adj.no.results <- data.frame(county.pr.no=reg.adj.mean, low=ci.low, up=ci.up)
}

#Store results
dir.reg.adj.no.results <- data.frame(county=levels(pop.full$county), dir.reg.adj.no.results)
write.table(dir.reg.adj.no.results, file="dir-reg-adj-results-rf.txt")

###############
##   LIST    ##
###############

###
# Bayesian hierarchical  model with county random effects
# Phi control, Sigma sensitive
###

# Create numeric county indicator
list$numeric.county <- as.numeric(list$county)
# Inercept formula
intercept <- formula(~1)
# Overdispersed starting values
draws <- mvrnorm(n = 3, mu = coef(ml), Sigma = vcov(ml) * 9)

n.draws <- 1000000
burn <- 100000
thin <- 180

chains <- 1:3
bayesDraws. <- vector('list', length(chains))

for (i in chains){
  bayesDraws.[[i]] <- ictregBayes(list.y ~ dem
                                  + gop  
                                  + maleNA
                                  + ageNA,
                                  data = list, 
                                  treat = "treat",
                                  J=4,
                                  Sigma.scale = .9, 
                                  Phi.scale = .9, 
                                  delta.start = draws[i, 1:5], 
                                  psi.start = draws[i, 6:10], 
                                  burnin = burn, 
                                  n.draws = n.draws, 
                                  thin = thin,
                                  delta.tune = diag(.006, 5), 
                                  psi.tune = diag(.002, 5),
                                  gamma.tune = rep(.08, 19), #control RF
                                  zeta.tune = rep(.43, 19),  #sensitive RF
                                  group.mixed = "numeric.county", 
                                  formula.mixed = intercept,
                                  sensitive.model = "logit",              
                                  constrained.single = "none")
  holder <- bayesDraws.[[i]]
  save(holder, file=paste("list_out_rf_", i, ".RData", sep=""))
}

# COMBINE CHAINS

# With covariates
load("list_out_rf_1.RData")
bayesDraws.1 <- holder
load("list_out_rf_2.RData")
bayesDraws.2 <- holder
load("list_out_rf_3.RData")
bayesDraws.3 <- holder

# Combine second half of chains
list.out <-NULL

# Create 7,500 x n matrix from all 3 chains
list.out$zeta <- rbind(as.matrix(bayesDraws.1$zeta[2293:4792,]), as.matrix(bayesDraws.2$zeta[2293:4792,]), 
                       as.matrix(bayesDraws.3$zeta[2293:4792,]))
list.out$gamma <- rbind(as.matrix(bayesDraws.1$gamma[2293:4792,]), as.matrix(bayesDraws.2$gamma[2293:4792,]), 
                        as.matrix(bayesDraws.3$gamma[2293:4792,]))
list.out$delta <- rbind(as.matrix(bayesDraws.1$delta[2293:4792,]), as.matrix(bayesDraws.2$delta[2293:4792,]), 
                        as.matrix(bayesDraws.3$delta[2293:4792,]))
list.out$psi <- rbind(as.matrix(bayesDraws.1$psi[2293:4792,]), as.matrix(bayesDraws.2$psi[2293:4792,]), 
                      as.matrix(bayesDraws.3$psi[2293:4792,]))

list.out$x <- bayesDraws.1$x

save(list.out, file="list_out_rf_combined_chain_final.RData")
load("list_out_rf_combined_chain_final.RData")

## UNWEIGHTED 

# Note:
# x is 1325 x 5 (individual-level covariates with intercept)
# delta is 7500 x 5 (individual-level fixed effects)
# zeta is 7500 x 19 (random intercept for each county)

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=list$county, list.out$x)

n <- 1:19
county.pr.no <- NULL
county.pr.no.low <- NULL
county.pr.no.hi <- NULL

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(list.out$zeta[,i] + as.matrix(x) %*% t(list.out$delta)) /
    (1 + exp(list.out$zeta[,i] + as.matrix(x) %*% t(list.out$delta)))
  
  # Take average across all obs to get county-level prediction for each MCMC draw (vector of length equal to number of MCMC draws)
  mcmc.pr <- colMeans(pr.yes)           
  
  # Take 50%, 2.5%, and 97.5% across all MCMC draws to get county-level prediction and Bayesian CI
  county.pr.no[i] <- quantile(1-mcmc.pr, probs=0.50)
  county.pr.no.low[i] <- quantile(1-mcmc.pr, probs=0.025)
  county.pr.no.hi[i] <- quantile(1-mcmc.pr, probs=0.975)
  list.unweighted.county.results.pr.no <- data.frame(county.pr.no=county.pr.no, low=county.pr.no.low, hi=county.pr.no.hi)
}

#Store results
list.unweighted.results.rf <- data.frame(county=levels(list$county), list.unweighted.county.results.pr.no)
write.table(list.unweighted.results.rf, file="list-unweighted-results-rf.txt")

## WEIGHTED 

x.all <- data.frame(county=list$county, list.out$x)

n <- 1:19
county.pr.no <- NULL
county.pr.no.low <- NULL
county.pr.no.hi <- NULL

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Create weights that sum to number of obs in each county
  county <- subset(list, as.numeric(county)==i)
  county$weight <- county$weight/sum(county$weight)*nrow(county)
  #length(county$weight) #number of observations in county long
  #sum(county$weight) #sums to number of observations in county
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(list.out$zeta[,i] + as.matrix(x) %*% t(list.out$delta)) /
    (1 + exp(list.out$zeta[,i] + as.matrix(x) %*% t(list.out$delta)))
  
  # Apply county weights to individual-level probabilities
  weighted.pr.yes <- county$weight*pr.yes
  
  # Take average across all obs to get county-level prediction for each MCMC draw (vector of length equal to number of MCMC draws)
  mcmc.pr <- colMeans(weighted.pr.yes)           
  
  # Take 50%, 2.5%, and 97.5% across all MCMC draws to get county-level prediction and Bayesian CI
  county.pr.no[i] <- quantile(1-mcmc.pr, probs=0.50)
  county.pr.no.low[i] <- quantile(1-mcmc.pr, probs=0.025)
  county.pr.no.hi[i] <- quantile(1-mcmc.pr, probs=0.975)
  list.weighted.county.results.pr.no <- data.frame(county.pr.no=county.pr.no, low=county.pr.no.low, hi=county.pr.no.hi)
}

#Store results
list.weighted.results.rf <- data.frame(county=levels(list$county), list.weighted.county.results.pr.no)
write.table(list.weighted.results.rf, file="list-weighted-results-rf.txt")

## REG ADJ 

#Setup for out-of-sample prediction loop
n <- 1:19
reg.adj.mean <- NULL
reg.adj.low <- NULL
reg.adj.hi <- NULL
counties <- levels(pop.full$county)

for (i in n) {
  # Subset county data
  pop.county <- subset(pop.full, as.numeric(county)==i)
  
  # Sample from full population data for each county
  pop.county <- pop.county[sample(1:nrow(pop.county), 2000, replace=FALSE),] 
  
  # Create model matrix
  pop.county.model.matrix <- cbind(rep(1,nrow(pop.county)), subset(pop.county, select=c(dem, gop, maleNA, ageNA)))
  names(pop.county.model.matrix) <- colnames(list.out$x)
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(list.out$zeta[,i] + as.matrix(pop.county.model.matrix) %*% t(list.out$delta)) /
    (1 + exp(list.out$zeta[,i] + as.matrix(pop.county.model.matrix) %*% t(list.out$delta)))
  
  # Take average across all obs to get county-level prediction for each MCMC draw (vector of length equal to number of MCMC draws)
  mcmc.pr <- colMeans(pr.yes)   
  
  # Take 50%, 2.5%, and 97.5% across all MCMC draws to get county-level prediction and Bayesian CI
  reg.adj.mean[i] <- quantile(1-mcmc.pr, probs=0.50)
  reg.adj.low[i] <- quantile(1-mcmc.pr, probs=0.025)
  reg.adj.hi[i] <- quantile(1-mcmc.pr, probs=0.975)
  list.reg.adj.no.predict.results <- data.frame(county.pr.no=reg.adj.mean, low=reg.adj.low, hi=reg.adj.hi)
}

#Store results
list.reg.adj.no.predict.results <- data.frame(county=counties, list.reg.adj.no.predict.results)
write.table(list.reg.adj.no.predict.results, file="list-reg-adj-results-rf.txt")

## Also calculate county-level estimates using diff-in-means 
mean.t <- NULL
mean.c <- NULL
list.diff.in.means <- NULL

for (i in levels(list$county)){
  county <- subset(list, county == i)
  mean.t[i] <- sum(county$list.treat*county$list.y)/sum(county$list.treat)
  mean.c[i] <- sum(county$list.control*county$list.y)/(nrow(county)-sum(county$list.treat))
  list.diff.in.means[i] <- 1 - (mean.t[i] - mean.c[i])
}

## Compute county-level correlation between diff-in-means estimator and actual election result 
load("ms-g11-county-vote-data.RData")
vote <- subset(vote, insample==1)
cor.test(list.diff.in.means, vote$no26)

######################
##   ENDORSEMENT    ##
######################

## Hierarchical model with covariates
MCMC <- 2000000
burn <- 200000
thin <- 360

start.value <- rnorm(4, sd = .6)

chains <- 1:4
endorse.out <- vector('list', length(chains))

for (i in chains){
  endorse.out[[i]] <- endorse(Y = Y,
                              data = end,
                              s.out = TRUE,
                              MCMC = MCMC,
                              burn = burn,
                              thin = thin,
                              prop = .03,
                              identical.lambda = TRUE,
                              covariates = TRUE,
                              hierarchical = TRUE,
                              village = "county",
                              data.village = data.frame(levels(end$county)),
                              formula.village = formula( ~ 1),
                              formula.indiv = cov.formula,
                              s0.omega2 = 1,
                              nu0.omega2 = 10, 
                              s0.phi2 = 1,
                              nu0.phi2 = 10,
                              s0.psi2 = 1,
                              nu0.psi2 = 10,
                              s0.rho2 = 1,
                              nu0.rho2 = 10,
                              beta.start = matrix(start.value[i], nrow = 1, ncol = 2))
  holder <- endorse.out[[i]]
  save(holder, file=paste("endorse_out_all_rf_", i, ".RData", sep=""))
}

# COMBINE CHAINS

# With covariates and random intercepts for each county (all observations)
load("endorse_out_all_rf_1.RData")
endorse.out.a <- holder
load("endorse_out_all_rf_2.RData")
endorse.out.b <- holder
load("endorse_out_all_rf_3.RData")
endorse.out.c <- holder
load("endorse_out_all_rf_4.RData")
endorse.out.d <- holder

# Combine second half of all four chains for 10k MCMC draws
# similar to output object from a single chain run using endorse()

endorse.out <-NULL

endorse.out$beta <- rbind(as.matrix(endorse.out.a$beta[2501:5000,]), as.matrix(endorse.out.b$beta[2501:5000,]), 
                          as.matrix(endorse.out.c$beta[2501:5000,]), as.matrix(endorse.out.d$beta[2501:5000,]))

endorse.out$lambda <- rbind(as.matrix(endorse.out.a$lambda[2501:5000,]), as.matrix(endorse.out.b$lambda[2501:5000,]), 
                            as.matrix(endorse.out.c$lambda[2501:5000,]), as.matrix(endorse.out.d$lambda[2501:5000,]))

endorse.out$omega2 <- rbind(as.matrix(endorse.out.a$omega2[2501:5000,]), as.matrix(endorse.out.b$omega2[2501:5000,]), 
                            as.matrix(endorse.out.c$omega2[2501:5000,]), as.matrix(endorse.out.d$omega2[2501:5000,]))

endorse.out$s <- rbind(as.matrix(endorse.out.a$s[2501:5000,]), as.matrix(endorse.out.b$s[2501:5000,]), 
                       as.matrix(endorse.out.c$s[2501:5000,]), as.matrix(endorse.out.d$s[2501:5000,]))

endorse.out$delta <- rbind(as.matrix(endorse.out.a$delta[2501:5000,]), as.matrix(endorse.out.b$delta[2501:5000,]), 
                           as.matrix(endorse.out.c$delta[2501:5000,]), as.matrix(endorse.out.d$delta[2501:5000,]))

endorse.out$x <- rbind(as.matrix(endorse.out.a$x[2501:5000,]), as.matrix(endorse.out.b$x[2501:5000,]), 
                       as.matrix(endorse.out.c$x[2501:5000,]), as.matrix(endorse.out.d$x[2501:5000,]))

endorse.out$model.matrix.indiv <- endorse.out.a$model.matrix.indiv

save(endorse.out, file="endorse_out_rf_combined_chain_final.RData")
load("endorse_out_rf_combined_chain_final.RData")

## UNWEIGHTED 

# Predicted probability of 'NO' vote based on combined chain

# Setup for in-sample prediction loop
n <- 1:19
end.rf.unweighted.mean <- NULL
end.rf.unweighted.lowci <- NULL
end.rf.unweighted.upci <-NULL
counties <- levels(pop.full$county)

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=end$county, endorse.out$model.matrix.indiv)

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  names(x) <- colnames(endorse.out$model.matrix.indiv) 
  
  # Use lambda for each county and covariates from sample in that county to make in-sample predictions 
  # Extract group coefficient and individual-level coefficients
  lambda <- endorse.out$lambda[,c(i,20:24)]
  
  # Predict on data from each county
  in.sample.no.prediction <- colMeans(pnorm(sweep((as.matrix(x) %*% t(lambda)), 2, sqrt(endorse.out$omega2), "/")))
  
  end.rf.unweighted.mean[i] <- quantile(in.sample.no.prediction, probs=0.50)
  end.rf.unweighted.lowci[i] <- quantile(in.sample.no.prediction, probs=0.025)
  end.rf.unweighted.upci[i] <- quantile(in.sample.no.prediction, probs=0.975)
  end.in.sample.no.predict.results <- data.frame("endorsement unweighted"=end.rf.unweighted.mean, "Low CI"=end.rf.unweighted.lowci, "Up CI"=end.rf.unweighted.upci)
}

end.unweighted.results <- data.frame(county=levels(end$county), end.in.sample.no.predict.results)
write.table(end.unweighted.results, file="endorse-unweighted-results-rf.txt")

## WEIGHTED 

# Predicted probability of 'NO' vote based on combined chain

# Setup for in-sample prediction loop
n <- 1:19
end.rf.weighted.mean <- NULL
end.rf.weighted.sd <- NULL
end.rf.weighted.lowci <- NULL
end.rf.weighted.upci <-NULL
counties <- levels(end$county)

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=end$county, endorse.out$model.matrix.indiv)

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  names(x) <- colnames(endorse.out$model.matrix.indiv) 
  
  # Use lambda for each county and covariates from sample in that county to make in-sample predictions 
  # Extract group coefficient and individual-level coefficients
  lambda <- endorse.out$lambda[,c(i,20:24)]
  
  # Create weights that sum to number of obs in each county
  county <- subset(end, as.numeric(county)==i)
  county$weight <- county$weight/sum(county$weight)*nrow(county)
  #length(county$weight) #number of observations in county long
  #sum(county$weight) #sums to number of observations in county
  
  #Predict on data from each county
  in.sample.no.prediction <- colMeans(sweep(pnorm(sweep((as.matrix(x) %*% t(lambda)), 2, sqrt(endorse.out$omega2), "/")), 1, county$weight, "*"))
  end.rf.weighted.mean[i] <- quantile(in.sample.no.prediction, probs=0.50)
  end.rf.weighted.lowci[i] <- quantile(in.sample.no.prediction, probs=0.025)
  end.rf.weighted.upci[i] <- quantile(in.sample.no.prediction, probs=0.975)
  end.in.sample.no.predict.results.weighted <- data.frame("endorsement weighted"=end.rf.weighted.mean, "Low CI"=end.rf.weighted.lowci, "Up CI"=end.rf.weighted.upci)
  
}

end.weighted.results <- data.frame(county=counties, end.in.sample.no.predict.results.weighted)
write.table(end.weighted.results, file="endorse-weighted-results-rf.txt")

## REG ADJ 

#Setup for out-of-sample prediction loop
n <- 1:19
reg.adj.mean <- NULL
reg.adj.lowci <- NULL
reg.adj.upci <-NULL
counties <- levels(end$county)

for (i in n) {
  # Subset county data
  pop.county <- subset(pop.full, as.numeric(county)==i)
  # Sample from full population data for each county
  pop.county <- pop.county[sample(1:nrow(pop.county), 2000, replace=FALSE),] 
  # Create model matrix
  pop.county.model.matrix <- cbind(rep(1,nrow(pop.county)), subset(pop.county, select=c(dem, gop, maleNA, age55plus, ageNA)))
  names(pop.county.model.matrix) <- colnames(endorse.out$model.matrix.indiv) 
  
  #Use lambda for each county and covariates from population of that county to make out-of-sample-predictions 
  #Extract group coefficient and individual-level coefficients
  lambda <- endorse.out$lambda[,c(i,20:24)]
  
  #Predict on data from each county
  out.of.sample.no.prediction <- colMeans(pnorm(sweep((as.matrix(pop.county.model.matrix)%*%t(lambda)), 2, sqrt(endorse.out$omega2), "/")))
  
  reg.adj.mean[i] <- quantile(out.of.sample.no.prediction, probs=0.50)
  reg.adj.lowci[i] <- quantile(out.of.sample.no.prediction, probs=0.025)
  reg.adj.upci[i] <- quantile(out.of.sample.no.prediction, probs=0.975)
  end.out.of.sample.no.predict.results <- data.frame(reg.adj.mean, reg.adj.lowci, reg.adj.upci)
}
end.reg.adj.results <- data.frame(counties, end.out.of.sample.no.predict.results)
write.table(end.reg.adj.results, file="endorse-reg-adj-results-rf.txt")

##############################
##   RANDOMIZED RESPONSE    ##
##############################

# Starting values for MLE
fit <- glm(rr.dum ~ 
             gop 
           + maleNA 
           + age55plus 
           + ageNA, data=rr, family=binomial)
start <- fit$coefficients

# Full MLE with covariates from the voter file
# Use these to obtain starting values for beta in mixed effects model below
rr.mod <- rrreg(rr.dum ~ 
                  gop 
                + maleNA 
                + age55plus 
                + ageNA, data = rr, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")


# Create numeric county indicator
rr$numeric.county <- as.numeric(rr$county)

# Intercept formula
intercept <- formula(~ 1)

# Set number of chains
chains <- 1:4

# Holder for chains
rr.out <- vector('list', length(chains))

# Iterations
MCMC <- 2000000
burn <- 200000
thin <- 360

# Test run
#MCMC <- 11000
#burn <- 1000
#thin <- 10

# Dispersed starting values
beta.start <- mvrnorm(n = length(chains), mu = coef(rr.mod), Sigma = vcov(rr.mod)*4) 

start <- Sys.time()
print(start)

for (i in chains){
  rr.out[[i]] <- rrreg.bayes(rr.dum ~ gop + maleNA + age55plus + ageNA,
                             p = p, 
                             p1 = p1,
                             p0 = 0,
                             design = "forced-known", 
                             data = rr, 
                             group.mixed = "numeric.county", 
                             formula.mixed = intercept,
                             verbose = T, 
                             n.draws = MCMC, 
                             burnin = burn, 
                             thin = thin,
                             beta.start = beta.start[i,], ## Starting with draws from mvrnorm 
                             beta.mu0 = rep(0, 5),  ## Prior mean of 0 for all betas
                             beta.A0 = diag(5)*0.01, ## Prior precision for all betas, previously .001
                             beta.tune = diag(5)*0.08, ## Tuning parameters for beta 
                             #Psi = 10, ## Starting value for intercept parameter in mixed effects model 
                             Psi.df = 21, ## Prior degrees of freedom for mixed effects model 
                             Psi.scale = 1,  ## Prior scale parameter for mixed effects model 
                             Psi.tune = rep(.6, length(unique(rr$numeric.county)))) ## Tuning parameter for psi
}

stop <- Sys.time()
print(stop)
print(stop-start)

save(rr.out, file="rr_out_rf_all.RData")
#load("rr_out_rf_all.RData")

# List of all chains for summary output
rr.chains <- as.list(rr.out[[1]], rr.out[[2]], rr.out[[3]], rr.out[[4]])
summary(rr.chains)

# Save combined chain
# Keeping only second half of each chain
draws <- seq(2501, 5000, 1)
rrout <-NULL

rrout$beta <- rbind(as.matrix(rr.out[[1]]$beta[draws,]), as.matrix(rr.out[[2]]$beta[draws,]), 
                    as.matrix(rr.out[[3]]$beta[draws,]), as.matrix(rr.out[[4]]$beta[draws,]))
rrout$gamma <- rbind(as.matrix(rr.out[[1]]$gamma[draws,]), as.matrix(rr.out[[2]]$gamma[draws,]), 
                     as.matrix(rr.out[[3]]$gamma[draws,]), as.matrix(rr.out[[4]]$gamma[draws,]))
rrout$Psi <- rbind(as.matrix(rr.out[[1]]$Psi[draws,]), as.matrix(rr.out[[2]]$Psi[draws,]), 
                   as.matrix(rr.out[[3]]$Psi[draws,]), as.matrix(rr.out[[4]]$Psi[draws,]))
rrout$x <- rr.out[[1]]$x

tmp <- rr.out
rr.out <- rrout

save(rr.out, file="rr_out_rf_combined_chain.RData")

# Load combined chain for analysis
load("rr_out_rf_combined_chain.RData")

#Predicted probability of 'NO' vote based on combined chain
#Model with covariates and random effects for each county
dim(rr.out$beta) #10k x 5
dim(rr.out$gamma) #10k x 19
dim(rr.out$Psi) #10k x 1
dim(rr.out$x) #818 x 5

## Unweighted 

#Setup for in-sample prediction loop
n <- 1:19
rr.rf.mean <- NULL
rr.rf.lowci <- NULL
rr.rf.upci <-NULL
counties <- levels(rr$county)

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=rr$county, rr.out$x)

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(rr.out$gamma[,i] + as.matrix(x) %*% t(rr.out$beta)) /
    (1 + exp(rr.out$gamma[,i] + as.matrix(x) %*% t(rr.out$beta)))
  
  # Take average across all obs to get county-level prediction for each MCMC draw (vector of length equal to number of MCMC draws)
  mcmc.pr <- colMeans(pr.yes)           
  
  rr.rf.mean[i] <- quantile(1-mcmc.pr, probs=0.50)
  rr.rf.lowci[i] <- quantile(1-mcmc.pr, probs=0.025)
  rr.rf.upci[i] <- quantile(1-mcmc.pr, probs=0.975)
  rr.in.sample.no.results <- data.frame("rr unweighted"=rr.rf.mean, "Low CI"=rr.rf.lowci, "Up CI"=rr.rf.upci)
}

rr.unweighted.results <- data.frame(county=levels(rr$county), rr.in.sample.no.results)
write.table(rr.unweighted.results, file="rrt-unweighted-results-rf.txt")

## Weighted 

#Setup for in-sample prediction loop
n <- 1:19
rr.rf.mean <- NULL
rr.rf.lowci <- NULL
rr.rf.upci <-NULL
counties <- levels(rr$county)

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=rr$county, rr.out$x)

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Create weights that sum to number of obs in each county
  county <- subset(rr, as.numeric(county)==i)
  county$weight <- county$weight/sum(county$weight)*nrow(county)
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(rr.out$gamma[,i] + as.matrix(x) %*% t(rr.out$beta)) /
    (1 + exp(rr.out$gamma[,i] + as.matrix(x) %*% t(rr.out$beta)))
  
  # Apply county weights to individual-level probabilities
  weighted.pr.yes <- county$weight*pr.yes
  
  # Take average across all obs to get county-level prediction for each MCMC draw (vector of length equal to number of MCMC draws)
  mcmc.pr <- colMeans(weighted.pr.yes)           
  
  rr.rf.mean[i] <- quantile(1-mcmc.pr, probs=0.50)
  rr.rf.lowci[i] <- quantile(1-mcmc.pr, probs=0.025)
  rr.rf.upci[i] <- quantile(1-mcmc.pr, probs=0.975)
  rr.in.sample.no.results <- data.frame("rr weighted"=rr.rf.mean, "Low CI"=rr.rf.lowci, "Up CI"=rr.rf.upci)
}

rr.weighted.results <- data.frame(county=levels(rr$county), rr.in.sample.no.results)
write.table(rr.weighted.results, file="rrt-weighted-results-rf.txt")

## REG ADJ 

#Setup for out-of-sample prediction loop
n <- 1:19
rr.rf.mean <- NULL
rr.rf.lowci <- NULL
rr.rf.upci <-NULL
counties <- levels(rr$county)

for (i in n) {
  # Subset county data
  pop.county <- subset(pop.full, as.numeric(county)==i)
  
  # Sample from full population data for each county
  pop.county <- pop.county[sample(1:nrow(pop.county), 2000, replace=FALSE),] 
  
  # Create model matrix
  pop.county.model.matrix <- cbind(rep(1,nrow(pop.county)), subset(pop.county, select=c(gop, maleNA, age55plus, ageNA)))
  names(pop.county.model.matrix) <- colnames(rr.out$x)
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  pr.yes <- exp(rr.out$gamma[,i] + as.matrix(pop.county.model.matrix) %*% t(rr.out$beta)) /
    (1 + exp(rr.out$gamma[,i] + as.matrix(pop.county.model.matrix) %*% t(rr.out$beta)))
  
  # Take average across all obs to get county-level prediction for each MCMC draw (vector of length equal to number of MCMC draws)
  mcmc.pr <- colMeans(pr.yes)   
  
  # Take mean and sd across all MCMC draws to get county-level prediction and standard errors
  rr.rf.mean[i] <- quantile(1-mcmc.pr, probs=0.50)
  rr.rf.lowci[i] <- quantile(1-mcmc.pr, probs=0.025)
  rr.rf.upci[i] <- quantile(1-mcmc.pr, probs=0.975)
  rr.out.sample.no.results <- data.frame("rr reg adj"=rr.rf.mean, "Low CI"=rr.rf.lowci, "Up CI"=rr.rf.upci)
}

#Store results
rr.reg.adj.no.results <- data.frame(county=levels(rr$county), rr.out.sample.no.results)
write.table(rr.reg.adj.no.results, file="rrt-reg-adj-results-rf.txt")


##########################
##       FIGURE 4       ##
## COUNTY-LEVEL RESULTS ##
##      NO POOLING      ##
##########################

#Read in county-level voting results 
load("ms-g11-county-vote-data.RData")
vote <- subset(vote, insample==1)

#Read in results - using all obs
#Unweighted
direct.unweighted.results <- read.table("dir-unweighted-results-rf.txt", header=T)
list.results <- read.table("list-unweighted-results-rf.txt")
end.unweighted.results <- read.table("endorse-unweighted-results-rf.txt")
rrt.results <- read.table("rrt-unweighted-results-rf.txt", header=T)

#Weighted
direct.weighted.results <- read.table("dir-weighted-results-rf.txt", header=T)
list.weighted.results <- read.table("list-weighted-results-rf.txt")
end.weighted.results <- read.table("endorse-weighted-results-rf.txt")
rrt.results.weighted <- read.table("rrt-weighted-results-rf.txt", header=T)

#Regression Adjusted
dir.reg.adj.results <- read.table("dir-reg-adj-results-rf.txt")
end.reg.adj.results <- read.table("endorse-reg-adj-results-rf.txt")
list.reg.adj.results <- read.table("list-reg-adj-results-rf.txt")
rrt.reg.adj.results <- read.table("rrt-reg-adj-results-rf.txt")

#Calculate RMSE
results <- list(direct.unweighted.results,
                list.results,
                end.unweighted.results,
                rrt.results,
                direct.weighted.results,
                list.weighted.results,
                end.weighted.results,
                rrt.results.weighted,
                dir.reg.adj.results,
                list.reg.adj.results,
                end.reg.adj.results,
                rrt.reg.adj.results)
approach <- c("direct.unweighted",
              "list.unweighted",
              "end.unweighted",
              "rrt.unweighted",
              "direct.weighted",
              "list.weighted",
              "end.weighted",
              "rrt.weighted",
              "dir.reg.adj",
              "list.reg.adj",
              "end.reg.adj",
              "rrt.reg.adj.results")
rmse <- NULL
for (i in 1:12) {
  print(i)
  rmse[i] <- sqrt(mean((results[[i]][,2] - vote$no26)^2))
}
rmse[3] <- sqrt(mean((results[[3]][,2] - vote$no26)^2))

#Calculate bias
bias <- NULL
for (i in 1:12) {
  bias[i] <- mean(results[[i]][,2]) - mean(vote$no26) 
}

#Calculate correlation
cor <- NULL
for (i in 1:12) {
  cor[i] <- cor(vote$no26, results[[i]][,2])
  print(cor.test(vote$no26, results[[i]][,2]))
}
tab <- data.frame(approach, rmse, bias, cor)
print(tab)

#Open plot
pdf("county-fig.pdf", height = 5.5, width = 8.5) 

par(mfrow=c(3,4),
    las = 1, 
    mar=c(3.3,3.5,.4,.3), 
    oma=c(1,8.5,3,.5),
    mgp=c(1.8,.6,0),
    tck=-.05)

truth <- vote$no26
pch <- c( rep(19,4), rep(15,4), rep(17,4) )

for (i in 1:length(results)){
  est <- results[[i]][,2]
  low.est <- results[[i]][,3]
  up.est <- results[[i]][,4]
  plot(truth, est, type = "p", ylab = "Estimate", xlab = "Actual", pch = pch[i], cex = 1,
       main="",
       cex.axis=.7,
       xlim=c(0,1),
       ylim=c(0,1))
  abline(0,1, col="red")
  segments(truth, low.est, truth, up.est, lwd =  0.5)
  text(0,.9, paste("bias = ", round(tab[i,3], 3), "\nRMSE = ", round(tab[i,2] ,3), sep=""), cex=.7, adj=0)
}

mtext("Unweighted", side = 2, line = 1.5, outer = TRUE, at=.88, cex=.8, font=2) 
mtext("Weighted", side = 2, line = 1.5, outer = TRUE, at=.54, cex=.8, font=2) 
mtext("Regression\nAdjusted", side = 2, line = 1.5, outer = TRUE, at=.20, cex=.8, font=2)

mtext("Direct Question", side = 3, line = .6, outer = TRUE, at=.156, cex=.8, font=2) 
mtext("List Experiment", side = 3, line = .6, outer = TRUE, at=.41, cex=.8, font=2) 
mtext("Endorsement Experiment", side = 3, line = .6, outer = TRUE, at=.65, cex=.8, font=2)
mtext("Randomized Response", side = 3, line = .6, outer = TRUE, at=.9, cex=.8, font=2)
dev.off()

####################################################
##                   SECTION 4.3                  ##
##           COUNTY-LEVEL RESULTS (CON'T)         ##
##                                                ##
##               STATEWIDE ESTIMATES              ##
## BASED ON AGGREGATION OF COUNTY-LEVEL ESTIMATES ##
####################################################

#################
##   DIRECT    ##
#################

## UNWEIGHTED 

# Create individual-level matrix of covariates for all observations with a county identifier
x <- model.matrix(dir.ml) 
# Note:
# x is 2016 x 6 (individual-level covariates with intercept)
# fixcoef is 6 x 1 (individual-level fixed effects)
# rancoef is 19 x 1 (random intercept for each county)
x.all <- data.frame(county=dir$county, x)

# Draw FEs from multivariate normal distribution
fixcoef.draws <- mvrnorm(10000, fixcoef, vcov(dir.ml))

# Get draws for each county and store them
n <- 1:19
draws <- vector('list', length(n))

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  draws[[i]] <- exp(rancoef[i] + as.matrix(x) %*% t(fixcoef.draws)) /
    (1 + exp(rancoef[i] + as.matrix(x) %*% t(fixcoef.draws)))
  
}

# Create matrix of draws
draws <- do.call("rbind", draws)

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(draws)

# Use all MCMC draws to get 'no' prediction and CI
dir.unweighted.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))

## WEIGHTED 

#Create weight which sums to number of obs
dir$weight <- dir$weight*nrow(dir)

#Calculate weighted draws
weighted.draws <- draws*dir$weight

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(weighted.draws)

# Use all MCMC draws to get 'no' prediction and CI
dir.weighted.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))

## REG ADJ 

#Setup for out-of-sample prediction loop
draws.pop <- vector('list', length(n))

for (i in n) {
  # Subset county data
  pop.county <- subset(pop.full, as.numeric(county)==i)
  
  # Sample from full population data for each county
  pop.county <- pop.county[sample(1:nrow(pop.county), ceiling(20000*nrow(pop.county)/nrow(pop.full)), replace=FALSE),] 
  
  # Create model matrix
  pop.county.model.matrix <- cbind(rep(1,nrow(pop.county)), subset(pop.county, select=c(dem, gop, maleNA, age55plus, ageNA)))
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  draws.pop[[i]] <- exp(rancoef[i] + as.matrix(pop.county.model.matrix) %*% t(fixcoef.draws)) /
    (1 + exp(rancoef[i] + as.matrix(pop.county.model.matrix) %*% t(fixcoef.draws)))
  
}

# Create matrix of draws
draws.pop <- do.call("rbind", draws.pop)

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(draws.pop)

# Use all MCMC draws to get 'no' prediction and CI
dir.reg.adj.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))

dir.statewide.results <- rbind(dir.unweighted.all.no, dir.weighted.all.no, dir.reg.adj.all.no)
write.table(dir.statewide.results, "dir-statewide-results-rf.txt")

###############
##   LIST    ##
###############

## UNWEIGHTED 

# Note:
# x is 1325 x 5 (individual-level covariates with intercept)
# delta is 7500 x 5 (individual-level fixed effects)
# zeta is 7500 x 19 (random intercept for each county)

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=list$county, list.out$x)

# Get draws for each county and store them
n <- 1:19
draws <- vector('list', length(n))
for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  draws[[i]] <- exp(list.out$zeta[,i] + as.matrix(x) %*% t(list.out$delta)) /
    (1 + exp(list.out$zeta[,i] + as.matrix(x) %*% t(list.out$delta)))
}

# Create matrix of draws
draws <- do.call("rbind", draws)

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(draws)

# Use all MCMC draws to get 'no' prediction and CI
list.unweighted.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))

## WEIGHTED 

#Create weight which sums to number of obs
list$weight <- list$weight/sum(list$weight)*nrow(list)
sum(list$weight)

#Calculate weighted draws
weighted.draws <- draws*list$weight

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(weighted.draws)

# Use all MCMC draws to get weighted 'no' prediction and CI
list.weighted.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))

## REG ADJ 

#Setup for out-of-sample prediction loop
draws.pop <- vector('list', length(n))

for (i in n) {
  # Subset county data
  pop.county <- subset(pop.full, as.numeric(county)==i)
  
  # Sample from full population data for each county
  pop.county <- pop.county[sample(1:nrow(pop.county), ceiling(20000*nrow(pop.county)/nrow(pop.full)), replace=FALSE),] 
  
  # Create model matrix
  pop.county.model.matrix <- cbind(rep(1,nrow(pop.county)), subset(pop.county, select=c(dem, gop, maleNA, ageNA)))
  names(pop.county.model.matrix) <- colnames(list.out$x)
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  draws.pop[[i]] <- exp(list.out$zeta[,i] + as.matrix(pop.county.model.matrix) %*% t(list.out$delta)) /
    (1 + exp(list.out$zeta[,i] + as.matrix(pop.county.model.matrix) %*% t(list.out$delta)))
}

# Create matrix of draws
draws.pop <- do.call("rbind", draws.pop)

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(draws.pop)

# Use all MCMC draws to get 'no' prediction and CI
list.reg.adj.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))

list.statewide.results <- rbind(list.unweighted.all.no, list.weighted.all.no, list.reg.adj.all.no)
write.table(list.statewide.results, "list-statewide-results-rf.txt")


######################
##   ENDORSEMENT    ##
######################

## UNWEIGHTED 

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=end$county, endorse.out$model.matrix.indiv)

# Get draws for each county and store them
n <- 1:19
draws <- vector('list', length(n))

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  names(x) <- colnames(endorse.out$model.matrix.indiv)
  
  # Use lambda for each county and covariates from sample in that county to make in-sample predictions 
  # Extract group coefficient and individual-level coefficients
  lambda <- endorse.out$lambda[,c(i,20:24)]
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  draws[[i]] <- pnorm(sweep((as.matrix(x) %*% t(lambda)), 2, sqrt(endorse.out$omega2), "/"))
}

# Create matrix of draws
draws <- do.call("rbind", draws)

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(draws)

# Use all MCMC draws to get 'no' prediction and CI
end.unweighted.all.no <- quantile((mcmc.pr), probs=c(0.50, 0.025, 0.975))


## WEIGHTED 

#Create weight which sums to number of obs
end$weight <- end$weight/sum(end$weight)*nrow(end)

#Calculate weighted draws
weighted.draws <- draws*end$weight

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(weighted.draws)

# Use all MCMC draws to get 'no' prediction and CI
end.weighted.all.no <- quantile((mcmc.pr), probs=c(0.50, 0.025, 0.975))


## REG ADJ 

#Setup for out-of-sample prediction loop
draws.pop <- vector('list', length(n))

for (i in n) {
  # Subset county data
  pop.county <- subset(pop.full, as.numeric(county)==i)
  
  # Sample from full population data for each county
  pop.county <- pop.county[sample(1:nrow(pop.county), ceiling(20000*nrow(pop.county)/nrow(pop.full)), replace=FALSE),] 
  
  # Create model matrix
  pop.county.model.matrix <- cbind(rep(1,nrow(pop.county)), subset(pop.county, select=c(dem, gop, maleNA, age55plus, ageNA)))
  names(pop.county.model.matrix) <- colnames(endorse.out$model.matrix.indiv)
  
  #Use lambda for each county and covariates from population of that county to make out-of-sample-predictions 
  #Extract group coefficient and individual-level coefficients
  lambda <- endorse.out$lambda[,c(i,20:24)]
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  draws.pop[[i]] <- pnorm(sweep((as.matrix(pop.county.model.matrix)%*%t(lambda)), 2, sqrt(endorse.out$omega2), "/"))
}

# Create matrix of draws
draws.pop <- do.call("rbind", draws.pop)

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(draws.pop)

# Use all MCMC draws to get 'no' prediction and CI
end.reg.adj.all.no <- quantile((mcmc.pr), probs=c(0.50, 0.025, 0.975))

end.statewide.results <- rbind(end.unweighted.all.no, end.weighted.all.no, end.reg.adj.all.no)
write.table(end.statewide.results, "endorse-statewide-results-rf.txt")


##############################
##   RANDOMIZED RESPONSE    ##
##############################

## UNWEIGHTED 

# Get draws for each county and store them
n <- 1:19
draws <- vector('list', length(n))
counties <- levels(rr$county)

# Create individual-level matrix of covariates for all observations with a county identifier
x.all <- data.frame(county=rr$county, rr.out$x)

for (i in n) {
  # Create individual-level matrix of covariates for each county
  x <- subset(x.all, as.numeric(county)==i)
  x <- x[,-1]
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  draws[[i]] <- exp(rr.out$gamma[,i] + as.matrix(x) %*% t(rr.out$beta)) /
    (1 + exp(rr.out$gamma[,i] + as.matrix(x) %*% t(rr.out$beta)))
}  

# Create matrix of draws
draws <- do.call("rbind", draws)

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(draws)

# Use all MCMC draws to get 'no' prediction and CI
rr.unweighted.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))


## WEIGHTED

#Create weight which sums to number of obs
rr$weight <- rr$weight*nrow(rr)

#Calculate weighted draws
weighted.draws <- draws*rr$weight

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(weighted.draws)

# Use all MCMC draws to get 'no' prediction and CI
rr.weighted.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))


## REG ADJ 

#Setup for out-of-sample prediction loop
draws.pop <- vector('list', length(n))

for (i in n) {
  # Subset county data
  pop.county <- subset(pop.full, as.numeric(county)==i)
  
  # Sample from full population data for each county
  pop.county <- pop.county[sample(1:nrow(pop.county), ceiling(20000*nrow(pop.county)/nrow(pop.full)), replace=FALSE),] 
  
  # Create model matrix
  pop.county.model.matrix <- cbind(rep(1,nrow(pop.county)), subset(pop.county, select=c(gop, maleNA, age55plus, ageNA)))
  names(pop.county.model.matrix) <- colnames(rr.out$x)
  
  # Compute matrix of individual-level probabilities (observations in each county x MCMC draws)
  draws.pop[[i]] <- exp(rr.out$gamma[,i] + as.matrix(pop.county.model.matrix) %*% t(rr.out$beta)) /
    (1 + exp(rr.out$gamma[,i] + as.matrix(pop.county.model.matrix) %*% t(rr.out$beta)))
}

# Create matrix of draws
draws.pop <- do.call("rbind", draws.pop)

# Take average across all obs to get prediction for each MCMC draw (vector of length equal to number of MCMC draws)
mcmc.pr <- colMeans(draws.pop)

# Use all MCMC draws to get 'no' prediction and CI
rr.reg.adj.all.no <- quantile((1-mcmc.pr), probs=c(0.50, 0.025, 0.975))

rr.statewide.results <- rbind(rr.unweighted.all.no, rr.weighted.all.no, rr.reg.adj.all.no)
write.table(rr.statewide.results, "rrt-statewide-results-rf.txt")

####################################################
##                   FIGURE 5                     ##
##               STATEWIDE RESULTS                ##
## BASED ON AGGREGATION OF COUNTY-LEVEL ESTIMATES ##
####################################################

# Load results tables produced above from models with county-level random intercepts
dir.state <- read.table("dir-statewide-results-rf.txt")
list.state <- read.table("list-statewide-results-rf.txt")
end.state <- read.table("endorse-statewide-results-rf.txt")
rrt.state <- read.table("rrt-statewide-results-rf.txt")

pdf("countyagg-statewide-fig.pdf") 
par(mfrow=c(1,1), las=1, mar=c(0,2,2,1), oma=c(2,3,2,1), cex=1)

x.coords <- c(1.33, 1.66, 1.99, 2.99, 3.33, 3.66, 4.66, 4.99, 5.33, 6.33, 6.66, 6.99)

plot(0:11, seq(from = -2, to = 1.5, length = 12), type='n', xlim=c(1,7.33), ylim=c(0,1), axes=FALSE)
axis(2, at=seq(0, 1, .1), las=1, cex.lab=1.2)
abline(h=0.653, lty="dashed")
par(las=0)
mtext("Estimated proportion of 'no' votes on Personhood", outer=TRUE, side=2, cex=1, at=0.5, line=1.5, padj=.1)

est <- c(dir.state[,1], list.state[,1], end.state[,1], rrt.state[,1])
low <- c(dir.state[,2], list.state[,2], end.state[,2], rrt.state[,2])
up <- c(dir.state[,3], list.state[,3], end.state[,3], rrt.state[,3])

segments(x.coords, low, x.coords, up)
points(x.coords, est, pch=rep(c(19,15,17), 3), col=rep(c("black","black","black"), 3), cex=1.5)

text(1.7, .67, "actual vote share", cex=.9, font=3)
mtext("Direct\nQuestion\n(n = 2,655)", outer=TRUE, cex=1, at=0.185, line=-3.5, font=2)
mtext("List\nExperiment\n(n = 1,352)", outer=TRUE, cex=1, at=0.395, line=-3.5, font=2)
mtext("Endorsement\nExperiment\n(n = 1,841)", outer=TRUE, cex=1, at=0.615, line=-3.5, font=2)
mtext("Randomized\nResponse\n(n = 943)", outer=TRUE, cex=1, at=0.845, line=-3.5, font=2)

legend('bottomright', c("Unweighted", "Weighted", "Regression Adjusted"), 
       pch=c(19,15,17), col=c("black","black","black"), bty='n', cex=1)
dev.off()

###################
##  SECTION 4.4  ##
##  DIAGNOSTICS  ##
###################

## Check list experiment for floor/ceiling effects using ict.test()
# Null: No design effect
ict.test(list$list.y, list$treat, J=4, alpha=0.05)

## Check coin toss question in practice round
prop.table(table(dat$qc2))
prop.table(table(dat$qc2)[2:3]) # Deleting item non-response

## Association between age, education and responses on coin toss question
prop.table(table(dat$qc2, dat$eduHIGHER)[2:3,],2) # Deleting item non-response
chisq.test(table(dat$qc2, dat$eduHIGHER)[2:3,])

prop.table(table(dat$qc2, dat$ageyrs >= 25 & dat$ageyrs < 35)[2:3,],2) # Deleting item non-response
chisq.test(table(dat$qc2, dat$ageyrs >= 25 & dat$ageyrs < 35)[2:3,])

## RRT Results under Alternative Assumption
# Forced choice design parameters
p <- .575
p1 <- .425

# UNWEIGHTED

## NOTE: Results shown two ways, but they are similar estimates of true 'no'
## Basic estimator

rr.est <- 1-1/p*(mean(rr$rr.dum) - p1)
rr.se <- 2*sd(rr$rr.dum)/sqrt(length(rr$rr.dum))

## Based on logistic Regression
# Get starting values
fit <- glm(rr.dum ~ 1, data=rr, family=binomial)
start <- fit$coefficients

# Intercept Only
rr.mod <- rrreg(rr.dum ~ 1, data = rr, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")
summary(rr.mod)

# In-sample prediction (unweighted) - based on posterior
rr.pred <- predict(rr.mod, given.y=TRUE, avg=FALSE, quasi.bayes=TRUE)
rr.est <- mean(1-rr.pred$est) #Share 'no'
rr.se <- mean(rr.pred$se)

# WEIGHTED

## Based on Posterior
rr.est.weighted <- 1-sum(rr.pred$est*rr$weight) #Share 'no'

# Bootstrap SEs
n <- 1000 
bootstrap.average.predicted <- rep(NA, n) 
bootstrap.average.predicted.weight <- rep(NA, n) 
counter <- 0

for (j in 1:n){ # loop for bootstrap
  # taking bootstrap sample from the sample, of same sample size, but with replacement
  bootstrap.sample <- rr[sample(nrow(rr), nrow(rr), replace = TRUE, prob = NULL), ] 
  # ensure all weights sum to 1
  bootstrap.sample$weight <- bootstrap.sample$weight/sum(bootstrap.sample$weight)
  # fitting model   
  rr.mod <- rrreg(rr.dum ~ 1, data = bootstrap.sample, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")
  # In-sample prediction (unweighted) - based on posterior
  rr.pred <- predict(rr.mod, given.y = TRUE, avg = FALSE, quasi.bayes = TRUE)
  bootstrap.average.predicted[j] <- mean(1-rr.pred$est) #Share 'no'
  bootstrap.average.predicted.weight[j] <- 1-sum(rr.pred$est*bootstrap.sample$weight)
  counter <- counter + 1
  if (counter == 1) cat("Iteration: \n", counter) else cat(", ", counter)
}

rr.se.weighted <- sqrt(var(bootstrap.average.predicted.weight, na.rm = TRUE)) # calc S.E. from vector of bootstrapped quantity of interest
rr.weighted <- quantile(bootstrap.average.predicted.weight, prob=c(.5, .025, .975))

# REGRESSION ADJUSTED

fit <- glm(rr.dum ~ dem 
           + gop 
           + maleNA 
           + age55plus 
           + ageNA, data=rr, family=binomial)
start <- fit$coefficients

# Full MLE with covariates from the voter file
rr.mod <- rrreg(rr.dum ~ dem 
                + gop 
                + maleNA 
                + age55plus 
                + ageNA, data = rr, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")


pred.ml <- predict(rr.mod, newdata=pop.samp, given.y=FALSE, avg=TRUE, quasi.bayes=TRUE)

rr.reg.adj <- 1-pred.ml$est
rr.reg.adj.se <- pred.ml$se

rr.statewide.results <- rbind(unweighted=c(rr.est, rr.se), weighted=c(rr.est.weighted, rr.se.weighted), regadj=c(rr.reg.adj, rr.reg.adj.se))
write.table(rr.statewide.results, "rrt-statewide-results-alt.txt")

#############################
##       SECTION 4.5       ##
##  EFFICIENCY COMPARISON  ##
##    STATEWIDE RESULTS    ##
#############################

#Load sampled data (with equivalent sample sizes) for efficiency comparison of different indirect approaches 
#See replication code file "validate-replication-code-data-setup-weighting.R"
dir <- read.table("dir-weight-comp.txt")
list <- read.table("list-weight-comp.txt")
#end <- read.table("end-weight-comp.txt")
rr <- read.table("rr-weight-comp.txt")

#################
##   DIRECT    ##
#################

# UNWEIGHTED
dir.est <- mean(dir$dir)
se.dir <- sd(dir$dir, na.rm=T)/sqrt(nrow(as.matrix(na.omit(dir$dir))))

# WEIGHTED
dir.est.weighted <- sum(dir$dir*dir$weight, na.rm=T)
se.dir.weighted <- sqrt(sum(dir$weight^2, na.rm=T)*var(dir$dir, na.rm=T))

dir.statewide.results <- rbind(unweighted=c(1-dir.est, se.dir), weighted=c(1-dir.est.weighted, se.dir.weighted ))
write.table(dir.statewide.results, "dir-statewide-results-comp.txt")

###############
##   LIST    ##
###############

# UNWEIGHTED

#Use ictreg() to get posterior probabilities of saying "yes" to sensitive item
ml.nocov <- ictreg(list.y ~ 1, 
                   data = list, 
                   treat = "treat", 
                   J=4, 
                   method = "ml",
                   constrained = TRUE,
                   fit.start = "lm")
summary(ml.nocov)
list.est <- mean(ml.nocov$pred.post)
predict(ml.nocov, avg=T, se.fit=T) # Gives equivalent results

# Bootstrap SEs for weighted and unweighted results
n <- 1000 
bootstrap.average.predicted <- rep(NA, n) 
bootstrap.average.predicted.weight <- rep(NA, n)  
counter <- 0

for (j in 1:n){ 
  # taking bootstrap sample from the sample, of same sample size, but with replacement
  bootstrap.sample <- list[sample(nrow(list), nrow(list), replace = TRUE, prob = NULL), ] 
  # ensure all weights sum to 1
  bootstrap.sample$weight <- bootstrap.sample$weight/sum(bootstrap.sample$weight)
  # fitting model   
  bootstrap.fit <- ictreg(list.y ~ 1, 
                          data = bootstrap.sample, 
                          treat = "treat", 
                          J=4, 
                          method = "ml",
                          constrained = TRUE,
                          fit.start = "lm")
  
  bootstrap.average.predicted[j] <- mean(bootstrap.fit$pred.post)
  bootstrap.average.predicted.weight[j] <- sum(bootstrap.fit$pred.post*bootstrap.sample$weight)
  counter <- counter + 1
  if (counter == 1) cat("Iteration: \n", counter) else cat(", ", counter)
}

list.se <- sqrt(var(bootstrap.average.predicted, na.rm = TRUE)) 

# WEIGHTED

# Set sum of list weights to 1
list$weight <- list$weight/sum(list$weight)

# Calculate weighted average for mle with no covariates
list.est.weighted <- sum(ml.nocov$pred.post*list$weight)

# Bootstrap weighted SEs
list.se.weighted <- sqrt(var(bootstrap.average.predicted.weight, na.rm = TRUE))

list.statewide.results <- rbind(unweighted=c(1-list.est, list.se), weighted=c(1-list.est.weighted, list.se.weighted ))
write.table(list.statewide.results, "list-statewide-results-comp.txt")

######################
##   ENDORSEMENT    ##
######################

## >> NOTE: These results are replicated using the script "endorse-comp-iters.R"
## >> That script is not sourced directly here because it is 
## >> set up for parallel execution on multiple nodes.
## >> All MCMC draws and datasets produced by that analysis along with the results are
## >> archived in replication-endorse-comp.zip

## ########################
## Analyze Results Locally
## ########################

# Load chains to check convergence
nIters <- 35
beta <- matrix(NA, nIters, 5)
lambda <- matrix(NA, nIters, 2)
omega2 <- matrix(NA, nIters, 2)

for (j in 1:nIters){
  load(paste("endorse_out_comp_iter_", j, ".RData", sep=""))
  
  all.list.beta <- mcmc.list(list(as.mcmc(out[[1]]$beta), as.mcmc(out[[2]]$beta),
                                  as.mcmc(out[[3]]$beta), as.mcmc(out[[4]]$beta)))     
  
  all.list.lambda <- mcmc.list(list(as.mcmc(out[[1]]$lambda), as.mcmc(out[[2]]$lambda),
                                    as.mcmc(out[[3]]$lambda), as.mcmc(out[[4]]$lambda))) 
  
  all.list.omega2 <- mcmc.list(list(as.mcmc(out[[1]]$omega2), as.mcmc(out[[2]]$omega2),
                                    as.mcmc(out[[3]]$omega2), as.mcmc(out[[4]]$omega2))) 
  
  beta[j,] <- unlist(gelman.diag(all.list.beta)) # Col 1: alpha point est; Col 2: beta point est; Col 3: alpha upper CI; Col 4: beta upper CI; Col 5: mpsrf
  lambda[j,] <- unlist(gelman.diag(all.list.lambda)) # Col 1: point est, Col 2: upper CI
  omega2[j,] <- unlist(gelman.diag(all.list.omega2))
}

# Identify iterations with poor convergence
beta[,1:2]>=1.2
lambda[,1]>=1.2
omega2[,1]>=1.2

# Load results
d <- vector('list', length(nIters))

for (i in 1:nIters){
  d[[i]] <- read.table(paste("endorse-statewide-results-comp_iter_", i, ".txt", sep=""))
}

# Make table of unweighted and weighted results 
unweight <- do.call("rbind", lapply(d, `[`,1,))
rownames(unweight) <- 1:35
weight <- do.call("rbind", lapply(d, `[`,2,))
rownames(weight) <- 1:35

## Check stability of median across sims
# Find median of first 10 sims
median(unweight[1:10,1])
median(weight[1:10,1])

# Find median of first 15 sims
median(unweight[1:15,1])
median(weight[1:15,1])

# Find median of first 20 sims
median(unweight[1:20,1])
median(weight[1:20,1])

# Find median of first 25 sims
median(unweight[1:25,1])
median(weight[1:25,1])

# Find median of first 35 sims
median(unweight[1:35,1])
median(weight[1:35,1])

## Remove iterations with poor convergence
unweight.converged <- unweight[-c(5,10,17,21,22,27,28,31,34,35),]
weight.converged <- weight[-c(5,10,17,21,22,27,28,31,34,35),]

## Find mean point estimate, lower bound, and upper bound
end.unweighted <- colMeans(unweight.converged)
end.weighted <- colMeans(weight.converged)
end.statewide.results <- rbind(end.unweighted, end.weighted)
rownames(end.statewide.results) <- c("unweighted", "weighted")
write.table(end.statewide.results, "endorse-statewide-results-comp.txt")

## Find median of 25 iterations with good convergence 
median(unweight.converged[,1])

## Take point estimate and CI from median iteration
end.unweighted <- unweight[16,]
end.weighted <- weight[16,]
end.statewide.results.median <- rbind(end.unweighted, end.weighted)
rownames(end.statewide.results.median) <- c("unweighted", "weighted")
print(end.statewide.results.median)


##############################
##   RANDOMIZED RESPONSE    ##
##############################

# Forced choice design parameters
p <- .5
p1 <- .5

## UNWEIGHTED

## NOTE: Results shown two ways, but they are similar estimates of true 'no'
## Basic estimator

rr.est <- 1-1/p*(mean(rr$rr.dum) - p1)
rr.se <- 2*sd(rr$rr.dum)/sqrt(length(rr$rr.dum))

## Based on logistic Regression
# Get starting values
fit <- glm(rr.dum ~ 1, data=rr, family=binomial)
start <- fit$coefficients

# Model with intercept only
rr.mod <- rrreg(rr.dum ~ 1, data = rr, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")
summary(rr.mod)

# In-sample prediction (unweighted) - based on posterior
rr.pred <- predict(rr.mod, given.y = TRUE, avg = FALSE, quasi.bayes = TRUE) 
table(rr.pred$est)
rr.est <- mean(1-rr.pred$est) #Share 'no'

## WEIGHTED

## Based on Posterior
rr.est.weighted <- 1-sum(rr.pred$est*rr$weight) #Share 'no'

# Bootstrap SEs
n <- 1000 
bootstrap.average.predicted <- rep(NA, n) 
bootstrap.average.predicted.weight <- rep(NA, n) 
counter <- 0

for (j in 1:n){ # loop for bootstrap
  # taking bootstrap sample from the sample, of same sample size, but with replacement
  bootstrap.sample <- rr[sample(nrow(rr), nrow(rr), replace = TRUE, prob = NULL), ] 
  # ensure all weights sum to 1
  bootstrap.sample$weight <- bootstrap.sample$weight/sum(bootstrap.sample$weight)
  # fitting model   
  rr.mod <- rrreg(rr.dum ~ 1, data = bootstrap.sample, p = p, p0 = 0, p1 = p1, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")
  # In-sample prediction (unweighted) - based on posterior
  rr.pred <- predict(rr.mod, given.y = TRUE, avg = FALSE, quasi.bayes=TRUE)
  bootstrap.average.predicted[j] <- mean(1-rr.pred$est) #Share 'no'
  bootstrap.average.predicted.weight[j] <- 1-sum(rr.pred$est*bootstrap.sample$weight)
  counter <- counter + 1
  if (counter == 1) cat("Iteration: \n", counter) else cat(", ", counter)
}

rr.se <- sqrt(var(bootstrap.average.predicted, na.rm = TRUE))
rr.se.weighted <- sqrt(var(bootstrap.average.predicted.weight, na.rm = TRUE)) # calc S.E. from vector of bootstrapped quantity of interest

rr.statewide.results <- rbind(unweighted=c(rr.est, rr.se), weighted=c(rr.est.weighted, rr.se.weighted))
write.table(rr.statewide.results, "rrt-statewide-results-comp.txt")


###################################
##            FIGURE 6           ##
##         POOLED RESULTS        ##
## USING EQUIVALENT SAMPLE SIZES ##
##      EFFICIENCY COMPARISON    ##
###################################

# Read in results produced above using equivalent sample sizes
dir.state <- read.table("dir-statewide-results-comp.txt")
list.state <- read.table("list-statewide-results-comp.txt")
end.state <- read.table("endorse-statewide-results-comp.txt")
rrt.state <- read.table("rrt-statewide-results-comp.txt")

# Calculate CIs for plot using results tables 
dir.state <- data.frame(dir.state, low=dir.state[,1]-qnorm(.975)*dir.state[,2], up=dir.state[,1]+qnorm(.975)*dir.state[,2])
list.state <- data.frame(list.state, low=list.state[,1]-qnorm(.975)*list.state[,2], up=list.state[,1]+qnorm(.975)*list.state[,2])
end.state <- data.frame(end.state[,1], rep(NA, 2), low=end.state[,2], up=end.state[,3])
rrt.state <- data.frame(rrt.state, low=rrt.state[,1]-qnorm(.975)*rrt.state[,2], up=rrt.state[,1]+qnorm(.975)*rrt.state[,2])

pdf("comp-pooled-results-fig.pdf") 
par(mfrow=c(1,1), las=1, mar=c(0,2,2,3.5), oma=c(2,3,2,0), cex=1) 

x.coords <- c(1.33, 1.66, 2.66, 2.99, 3.99, 4.33, 5.33, 5.66)

plot(0:7, seq(from = -2, to = 1.5, length = 8), type='n', xlim=c(1,5.99), ylim=c(0,1), axes=FALSE,
     main="")
axis(2, at=seq(0, 1, .1), las=1, cex.lab=1.2)
abline(h=0.653, lty="dashed")
par(las=0)
mtext("Estimated proportion of 'no' votes on Personhood", outer=TRUE, side=2, cex=1, at=0.5, line=1.5, padj=.1)

est <- c(dir.state[1:2,1], list.state[1:2,1], end.state[1:2,1], rrt.state[1:2,1])
low <- c(dir.state[1:2,3], list.state[1:2,3], end.state[1:2,3], rrt.state[1:2,3])
up <- c(dir.state[1:2,4], list.state[1:2,4], end.state[1:2,4], rrt.state[1:2,4])

segments(x.coords, low, x.coords, up)
points(x.coords, est, pch=rep(c(19,15), 5), col=rep(c("black","black"), 5), cex=1.5)

text(1.7, .67, "actual vote share", cex=.9, font=3)

mtext("Direct\nQuestion\n(n = 940)", outer=TRUE, cex=1, at=0.18, line=-3.5, font=2)
mtext("List\nExperiment\n(n = 940)", outer=TRUE, cex=1, at=0.38, line=-3.5, font=2)
mtext("Endorsement\nExperiment\n(n = 940)", outer=TRUE, cex=1, at=0.58, line=-3.5, font=2)
mtext("Randomized\nResponse\n(n = 940)", outer=TRUE, cex=1, at=0.79, line=-3.5, font=2)

legend('bottomright', c("Unweighted", "Weighted"), 
       pch=c(19,15), col=c("black","black"), bty='n', cex=1)
dev.off()


#################################
##         SECTION 4.6         ##
##  INDIVIDUAL-LEVEL ANALYSIS  ##
#################################

## Load Data
dir <- read.table("dir-weight.txt")
list <- read.table("list-weight.txt")
end <- read.table("end-weight.txt")
rr <- read.table("rr-weight.txt")

## Common covariate formula
# Include these variables:
# age, age squared, gender, PID, education
cov.formula <- formula(~ ageyrs.new + ageyrs2.new + male + pidREP + pidIND + eduHIGHER)

#################
##   DIRECT    ##
#################

dir.logit <- glm(update.formula(cov.formula, dir ~.), family=binomial(link=logit), data=dir)
summary(dir.logit)

vars <- c("male", "pidREP", "eduHIGHER")
model.matrix <- data.frame(model.matrix(dir.logit))
predict <- NULL
tab <- matrix(NA, 3, 6)
rownames(tab) <- vars
colnames(tab) <- c("est.1", "low.1", "hi.1", "est.0", "low.0", "hi.0")
j <- 0

for (i in vars){
  j <- j+1
  new.model.matrix <- subset(model.matrix, get(i)==1)
  predict <- predict(dir.logit, newdata=new.model.matrix, type="response", se.fit=TRUE)
  tab[j,1] <- 1-mean(predict$fit) # predicted probability of "no" vote
  tab[j,2] <- (1-mean(predict$fit))-qnorm(.975)*mean(predict$se.fit)
  tab[j,3] <- (1-mean(predict$fit))+qnorm(.975)*mean(predict$se.fit)
  new.model.matrix <- subset(model.matrix, get(i)==0)
  predict <- predict(dir.logit, newdata=new.model.matrix, type="response", se.fit=TRUE)
  tab[j,4] <- 1-mean(predict$fit) # predicted probability of "no" vote
  tab[j,5] <- (1-mean(predict$fit))-qnorm(.975)*mean(predict$se.fit)
  tab[j,6] <- (1-mean(predict$fit))+qnorm(.975)*mean(predict$se.fit)
}

# Replace prediction for non-republicans with prediction for democrats
new.model.matrix <- subset(model.matrix, pidREP==0 & pidIND==0)
predict <- predict(dir.logit, newdata=new.model.matrix, type="response", se.fit=TRUE)
tab[2,4] <- 1-mean(predict$fit) # predicted probability of "no" vote
tab[2,5] <- (1-mean(predict$fit))-qnorm(.975)*mean(predict$se.fit)
tab[2,6] <- (1-mean(predict$fit))+qnorm(.975)*mean(predict$se.fit)

dir.tab <- tab
rownames(dir.tab) <- c("male", "pidREP", "eduHIGHER")



###############
##   LIST    ##
###############

ml <- ictreg(list.y ~ ageyrs.new + ageyrs2.new + male + pidREP + pidIND + eduHIGHER, 
             data = list, 
             treat = "treat", 
             J=4, 
             method = "ml",
             constrained = TRUE,
             fit.start = "lm")
summary(ml)

vars <- c("male", "pidREP", "eduHIGHER")
model.matrix <- data.frame(ml$x) 
predict <- NULL
tab <- matrix(NA, 3, 6)
rownames(tab) <- vars
colnames(tab) <- c("est.1", "low.1", "hi.1", "est.0", "low.0", "hi.0")
j <- 0

for (i in vars){
  j <- j+1
  new.model.matrix <- subset(model.matrix, get(i)==1)
  predict <- predict(ml, newdata=new.model.matrix, avg=T, se.fit=T)
  tab[j,1] <- 1-mean(predict$fit) # predicted probability of "no" vote
  tab[j,2] <- (1-mean(predict$fit))-qnorm(.975)*mean(predict$se.fit)
  tab[j,3] <- (1-mean(predict$fit))+qnorm(.975)*mean(predict$se.fit)
  new.model.matrix <- subset(model.matrix, get(i)==0)
  predict <- predict(ml, newdata=new.model.matrix, avg=T, se.fit=T)
  tab[j,4] <- 1-mean(predict$fit) # predicted probability of "no" vote
  tab[j,5] <- (1-mean(predict$fit))-qnorm(.975)*mean(predict$se.fit)
  tab[j,6] <- (1-mean(predict$fit))+qnorm(.975)*mean(predict$se.fit)
}

# Replace prediction for non-republicans with prediction for democrats
new.model.matrix <- subset(model.matrix, pidREP==0 & pidIND==0)
predict <- predict(ml, newdata=new.model.matrix, avg=T, se.fit=T)
tab[2,4] <- 1-mean(predict$fit) # predicted probability of "no" vote
tab[2,5] <- (1-mean(predict$fit))-qnorm(.975)*mean(predict$se.fit)
tab[2,6] <- (1-mean(predict$fit))+qnorm(.975)*mean(predict$se.fit)

list.tab <- tab
rownames(list.tab) <- c("male", "pidREP", "eduHIGHER")

######################
##   ENDORSEMENT    ##
######################

# Must omit obs with missing values on covariates (leaves 1652 obs)
end.cov <- subset(end, select=c(end.bryant.c, end.bryant.t, ageyrs.new, ageyrs2.new, male, pidREP, pidIND, eduHIGHER, weight, county))
end.cov <- end.cov[!is.na(end.cov$ageyrs.new) 
                   & !is.na(end.cov$ageyrs2.new)
                   & !is.na(end.cov$male) 
                   & !is.na(end.cov$pidREP) 
                   & !is.na(end.cov$pidIND) 
                   & !is.na(end.cov$eduHIGHER),]

Y <- list(Q1 = c("end.bryant.c", "end.bryant.t"))

MCMC <- 2000000
burn <- 200000
thin <- 360

start.value <- rnorm(4, sd = .6)

chains <- 1:4
endorse.out <- vector('list', length(chains))

for (i in chains){
  endorse.out[[i]] <- endorse(Y = Y,
                              data = end.cov,
                              s.out = TRUE,
                              MCMC = MCMC,
                              burn = burn,
                              thin = thin,
                              prop = .03,
                              identical.lambda = TRUE,
                              covariates = TRUE,
                              hierarchical = FALSE,
                              formula.indiv = cov.formula,
                              s0.omega2 = 1,
                              nu0.omega2 = 40,
                              s0.phi2 = 1,
                              nu0.phi2 = 10,
                              s0.psi2 = 1,
                              nu0.psi2 = 10,
                              s0.rho2 = 1,
                              nu0.rho2 = 10,
                              beta.start = matrix(start.value[i], nrow = 1, ncol = 2))
  holder <- endorse.out[[i]]
  save(holder, file=paste("endorse_out_survey_covs_", i, ".RData", sep=""))
}

# COMBINE CHAINS

load("endorse_out_survey_covs_1.RData")
endorse.out.a <- holder
load("endorse_out_survey_covs_2.RData")
endorse.out.b <- holder
load("endorse_out_survey_covs_3.RData")
endorse.out.c <- holder
load("endorse_out_survey_covs_4.RData")
endorse.out.d <- holder

# Combine second half of all four chains for 10k MCMC draws
# similar to output object from a single chain run using endorse()

endorse.out <-NULL

endorse.out$beta <- rbind(as.matrix(endorse.out.a$beta[2501:5000,]), as.matrix(endorse.out.b$beta[2501:5000,]), 
                          as.matrix(endorse.out.c$beta[2501:5000,]), as.matrix(endorse.out.d$beta[2501:5000,]))

endorse.out$lambda <- rbind(as.matrix(endorse.out.a$lambda[2501:5000,]), as.matrix(endorse.out.b$lambda[2501:5000,]), 
                            as.matrix(endorse.out.c$lambda[2501:5000,]), as.matrix(endorse.out.d$lambda[2501:5000,]))

endorse.out$omega2 <- rbind(as.matrix(endorse.out.a$omega2[2501:5000,]), as.matrix(endorse.out.b$omega2[2501:5000,]), 
                            as.matrix(endorse.out.c$omega2[2501:5000,]), as.matrix(endorse.out.d$omega2[2501:5000,]))

endorse.out$s <- rbind(as.matrix(endorse.out.a$s[2501:5000,]), as.matrix(endorse.out.b$s[2501:5000,]), 
                       as.matrix(endorse.out.c$s[2501:5000,]), as.matrix(endorse.out.d$s[2501:5000,]))

endorse.out$delta <- rbind(as.matrix(endorse.out.a$delta[2501:5000,]), as.matrix(endorse.out.b$delta[2501:5000,]), 
                           as.matrix(endorse.out.c$delta[2501:5000,]), as.matrix(endorse.out.d$delta[2501:5000,]))

endorse.out$x <- rbind(as.matrix(endorse.out.a$x[2501:5000,]), as.matrix(endorse.out.b$x[2501:5000,]), 
                       as.matrix(endorse.out.c$x[2501:5000,]), as.matrix(endorse.out.d$x[2501:5000,]))

endorse.out$model.matrix.indiv <- endorse.out.a$model.matrix.indiv

# SAVE/RUN LOCALLY
save(endorse.out, file="endorse_out_survey_covs_combined_chain_final.RData")
load("endorse_out_survey_covs_combined_chain_final.RData")

vars <- c("male", "pidREP", "eduHIGHER")
model.matrix <- data.frame(endorse.out$model.matrix.indiv)  
predict <- NULL
tab <- matrix(NA, 3, 6)
rownames(tab) <- vars
colnames(tab) <- c("est.1", "low.1", "hi.1", "est.0", "low.0", "hi.0")
j <- 0

for (i in vars){
  j <- j+1
  new.model.matrix <- subset(model.matrix, get(i)==1)
  predict <- colMeans(pnorm(sweep((as.matrix(new.model.matrix)%*%t(endorse.out$lambda)), 2, sqrt(endorse.out$omega2), "/")))
  tab[j,1] <- quantile(predict, probs=c(0.50)) # predicted probability of "no" vote
  tab[j,2] <- quantile(predict, probs=c(0.025))
  tab[j,3] <- quantile(predict, probs=c(0.975))
  new.model.matrix <- subset(model.matrix, get(i)==0)
  predict <- colMeans(pnorm(sweep((as.matrix(new.model.matrix)%*%t(endorse.out$lambda)), 2, sqrt(endorse.out$omega2), "/")))
  tab[j,4] <- quantile(predict, probs=c(0.50)) # predicted probability of "no" vote
  tab[j,5] <- quantile(predict, probs=c(0.025))
  tab[j,6] <- quantile(predict, probs=c(0.975))
}

# Replace prediction for non-republicans with prediction for democrats
new.model.matrix <- subset(model.matrix, pidREP==0 & pidIND==0)
predict <- colMeans(pnorm(sweep((as.matrix(new.model.matrix)%*%t(endorse.out$lambda)), 2, sqrt(endorse.out$omega2), "/")))
tab[2,4] <- quantile(predict, probs=c(0.50))  # predicted probability of "no" vote
tab[2,5] <- quantile(predict, probs=c(0.025))
tab[2,6] <- quantile(predict, probs=c(0.975))

end.tab <- tab
rownames(end.tab) <- c("male", "pidREP", "eduHIGHER")

##############################
##   RANDOMIZED RESPONSE    ##
##############################

fit <- glm(update.formula(cov.formula, rr.dum ~.), data=rr, family=binomial)
start <- fit$coefficients
rr.mod <- rrreg(rr.dum ~ageyrs.new + ageyrs2.new + male + pidREP + pidIND + eduHIGHER, data = rr, p = 1/2, p0 = 0, p1 = 1/2, verbose=TRUE, start = start, maxIter = 10000, design = "forced-known")             
summary(rr.mod)

vars <- c("male", "pidREP", "eduHIGHER")
model.matrix <- data.frame(rr.mod$x) 
predict <- NULL
tab <- matrix(NA, 3, 6)
rownames(tab) <- vars
colnames(tab) <- c("est.1", "low.1", "hi.1", "est.0", "low.0", "hi.0")
j <- 0

for (i in vars){
  j <- j+1
  new.model.matrix <- subset(model.matrix, get(i)==1)
  predict <- predict(rr.mod, newdata=new.model.matrix, given.y=F, avg=T, quasi.bayes=T)
  tab[j,1] <- 1-predict$est # predicted probability of "no" vote
  tab[j,2] <- (1-predict$est)-qnorm(.975)*predict$se
  tab[j,3] <- (1-predict$est)+qnorm(.975)*predict$se
  new.model.matrix <- subset(model.matrix, get(i)==0)
  predict <- predict(rr.mod, newdata=new.model.matrix, given.y=F, avg=T, quasi.bayes=T)
  tab[j,4] <- 1-predict$est # predicted probability of "no" vote
  tab[j,5] <- (1-predict$est)-qnorm(.975)*predict$se
  tab[j,6] <- (1-predict$est)+qnorm(.975)*predict$se
}

# Replace prediction for non-republicans with prediction for democrats
new.model.matrix <- subset(model.matrix, pidREP==0 & pidIND==0)
predict <- predict(rr.mod, newdata=new.model.matrix, given.y=F, avg=T, quasi.bayes=T)
tab[2,4] <- 1-predict$est # predicted probability of "no" vote
tab[2,5] <- (1-predict$est)-qnorm(.975)*predict$se
tab[2,6] <- (1-predict$est)+qnorm(.975)*predict$se

rr.tab <- tab
rownames(rr.tab) <- c("male", "pidREP", "eduHIGHER")


## Save individual-level analysis results for use in plot
write.table(dir.tab, file="dir-survey-cov-predictions-final.txt")
write.table(list.tab, file="list-survey-cov-predictions-final.txt")
write.table(end.tab, file="end-survey-cov-predictions-final.txt")
write.table(rr.tab, file="rr-survey-cov-predictions-final.txt")

###############################
##          FIGURE 7         ##
## INDIVIDUAL-LEVEL ANALYSIS ##
###############################

dir.tab <- read.table("dir-survey-cov-predictions-final.txt")
list.tab <- read.table("list-survey-cov-predictions-final.txt")
end.tab <- read.table("end-survey-cov-predictions-final.txt")
rr.tab <- read.table("rr-survey-cov-predictions-final.txt")

indiv.tab <- rbind(dir.tab, list.tab, end.tab, rr.tab)

#Set point sz/line to be used
x <- .8
a <- -2.5

pdf("indiv-analysis.pdf", height = 4, width = 8.5) #open pdf device
par(mfrow=c(1,1), las=1, mar=c(0,2,2,1), oma=c(1,3,1,0), cex=1) 

x.coords <- c(1.33, 1.66, 1.99, 2.33, # Grouped by variable level
              3.33, 3.66, 3.99, 4.33,
              6.33, 6.66, 6.99, 7.33,
              8.33, 8.66, 8.99, 9.33,
              11.33, 11.66, 11.99, 12.33,
              13.33, 13.66, 13.99, 14.33)

plot(0:23, seq(from = -2, to = 1.5, length = 24), type='n', xlim=c(1,14.66), ylim=c(0,1), axes=FALSE,
     main="")
axis(2, at=seq(0, 1, .1), las=1, cex.lab=1.2)
par(las=0)
mtext("Estimated proportion of 'no' votes on Personhood", outer=TRUE, side=2, cex=1, at=0.5, line=1.5, padj=.1)

est <- c(indiv.tab[seq(1,12,3), 1],
         indiv.tab[seq(1,12,3), 4],
         indiv.tab[seq(2,12,3), 1],
         indiv.tab[seq(2,12,3), 4],
         indiv.tab[seq(3,12,3), 4],
         indiv.tab[seq(3,12,3), 1])

low <- c(indiv.tab[seq(1,12,3), 2],
         indiv.tab[seq(1,12,3), 5],
         indiv.tab[seq(2,12,3), 2],
         indiv.tab[seq(2,12,3), 5],
         indiv.tab[seq(3,12,3), 5],
         indiv.tab[seq(3,12,3), 2])

up <- c(indiv.tab[seq(1,12,3), 3],
        indiv.tab[seq(1,12,3), 6],
        indiv.tab[seq(2,12,3), 3],
        indiv.tab[seq(2,12,3), 6],
        indiv.tab[seq(3,12,3), 6],
        indiv.tab[seq(3,12,3), 3])

segments(x.coords, low, x.coords, up)
points(x.coords, est, pch=(rep(c(19, 15, 23, 17), 6)), col="black", bg="black", cex=1)
abline(h=0.653, lty="dashed")

mtext("Gender", outer=TRUE, cex=1.3, at=0.20, line=-1, font=2)
mtext("Party", outer=TRUE, cex=1.3, at=0.5, line=-1, font=2)
mtext("Education", outer=TRUE, cex=1.3, at=0.835, line=-1, font=2)

mtext("Male", outer=TRUE, cex=1, at=0.13, line=a, font=2)
mtext("Female", outer=TRUE, cex=1, at=0.26, line=a, font=2)

mtext("Republican", outer=TRUE, cex=1, at=0.44, line=a, font=2)
mtext("Democrat", outer=TRUE, cex=1, at=0.57, line=a, font=2)

mtext("No Higher", outer=TRUE, cex=1, at=0.76, line=a, font=2)
mtext("Higher", outer=TRUE, cex=1, at=0.89, line=a, font=2)

text(1.33, up[1]+.03, "DIR", cex=x)
text(1.66, up[2]+.03, "LIST", cex=x)
text(1.99, up[3]+.03, "END", cex=x)
text(2.33, up[4]+.03, "RR", cex=x)
text(6.33, up[9]+.03, "DIR", cex=x)
text(6.66, up[10]+.03, "LIST", cex=x)
text(6.99, up[11]+.03, "END", cex=x)
text(7.33, up[12]+.03, "RR", cex=x)
text(11.33, up[17]+.03, "DIR", cex=x)
text(11.66, up[18]+.03, "LIST", cex=x)
text(11.99, up[19]+.025, "END", cex=x)
text(12.33, up[20]+.052, "RR", cex=x)
dev.off()

###################
## SUPPLEMENTARY ##
##   APPENDIX    ##
###################

######################################
##            TABLE 3               ##
## CHISQ TESTS FOR ORDERING EFFECTS ##
######################################

# DIRECT
d1 <- subset(dat, condition=="D")
d1$first <- rep(1, nrow(d1))
d <- subset(dat, condition!="D")
d$first <- rep(0, nrow(d))
dir <- rbind(d1, d)

# LIST
l1 <- subset(dat, condition=="2A1AD"
             | condition=="2A1BD"
             | condition=="2AD"
             | condition=="2B1AD"
             | condition=="2B1BD"
             | condition=="2BD")
l1$first <- rep(1, nrow(l1))
l <- subset(dat, condition=="1A2AD"
            | condition=="1A2BD"
            | condition=="1B2AD"
            | condition=="1B2BD")
l$first <- rep(0, nrow(l))
list <- rbind(l1, l)
list.c <- subset(list, list.control==1)
list.t <- subset(list, list.treat==1)

# ENDORSEMENT
e1 <- subset(dat, condition=="1A2AD"
             | condition=="1A2BD"
             | condition=="1B2AD"
             | condition=="1B2BD"
             | condition=="1ACD"
             | condition=="1BCD")
e1$first <- rep(1, nrow(e1))
e <- subset(dat, condition=="2A1AD"
            | condition=="2A1BD"
            | condition=="2B1AD"
            | condition=="2B1BD"
            | condition=="C1AD"
            | condition=="C1BD")
e$first <- rep(0, nrow(e))
end <- rbind(e1, e)
end.c <- subset(end, end.control==1)
end.t <- subset(end, end.treat==1)

# RR
r1 <- subset(dat, condition=="C1AD" 
             | condition=="C1BD")
r1$first <- rep(1, nrow(r1))
r <- subset(dat, condition=="1ACD"
            | condition=="1BCD")
r$first <- rep(0, nrow(r))
rrt <- rbind(r1, r)


## Chi Squared Results

# Direct
tab <- xtabs(~dir$dir + dir$first)
tab
summary(tab)
xtable(tab)

# List
tab <- xtabs(~list.c$list.y + list.c$first)
tab
summary(tab)
xtable(tab)
tab <- xtabs(~list.t$list.y + list.t$first)
tab
summary(tab)
xtable(tab)

# Endorsement
tab <- xtabs(~end.c$end.bryant.c + end.c$first)
tab
summary(tab)
xtable(tab)
tab <- xtabs(~end.t$end.bryant.t + end.t$first)
tab
summary(tab)
xtable(tab)

# RRT
tab <- xtabs(~rrt$rr.dum + rrt$first)
tab
summary(tab)
xtable(tab)

#######################################
##         TABLES 4 & 5              ##
## PREDICTING NON-RESP BY CONDITION  ##
##    LOGISTIC REGRESSION RESULTS    ##
##               AND                 ##
## PREDICTED PROBABILITY OF NON-RESP ##
##    BY RESPONDENT CHARACTERISTIC   ##
#######################################

## Model nonresponse as a function of covariates

# Common covariate formula 
# Include age, age squared, gender, party, and 6-category education 
cov.formula <- formula(~ ageyrs.new + ageyrs2.new + male + pid + edu)

# Direct
m.dir <- glm(update.formula(cov.formula, dir.nr ~.), family=binomial(link=logit), data=dat)
summary(m.dir)

# List
m.list <- glm(update.formula(cov.formula, list.nr ~.), family=binomial(link=logit), data=dat)
summary(m.list)

# Endorsement
m.end <- glm(end.nr ~ ageyrs.new + ageyrs2.new + male + eduHIGHER, family=binomial(link=logit), data=dat) 
summary(m.end)
    # Note: Because the overall non-response rate is very low for the endorsement condition, including 
    # additional categories of education or pid results in singularities in the model.

# RRT
m.rr <- glm(update.formula(cov.formula, rr.nr ~.), family=binomial(link=logit), data=dat)
summary(m.rr)

# Output results (Table 5)
stargazer(m.dir, m.list, m.end, m.rr, title="Logistic Regression Results Predicting Nonresponse by Condition", align=TRUE, out="predicting-nonresponse.tex")

## Now calculate the predicted probability of nonresponse by respondent characteristic (Table 4)

# Age
ages <- c(20,40,60,80)
pr.nr.byage <- list(matrix(NA, length(ages), 2), matrix(NA, length(ages), 2), matrix(NA, length(ages), 2))
mods <- list(m.dir, m.list, m.rr)
for (j in 1:length(mods)){
  for (i in 1:length(ages)){
    age <- data.frame(ageyrs.new=ages[i]/10, 
                      ageyrs2.new=(ages[i]/10)^2, 
                      male=dat$male, 
                      pid=dat$pid, 
                      edu=dat$edu)
    
    pred <- predict(mods[[j]], newdata=age, type = "response", se.fit=T)
    pr.nr.byage[[j]][i,] <- cbind(mean(pred$fit, na.rm=T), mean(pred$se.fit, na.rm=T))
  }
}
pr.nr.byage

# Gender
sex <- c(1,0)
pr.nr.bysex <- list(matrix(NA, length(sex), 2), matrix(NA, length(sex), 2), matrix(NA, length(sex), 2))
mods <- list(m.dir, m.list, m.rr)
for (j in 1:length(mods)){
  for (i in 1:length(sex)){
    gender <- data.frame(ageyrs.new=dat$ageyrs.new, 
                         ageyrs2.new=dat$ageyrs2.new, 
                         male=sex[i], 
                         pid=dat$pid, 
                         edu=dat$edu)
    
    pred <- predict(mods[[j]], newdata=gender, type = "response", se.fit=T)
    pr.nr.bysex[[j]][i,] <- cbind(mean(pred$fit, na.rm=T), mean(pred$se.fit, na.rm=T))
  }
}
pr.nr.bysex

# PID
pid <- c("REP", "DEM")
pr.nr.bypid <- list(matrix(NA, length(pid), 2), matrix(NA, length(pid), 2), matrix(NA, length(pid), 2))
mods <- list(m.dir, m.list, m.rr)
for (j in 1:length(mods)){
  for (i in 1:length(pid)){
    party <- data.frame(ageyrs.new=dat$ageyrs.new, 
                        ageyrs2.new=dat$ageyrs2.new, 
                        male=dat$male, 
                        pid=pid[i], 
                        edu=dat$edu)
    
    pred <- predict(mods[[j]], newdata=party, type = "response", se.fit=T)
    pr.nr.bypid[[j]][i,] <- cbind(mean(pred$fit, na.rm=T), mean(pred$se.fit, na.rm=T))
  }
}
pr.nr.bypid

# Education
edu <- c("1_Less than high school", "2_High School", "3_Incomplete college", "4_Associate degree", "5_Bachelors degree", "6_Postgrad degree")
pr.nr.byedu <- list(matrix(NA, length(edu), 2), matrix(NA, length(edu), 2), matrix(NA, length(edu), 2))
mods <- list(m.dir, m.list, m.rr)
for (j in 1:length(mods)){
  for (i in 1:length(edu)){
    educat <- data.frame(ageyrs.new=dat$ageyrs.new, 
                         ageyrs2.new=dat$ageyrs2.new, 
                         male=dat$male, 
                         pid=dat$pid, 
                         edu=edu[i])
    
    pred <- predict(mods[[j]], newdata=educat, type = "response", se.fit=T)
    pr.nr.byedu[[j]][i,] <- cbind(mean(pred$fit, na.rm=T), mean(pred$se.fit, na.rm=T))
  }
}
pr.nr.byedu


#################################
##         FIGURE 8            ##
##     TEST FOR BIAS FROM      ##
## SELF-PROTECTIVE NO RESPONSE ##
#################################

# Is county-level "NO" on turnout RRT practice question (y-axis) related to estimated "NO" on Personhood (x-axis)?
turnout.nos.bycounty <- tapply(dat$turnout.no, dat$county, mean, na.rm=T)

# Load RRT county-level estimates
rrt.results <- read.table("rrt-unweighted-results-rf.txt", header=T)

# Calculate correlation between observed share of "no's" on turnout practice question
# and RRT unweighted "no" vote estimates by county 
cor(turnout.nos.bycounty, rrt.results$rr.unweighted)

pdf("turnout-no-vs-rrt-est-no.pdf", height = 6, width = 7) 
plot(rrt.results$rr.unweighted, turnout.nos.bycounty, 
     xlab="Estimated No Vote on Personhood (RRT Unweighted)",
     ylab="Observed Share of No's on Turnout Practice Question",
     xlim=c(.50,.82),
)
textxy(rrt.results$rr.unweighted, turnout.nos.bycounty, names(turnout.nos.bycounty), cex=.9, offset=.6)
abline(lm(turnout.nos.bycounty ~ rrt.results$rr.unweighted))
text(.6,.095,paste("r=", round(cor(turnout.nos.bycounty, rrt.results$rr.unweighted),3), sep=""), font=2)
dev.off()
