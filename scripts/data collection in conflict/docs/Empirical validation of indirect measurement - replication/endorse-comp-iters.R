rm(list = ls())

### #############
### Parse CL Args
### #############

args <- commandArgs(trailingOnly = TRUE)
nIters <- as.integer(args[1]) # nIters = 1 for test run, 35 for final results
paste("nIters:", nIters)

### ############
### Dependencies
### ############

# Packages needed for the parallelization
library(foreach)
library(doMPI)    
library(doRNG)

# Packages needed for the analysis
library("arm")
library("endorse")
library("coda")

### ################
### Init MPI Backend
### ################
cl <- startMPIcluster()
registerDoMPI(cl)

### ##############
### Master Process
### ##############
system("hostname")
Sys.getpid()

### ########################
### Main Loop over MPI Procs
### ########################

registerDoRNG(1) # Reproducible parallel RNG << add 1

# SETUP OUTSIDE OF FOREACH() LOOP
# ENDORSEMENT MODEL WITH NO COVARIATES 
# COMPARABLE SAMPLE

set.seed(44) # Random starting values are generated outside of foreach()

Y <- list(Q1 = c("end.bryant.c", "end.bryant.t"))

MCMC <- 2000000
burn <- 200000
thin <- 360

#MCMC <- 120    #<< trial run
#burn <- 20
#thin <- 1

start.value <- matrix(rnorm(4*nIters, sd = .6), nIters, 4)
chains <- 1:4
seedData <- round(runif(nIters)*1000000,0)

# INSIDE FOREACH() LOOP

output <- foreach(j = (1:nIters),
                .export = c("seedData", 
                            "start.value",
                            "Y",
                            "MCMC",
                            "burn",
                            "thin",
                            "chains")   
                         ) %dopar% {
    set.seed(seedData[j]) # Sets a new seed for each iteration
   
#############################
##     DATA SETUP FOR      ##
##       SECTION 4.5       ##
##  EFFICIENCY COMPARISON  ##
#############################
    
    # NOTE: This code follows "validate-replication-code-data-setup-weighting.R"
    # The goal is to produce 25 randomly sampled datasets for the endorsement arm
    # of the efficiency analysis in Section 4.5
    
    # Load raw survey data
    dat <- read.table("ms_replication_data.txt", header=T) 
    
    ## Recode outcome variables for endorsement question type
    
    # Endorsement
    # control condition
    dat$end.bryant.c <- rep(NA, nrow(dat))
    dat$end.bryant.c[dat$q1aa == "very favorable"] <- 1
    dat$end.bryant.c[dat$q1aa == "somewhat favorable"] <- 2
    dat$end.bryant.c[dat$q1aa == "(vol) no opinion"] <- 3
    dat$end.bryant.c[dat$q1aa == "somewhat unfavorable"] <- 4
    dat$end.bryant.c[dat$q1aa == "very unfavorable"] <- 5
    
    # treatment condition
    dat$end.bryant.t <- rep(NA, nrow(dat))
    dat$end.bryant.t[dat$q1ba == "very favorable"] <- 1
    dat$end.bryant.t[dat$q1ba == "somewhat favorable"] <- 2
    dat$end.bryant.t[dat$q1ba == "(vol) no opinion"] <- 3
    dat$end.bryant.t[dat$q1ba == "somewhat unfavorable"] <- 4
    dat$end.bryant.t[dat$q1ba == "very unfavorable"] <- 5
    
    # Code indicator for each treatment status
    dat$end.treat <- ifelse(is.na(dat$q1ba)==F, 1, 0)
    dat$end.control <- ifelse(is.na(dat$q1aa)==F, 1, 0)
    
    #################
    ### WEIGHTING ###
    #################
    
    ###############################
    ### PREPARE POPULATION DATA ###
    ###############################
    
    ## Load joint distribution of demographic variables for use in weighting procedure
    ## We will focus on: county, party, age, and gender
    ## This data will also be used to produce all regression-adjusted estimates in the paper
    
    joint <- read.csv("ms_counts by county_final.csv")
    
    ### UNPASTE JOINT DIST VARS 
    
    # if factors, convert to characters
    joint_dist <- as.character(joint$joint)
    
    # split on forward slash
    joint_dist <- strsplit(joint_dist, "/", fixed = TRUE)
    
    # find the largest element
    maxLen <- max(sapply(joint_dist, length))
    
    # fill in any blanks. The t() is to transpose the return from sapply
    joint_dist <- 
      t(sapply(joint_dist, function(x)
        # append to x, NA's.  Note that if (0 == (maxLen - length(x))), then no NA's are appended 
        c(x, rep(NA, maxLen - length(x)))
      ))
    
    # add column names as necessary
    colnames(joint_dist) <- c("precinct", "party", "sex", "age")
    
    # Put it all back together
    joint <- data.frame(joint_dist, joint)
    
    ### Paste joint dist with county
    joint$joint_byco <- paste(joint$county, joint$joint, sep = "/")
    
    ##########################################
    ### COLLAPSE POPULATION DATA BY COUNTY ###
    ##########################################
    
    joint.new <- subset(joint, select=c(party, sex, age, county, n_pop))
    
    joint.new$joint_byco <- paste(joint.new$county, joint.new$party, joint.new$sex, joint.new$age, sep="/")
    
    joint.new <- tapply(joint.new$n_pop, joint.new$joint_byco, sum)
    joint.new <- data.frame(joint_byco=row.names(joint.new), n_pop=joint.new)
    
    ### UNPASTE JOINT DIST VARS AGAIN
    
    # if factors, convert to characters
    joint_dist <- as.character(joint.new$joint_byco)
    
    # split on forward slash
    joint_dist <- strsplit(joint_dist, "/", fixed = TRUE)
    
    # find the largest element
    maxLen <- max(sapply(joint_dist, length))
    
    # fill in any blanks. The t() is to transpose the return from sapply
    joint_dist <- 
      t(sapply(joint_dist, function(x)
        # append to x, NA's.  Note that if (0 == (maxLen - length(x))), then no NA's are appended 
        c(x, rep(NA, maxLen - length(x)))
      ))
    
    # add column names as necessary
    colnames(joint_dist) <- c("county", "party", "sex", "age")
    
    # Put it all back together
    joint.new <- data.frame(joint_dist, joint.new)
    
    ###########################
    ### PREPARE SURVEY DATA ###
    ###########################
    
    # Recode missing age in survey data to match voter file
    dat$vf.age <- ifelse(is.na(dat$samp_age_cod)==F, as.character(dat$samp_age_cod), "NA(age)")
    
    # Recode precinct to match voter file
    dat$vf.precinct <- sub("^[0]+", "", dat$precinct_recode)
    
    # Create joint distribution string in survey data for county/party/sex/age (joint_dist) 
    dat$joint_byco <- paste(dat$county, dat$party, dat$sex, dat$vf.age, sep = "/")
    
    # Create numeric dummies with voter file covariates
    dat$maleNA <- ifelse(dat$sex=="M" | dat$sex=="NA(sex)", 1, 0)
    dat$dem <- ifelse(dat$party=="DEM", 1, 0)
    dat$gop <- ifelse(dat$party=="GOP", 1, 0)
    dat$ageNA <- ifelse(is.na(dat$samp_age_cod)==T, 1, 0)
    dat$age55plus <- ifelse(is.na(dat$samp_age_cod)==T, 0, ifelse(dat$samp_age_cod=="55-64" | dat$samp_age_cod=="65plus", 1, 0))
    
    ## Create joint distribution table of demographic variables for use in weighting procedure
    ## Again, we will focus on: county, party, age, and gender.
    sample <- data.frame(table(dat$joint_byco))
    names(sample)[2] <- "n_samp"
    names(sample)[1]  <- "joint_byco"
    head(sample)
    sum(sample$n_samp) #checks out to equal 2655 respondents
    
    ## Merge joint distribution of demographic covariates from survey and population
    wdat <- merge(joint.new, sample, by.x = "joint_byco", by.y = "joint_byco", all.x=T)
    
    # Replace NAs with 0s in n_samp
    wdat$n_samp[is.na(wdat$n_samp)] <- 0
    
    # Create column for failures (i.e. the number we failed to sample, n_pop - n_samp)
    wdat$n_fail <- wdat$n_pop - wdat$n_samp
    
    #####
    ##      DRAW EQUAL SAMPLE SIZE 
    ##                &             
    ##   WEIGHT ENDORSEMENT CONDITION   
    #####
    
    ### SAMPLE
    # Note: Draw 540 from those who got method first, then draw another 400 
    # from those who got method second or third.
    # Then delete missing data and calculate survey weights for remaining sample
    
    # ENDORSEMENT
    e1 <- subset(dat, condition=="1A2AD"
                 | condition=="1A2BD"
                 | condition=="1B2AD"
                 | condition=="1B2BD"
                 | condition=="1ACD"
                 | condition=="1BCD")
    e <- subset(dat, condition=="2A1AD"
                | condition=="2A1BD"
                | condition=="2B1AD"
                | condition=="2B1BD"
                | condition=="C1AD"
                | condition=="C1BD")
    
    first <- e1
    other <- e
    holder <- vector('list', length(first))
    
    a <- first[sample(1:nrow(first), 540, replace=FALSE),]
    b <- other[sample(1:nrow(other), 400, replace=FALSE),]
    
    end.comp <- rbind(a, b)
    end.comp <- subset(end.comp, is.na(end.bryant.c)==F | is.na(end.bryant.t)==F)
    end.comp.c <- subset(end.comp, condition=="1A2AD"
                         | condition=="1A2BD"
                         | condition=="1ACD"
                         | condition=="2A1AD"
                         | condition=="2B1AD"
                         | condition=="C1AD")
    end.comp.t <- subset(end.comp, condition=="1B2AD"
                         | condition=="1B2BD"
                         | condition=="1BCD"
                         | condition=="2A1BD"
                         | condition=="2B1BD"
                         | condition=="C1BD")
    
    holder <- list(end.comp.c, end.comp.t)
    
    ### WEIGHTS FOR ENDORSEMENT CONDITION
    
    weight <- vector('list', length(holder))
    
    for (i in 1:length(holder)){
      cond <- holder[[i]]
      sample <- data.frame(table(cond$joint_byco))
      names(sample)[2] <- "n_samp"
      names(sample)[1]  <- "joint_byco"
      print(sum(sample$n_samp)) #This equals number of respondents in condition
      wdat <- merge(joint.new, sample, by.x = "joint_byco", by.y = "joint_byco", all.x=T)
      wdat$n_samp[is.na(wdat$n_samp)] <- 0
      wdat$n_fail <- wdat$n_pop - wdat$n_samp  
      m1 <- bayesglm(cbind(n_samp, n_fail) ~ age + sex + party + county, family = binomial(link="logit"), data = wdat,
                     prior.scale=2.5, prior.df=1)  
      pr.incl <- predict(m1, type = "response")
      weight[[i]] <- data.frame(joint_byco=names(pr.incl), weight=1/pr.incl)
    }
    
    ### Get dataset with weights for endorsement condition 
    
    wdat.by.cond <- vector('list', length(holder))
    x <- 20
    
    for (i in 1:length(holder)){
      # Placeholder for weight
      holder[[i]]$weight <- NULL
      # Merge weights with data for each condition and subset
      wdat.temp <- merge(holder[[i]], weight[[i]], by.x = "joint_byco", by.y = "joint_byco", all.x=T)
      # Trim weights so that the maximum weight is no more then 20 x the minimum weight
      wdat.temp$weight <- ifelse(wdat.temp$weight>(x*min(wdat.temp$weight, na.rm=T)), x*min(wdat.temp$weight, na.rm=T), wdat.temp$weight)
      # Normalize weights to 1
      wdat.temp$weight <- wdat.temp$weight/sum(wdat.temp$weight, na.rm=T)
      # Store datasets in a list
      wdat.by.cond[[i]] <- wdat.temp
    }
    
    # Merge control and treatment group data for endorsement
    # Add dummy for treatment status
    
    # Endorsement
    wdat.by.cond[[1]]$treat <- rep(0, nrow(wdat.by.cond[[1]]))
    wdat.by.cond[[2]]$treat <- rep(1, nrow(wdat.by.cond[[2]]))
    
    end.comp <- rbind(wdat.by.cond[[1]], wdat.by.cond[[2]])
    
    # Keep necessary variables for analysis
    end.nocov <- subset(end.comp, select=c(end.bryant.c, end.bryant.t, weight))
    
    # Write data
    write.table(end.nocov, file=paste("end-weight-data_iter_", j, ".txt", sep=""))

#####
## Now fit the model running 4 chains from overdispersed starting values
#####

out <- vector('list', length(chains))

for (i in chains){
  out[[i]] <- endorse(Y = Y,
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
                              beta.start = matrix(start.value[j, i], nrow = 1, ncol = 2))

}

save(out, file=paste("endorse_out_comp_iter_", j, ".RData", sep="")) # This is a list of all 4 chains


## Check for convergence using Gelman-Rubin stat
all.list.beta <- mcmc.list(list(as.mcmc(out[[1]]$beta), as.mcmc(out[[2]]$beta),
                                as.mcmc(out[[3]]$beta), as.mcmc(out[[4]]$beta)))     

all.list.lambda <- mcmc.list(list(as.mcmc(out[[1]]$lambda), as.mcmc(out[[2]]$lambda),
                                  as.mcmc(out[[3]]$lambda), as.mcmc(out[[4]]$lambda))) 

all.list.omega2 <- mcmc.list(list(as.mcmc(out[[1]]$omega2), as.mcmc(out[[2]]$omega2),
                                  as.mcmc(out[[3]]$omega2), as.mcmc(out[[4]]$omega2))) 

print(gelman.diag(all.list.beta))
print(gelman.diag(all.list.lambda))
print(gelman.diag(all.list.omega2))


## Combine second half of all four chains for 10k MCMC draws
#z <- 51:100       #<< Trial run
z <- 2501:5000

endorse.out <-NULL

endorse.out$beta <- rbind(as.matrix(out[[1]]$beta[z,]), as.matrix(out[[2]]$beta[z,]), 
                          as.matrix(out[[3]]$beta[z,]), as.matrix(out[[4]]$beta[z,]))

endorse.out$lambda <- rbind(as.matrix(out[[1]]$lambda[z,]), as.matrix(out[[2]]$lambda[z,]), 
                            as.matrix(out[[3]]$lambda[z,]), as.matrix(out[[4]]$lambda[z,]))

endorse.out$omega2 <- rbind(as.matrix(out[[1]]$omega2[z,]), as.matrix(out[[2]]$omega2[z,]), 
                            as.matrix(out[[3]]$omega2[z,]), as.matrix(out[[4]]$omega2[z,]))

endorse.out$s <- rbind(as.matrix(out[[1]]$s[z,]), as.matrix(out[[2]]$s[z,]), 
                       as.matrix(out[[3]]$s[z,]), as.matrix(out[[4]]$s[z,]))

endorse.out$x <- rbind(as.matrix(out[[1]]$x[z,]), as.matrix(out[[2]]$x[z,]), 
                       as.matrix(out[[3]]$x[z,]), as.matrix(out[[4]]$x[z,]))

endorse.out$model.matrix.indiv <- out[[1]]$model.matrix.indiv

# SAVE/RUN LOCALLY
save(endorse.out, file=paste("endorse_out_nocov_combined_chain_comp_iter_", j, ".RData", sep=""))

## UNWEIGHTED

pred <- pnorm(endorse.out$lambda/sqrt(endorse.out$omega2))
end.unweighted <- cbind(mean=mean(pred), low=mean(pred)-qnorm(.975)*sd(pred), up=mean(pred)+qnorm(.975)*sd(pred))
print(end.unweighted)

# Same estimate based on posterior distributiion of s, the support parameter
# 'No' vote estimate is weighted share of positive s_ijk 
# S matrix is 10k x 936 (comparable data)
# rows are MCMC draws, columns are respondents
bryant <- endorse.out$s
dim(bryant)

#Find share of negative support parameter in each row, take average for population support estimate 
#This is in-sample from the posterior distribution from the support parameter
#More negative = more support and that's our yes estimate.
bryant.nonzero <- bryant[, colSums(bryant) != 0] 
dim(bryant.nonzero) #leaves 454 obs (number of obs in treatment condition)

#How many negatives in each row? that is, in each MCMC draw
neg <- apply(bryant.nonzero, 1, function(x) length(x[x<0])) #Note: Can flip inequality for "no" estimate
#Divide by 454 to get share negative in each MCMC draw
share.neg <- neg/ncol(bryant.nonzero)
end.unweighted <- quantile(1-share.neg, prob=c(.5, .025, .975))

## WEIGHTED

# Again, using the posterior distribution of s, the support parameter
bryant <- endorse.out$s
dim(bryant)

# Find share of negative support parameter in each row, take average for population support estimate 
# This is in-sample estimate
# More negative = more support and that's our yes estimate.
bryant.nonzero <- bryant[, colSums(bryant) != 0] 
dim(bryant.nonzero) #leaves 454 obs (number of obs in treatment condition in comparable data)
#Let negatives = 1, positives = 0
bryant.neg1 <- as.matrix(bryant.nonzero)
bryant.neg1[bryant.neg1<0] <- 1
bryant.neg1[bryant.neg1!=1] <- 0
bryant.neg1 <- as.data.frame(bryant.neg1) 

#Create vector of weights for treatment condition only that sum to 1
end.t.weight <- subset(end.comp, treat==1, select=weight)

#Multiply each row by vector of weights
bryant.neg1.weighted <- t(as.matrix(bryant.neg1))*as.vector(t(end.t.weight))
dim(bryant.neg1.weighted)

#Get weighted negative MCMC draws (1x10k)
neg1.weight <- colSums(bryant.neg1.weighted)

#Let negatives = 0, positives = 1
bryant.pos1 <- as.matrix(bryant.nonzero)
bryant.pos1[bryant.pos1<0] <- 0 
bryant.pos1[bryant.pos1!=0] <- 1
bryant.pos1 <- as.data.frame(bryant.pos1) 

#Use vector of weights for treatment condition only, multiply each row by vector of weights
bryant.pos1.weighted <- t(as.matrix(bryant.pos1))*as.vector(t(end.t.weight))
dim(bryant.pos1.weighted)

#Get weighted positive MCMC draws (1x10k)
pos1.weight <- colSums(bryant.pos1.weighted)

#Calculate weighted share of negatives as sum(-)/[sum(+) + sum(-)]
#NOTE: length((neg1.weight/(neg1.weight+pos1.weight))) is 10,000 so we're taking the SD across all posterior draws
draws <- neg1.weight/(neg1.weight+pos1.weight)

end.weighted <- cbind(mean=1-mean(draws), low=1-mean(draws)-qnorm(.975)*sd(draws), up=1-mean(draws)+qnorm(.975)*sd(draws))

end.statewide.results <- rbind(end.unweighted, end.weighted)
rownames(end.statewide.results) <- c("unweighted", "weighted")
write.table(end.statewide.results, paste("endorse-statewide-results-comp_iter_", j, ".txt", sep="")) 

}



## ########################
## Close Everything Cleanly
## ########################

closeCluster(cl)
mpi.quit()

