################
###  SURVEY  ###
### RECODING ###
################

# Load raw survey data
load("ms_replication_data.RData")

## Recode outcome variables for each indirect question type

# Direct
dat$dir <- rep(NA, nrow(dat))
dat$dir[dat$qd == "Voted No"] <- 0
dat$dir[dat$qd == "Voted Yes"] <- 1

# List
dat$list.y <- rep(NA, nrow(dat))
dat$list.y[dat$q2a == "NONE" | dat$q2b == "NONE"] <- 0
dat$list.y[dat$q2a == "One" | dat$q2b == "One"] <- 1
dat$list.y[dat$q2a == "Two" | dat$q2b == "Two"] <- 2
dat$list.y[dat$q2a == "Three" | dat$q2b == "Three"] <- 3
dat$list.y[dat$q2a == "Four" | dat$q2b == "Four"] <- 4
dat$list.y[dat$q2b == "Five"] <- 5

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

# Randomized Response
dat$rr.dum <- rep(NA, nrow(dat))
dat$rr.dum[dat$qc3 == "No"] <- 0
dat$rr.dum[dat$qc3 == "Yes"] <- 1

# Code indicator for each question type and treatment status
dat$list.treat <- ifelse(is.na(dat$q2b)==F, 1, 0)
dat$list.control <- ifelse(is.na(dat$q2a)==F, 1, 0)
dat$end.treat <- ifelse(is.na(dat$q1ba)==F, 1, 0)
dat$end.control <- ifelse(is.na(dat$q1aa)==F, 1, 0)
dat$rr.bin <- ifelse(is.na(dat$rr.dum)==F, 1, 0)
dat$dir.bin <- ifelse(is.na(dat$dir)==F, 1, 0)

## Recode survey-measured covariates for individual-level analysis
## These variables are used in Section 4.6

# Age in years
dat$ageyrs <- ifelse((dat$qage==999 | is.na(dat$qage)), NA, dat$qage)

# Education (create 6-category factor variable)
dat$edu <- rep(NA, nrow(dat))
dat$edu[dat$qeducation == "No schooling completed"
        | dat$qeducation == "Nursery school to 4th grade"
        | dat$qeducation == "5th or 6th grade"
        | dat$qeducation == "7th or 8th grade"
        | dat$qeducation == "9th grade"
        | dat$qeducation == "10th grade"
        | dat$qeducation == "11th grade"
        | dat$qeducation == "12th grade NO DIPLOMA"] <- "1_Less than high school"
dat$edu[dat$qeducation == "HIGH SCHOOL GRADUATE high school DIPLOMA or the equivalent (GED)"] <- "2_High School"
dat$edu[dat$qeducation == "Some college, no degree"] <- "3_Incomplete college"
dat$edu[dat$qeducation == "Associate degree"] <- "4_Associate degree"
dat$edu[dat$qeducation == "Bachelors degree"] <- "5_Bachelors degree"
dat$edu[dat$qeducation == "Masters degree" | dat$qeducation == "Professional or Doctorate degree"] <- "6_Postgrad degree"
dat$edu[dat$qeducation == "(vol) Dont know"] <- NA
dat$edu[dat$qeducation == "(vol) Refused"] <- NA

# Dummy for male gender
dat$male <- ifelse(dat$qsex=="Male", 1, 0)

# Survey-measured party id
# Note: this is different than "party", which comes from the voter file
dat$pid <- rep(NA, nrow(dat))
dat$pid[dat$qpid == "A Democrat"] <- "DEM"
dat$pid[dat$qpid == "A Republican"] <- "REP"
dat$pid[dat$qpid == "An Independent"] <- "IND"
dat$pid[dat$qpid == "Or what?"] <- NA
dat$pid[dat$qpid == "(vol) Dont know"] <- NA
dat$pid[dat$qpid == "(vol) Refused"] <- NA

methods <- c("dat")
data <- vector('list', length(methods))
j <- 0

for (i in methods){
  j <- j+1
  
  # Create dummies for education
  A <- data.frame(model.matrix(~edu - 1, data=get(i)))
  A <- A[match(rownames(get(i)),rownames(A)),]
  rownames(A) <- rownames(get(i))
  names(A) <- c("edu1", "edu2", "edu3", "edu4", "edu5", "edu6")
  data[[j]] <- cbind(get(i), A)
  
  # Create variable for coarse educational categories
  data[[j]]$eduHIGHER <- ifelse(data[[j]]$edu5==1 | data[[j]]$edu6==1, 1, 0)
  data[[j]]$eduPOSTGRAD <- ifelse(data[[j]]$edu6==1, 1, 0)
  
  # Transform ageyrs
  data[[j]]$ageyrs.new <- data[[j]]$ageyrs/10
  data[[j]]$ageyrs2.new <- (data[[j]]$ageyrs/10)^2
  
  # Code dummies for survey-measured pid
  data[[j]]$pidREP <- ifelse(data[[j]]$pid=="REP", 1, 0)
  data[[j]]$pidIND <- ifelse(data[[j]]$pid=="IND", 1, 0)
}

dat <- data[[1]]


## Code item nonresponse dummy for each condition 
## These variables are used in Appendix Tables 4 & 5
# Direct
dat$dir.nr <- rep(NA, nrow(dat))
dat$dir.nr[dat$qd=="(vol) Did not vote"] <- NA
dat$dir.nr[dat$qd=="(vol) Dont know"] <- 1
dat$dir.nr[dat$qd=="(vol) Refused"] <- 1
dat$dir.nr[dat$qd=="Voted No"] <- 0
dat$dir.nr[dat$qd=="Voted Yes"] <- 0 

# List
dat$list.nr <- rep(NA, nrow(dat))
dat$list.nr[dat$q2a=="(vol) Dont know" | dat$q2b=="(vol) Dont know"] <- 1
dat$list.nr[dat$q2a=="(vol) Refused"] <- 1
dat$list.nr[dat$q2a=="Four" | dat$q2b=="Four"] <- 0
dat$list.nr[dat$q2a=="Three" | dat$q2b=="Three"] <- 0
dat$list.nr[dat$q2a=="Two" | dat$q2b=="Two"] <- 0
dat$list.nr[dat$q2a=="One" | dat$q2b=="One"] <- 0
dat$list.nr[dat$q2a=="NONE" | dat$q2b=="NONE"] <- 0
dat$list.nr[dat$q2b=="Five"] <- 0

# Endorsement
dat$end.nr <- rep(NA, nrow(dat))
dat$end.nr[dat$q1aa=="(vol) Refused"] <- 1
dat$end.nr[dat$q1aa=="somewhat favorable"] <- 0
dat$end.nr[dat$q1aa=="somewhat unfavorable"] <- 0
dat$end.nr[dat$q1aa=="(vol) no opinion"] <- 0
dat$end.nr[dat$q1aa=="very favorable"] <- 0
dat$end.nr[dat$q1aa=="very unfavorable"] <- 0

dat$end.nr[dat$q1ba=="(vol) Refused"] <- 1
dat$end.nr[dat$q1ba=="somewhat favorable"] <- 0
dat$end.nr[dat$q1ba=="somewhat unfavorable"] <- 0
dat$end.nr[dat$q1ba=="(vol) no opinion"] <- 0
dat$end.nr[dat$q1ba=="very favorable"] <- 0
dat$end.nr[dat$q1ba=="very unfavorable"] <- 0

#RRT
dat$rr.nr <- rep(NA, nrow(dat))
dat$rr.nr[dat$qc3=="(vol) Refused"] <- 1
dat$rr.nr[dat$qc3=="No"] <- 0
dat$rr.nr[dat$qc3=="Yes"] <- 0

## Code indicator for "no" response on turnout practice question
## This variable is used in Appendix Figure 8
dat$turnout.no <- rep(NA, nrow(dat))
dat$turnout.no[dat$qc1=="No"] <- 1
dat$turnout.no[dat$qc1=="Yes"] <- 0
dat$turnout.no[is.na(dat$qc1)==TRUE | dat$qc1=="(vol) Refused"] <- NA

#################
### WEIGHTING ###
#################

###############################
### PREPARE POPULATION DATA ###
###############################

## Load joint distribution of demographic variables for use in weighting procedure
## We will focus on: county, party, age, and gender
## This data will also be used to produce all regression-adjusted estimates in the paper

load("ms_counts by county_final.RData")

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


### Construct population data from joint distribution with         
### 1 row for each member of the population       
### We'll use this data for the regression-adjusted estimates 

# Unpack population data so that 1 row represents one case in population
freq <- as.vector(joint$n_pop)
sum(freq) #285860 individuals in pop data
pop.full <- joint[rep(1:nrow(joint),freq),] 

# Recode variables to indicators for key voter file demographics
pop.full$maleNA <- ifelse(pop.full$sex=="M" | pop.full$sex=="NA(sex)", 1, 0)
pop.full$dem <- ifelse(pop.full$party=="DEM", 1, 0)
pop.full$gop <- ifelse(pop.full$party=="GOP", 1, 0)
pop.full$other <- ifelse(pop.full$party=="OTHER", 1, 0)
pop.full$ageNA <- ifelse(pop.full$age=="NA(age)", 1, 0)
pop.full$age55plus <- ifelse(pop.full$age=="55-64" | pop.full$age=="65plus", 1, 0)

write.table(pop.full, file="population_data_final.txt") #NOTE: this is one row per case in population with N=285,860

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
table(wdat$n_fail, useNA=('ifany')) # Check: There are no negative cells.

### Now, we use a Bayesian glm approach to weighting
### We weight each condition separately to look more like the population
### In this first step, we create weights for each condition using all data

conditions <- c("dir.bin", "list.control", "list.treat", "end.control", "end.treat", "rr.bin")
weight <- vector('list', length(conditions))

for (i in 1:length(conditions)){
  cond <- subset(dat, get((conditions)[i])==1)
  sample <- data.frame(table(cond$joint_byco))
  names(sample)[2] <- "n_samp"
  names(sample)[1]  <- "joint_byco"
  print(sum(sample$n_samp)) #This should equal number of respondents in condition
  wdat <- merge(joint.new, sample, by.x = "joint_byco", by.y = "joint_byco", all.x=T)
  wdat$n_samp[is.na(wdat$n_samp)] <- 0
  wdat$n_fail <- wdat$n_pop - wdat$n_samp  
  m1 <- bayesglm(cbind(n_samp, n_fail) ~ age + sex + party + county, family = binomial(link="logit"), data = wdat,
                 prior.scale=2.5, prior.df=1) # Cauchy prior with scale 2.5 (conservative choice recommended in Gelman et al (2008))
  pr.incl <- predict(m1, type = "response")
  weight[[i]] <- data.frame(joint_byco=names(pr.incl), weight=1/pr.incl)
}

### Get datasets with weights for each condition

dat$weight <- NULL
wdat.by.cond <- vector('list', length(conditions))
x <- 20

for (i in 1:length(conditions)){
  # Merge weights with data for each condition and subset
  temp.dat <- merge(dat, weight[[i]], by.x = "joint_byco", by.y = "joint_byco", all.x=T)
  wdat.temp <- subset(temp.dat, get((conditions)[i])==1)
  # Trim weights so that the maximum weight is no more then 20 x the minimum weight
  wdat.temp$weight <- ifelse(wdat.temp$weight>(x*min(wdat.temp$weight, na.rm=T)), x*min(wdat.temp$weight, na.rm=T), wdat.temp$weight)
  # Normalize weights to 1
  wdat.temp$weight <- wdat.temp$weight/sum(wdat.temp$weight, na.rm=T)
  # Store datasets in a list
  wdat.by.cond[[i]] <- wdat.temp
}

# Check Data
quantile(wdat.by.cond[[2]]$weight, probs=c((seq(0, 1, .1))), na.rm=T)
nrow(wdat.by.cond[[2]])

# Merge control and treatment group data for list and endorsement
# Add dummy for treatment status

# List
wdat.by.cond[[2]]$treat <- rep(0, nrow(wdat.by.cond[[2]]))
wdat.by.cond[[3]]$treat <- rep(1, nrow(wdat.by.cond[[3]]))
# Endorsement
wdat.by.cond[[4]]$treat <- rep(0, nrow(wdat.by.cond[[4]]))
wdat.by.cond[[5]]$treat <- rep(1, nrow(wdat.by.cond[[5]]))

# Save Datasets
dir <- wdat.by.cond[[1]]
list <- rbind(wdat.by.cond[[2]], wdat.by.cond[[3]])
end <- rbind(wdat.by.cond[[4]], wdat.by.cond[[5]])
rr <- wdat.by.cond[[6]]

write.table(dir, file="dir-weight.txt")
write.table(list, file="list-weight.txt")
write.table(end, file="end-weight.txt")
write.table(rr, file="rr-weight.txt")



#############################
##     DATA SETUP FOR      ##
##       SECTION 4.5       ##
##  EFFICIENCY COMPARISON  ##
##    STATEWIDE RESULTS    ##
#############################

#####
## DRAW EQUAL SAMPLE SIZES 
##           &             
##   WEIGHT BY CONDITION   
#####

### SAMPLE
# Note: Draw 540 from those who got method first, then draw another 400 
# from those who got method second or third.
# For direct, take all 360 from first and draw remaining 580 from those who got question second or third
# Then delete missing data and calculate survey weights for remaining sample

# DIRECT
d1 <- subset(dat, condition=="D")  
d <- subset(dat, condition!="D")
d <- d[sample(1:nrow(d), 580, replace=FALSE),] 
dir.comp <- rbind(d1, d)
dir.comp <- subset(dir.comp, is.na(dir)==F)

# LIST
l1 <- subset(dat, condition=="2A1AD"
             | condition=="2A1BD"
             | condition=="2AD"
             | condition=="2B1AD"
             | condition=="2B1BD"
             | condition=="2BD")
l <- subset(dat, condition=="1A2AD"
            | condition=="1A2BD"
            | condition=="1B2AD"
            | condition=="1B2BD")

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

# RR
r1 <- subset(dat, condition=="C1AD" 
             | condition=="C1BD")
r <- subset(dat, condition=="1ACD"
            | condition=="1BCD")

first <- list(l1, e1, r1)
other <- list(l, e, r)
holder <- vector('list', length(first))

for (i in 1:length(first)){
  a <- first[[i]][sample(1:nrow(first[[i]]), 540, replace=FALSE),]
  b <- other[[i]][sample(1:nrow(other[[i]]), 400, replace=FALSE),]
  holder[[i]] <- rbind(a, b)  
}

list.comp <- holder[[1]]
list.comp <- subset(list.comp, is.na(list.y)==F)
list.comp.c <- subset(list.comp, condition=="1A2AD"
                      | condition=="1B2AD"
                      | condition=="2A1AD"
                      | condition=="2A1BD"
                      | condition=="2AD")
list.comp.t <- subset(list.comp, condition=="1A2BD"
                      | condition=="1B2BD"
                      | condition=="2B1AD"
                      | condition=="2B1BD"
                      | condition=="2BD")

end.comp <- holder[[2]]
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

rr.comp <- holder[[3]]
rr.comp <- subset(rr.comp, is.na(rr.dum)==F)
holder <- list(dir.comp, list.comp.c, list.comp.t, end.comp.c, end.comp.t, rr.comp)

### WEIGHTS FOR EACH CONDITION - (as above, but with equal sample sizes)

weight <- vector('list', length(holder))

for (i in 1:length(holder)){
  cond <- holder[[i]]
  sample <- data.frame(table(cond$joint_byco))
  names(sample)[2] <- "n_samp"
  names(sample)[1]  <- "joint_byco"
  print(sum(sample$n_samp)) #This should equal number of respondents in condition
  wdat <- merge(joint.new, sample, by.x = "joint_byco", by.y = "joint_byco", all.x=T)
  wdat$n_samp[is.na(wdat$n_samp)] <- 0
  wdat$n_fail <- wdat$n_pop - wdat$n_samp  
  m1 <- bayesglm(cbind(n_samp, n_fail) ~ age + sex + party + county, family = binomial(link="logit"), data = wdat,
                 prior.scale=2.5, prior.df=1)  
  pr.incl <- predict(m1, type = "response")
  weight[[i]] <- data.frame(joint_byco=names(pr.incl), weight=1/pr.incl)
}

### Get datasets with weights for each condition 

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

# Check Data
quantile(wdat.by.cond[[2]]$weight, probs=c((seq(0, 1, .1))), na.rm=T)
nrow(wdat.by.cond[[2]])

# Merge control and treatment group data for list and endorsement
# Add dummy for treatment status

# List
wdat.by.cond[[2]]$treat <- rep(0, nrow(wdat.by.cond[[2]]))
wdat.by.cond[[3]]$treat <- rep(1, nrow(wdat.by.cond[[3]]))
# Endorsement
wdat.by.cond[[4]]$treat <- rep(0, nrow(wdat.by.cond[[4]]))
wdat.by.cond[[5]]$treat <- rep(1, nrow(wdat.by.cond[[5]]))

# Save Datasets
dir.comp <- wdat.by.cond[[1]]
list.comp <- rbind(wdat.by.cond[[2]], wdat.by.cond[[3]])
end.comp <- rbind(wdat.by.cond[[4]], wdat.by.cond[[5]])
rr.comp <- wdat.by.cond[[6]]

write.table(dir.comp, file="dir-weight-comp.txt")
write.table(list.comp, file="list-weight-comp.txt")
write.table(end.comp, file="end-weight-comp.txt")
write.table(rr.comp, file="rr-weight-comp.txt")

