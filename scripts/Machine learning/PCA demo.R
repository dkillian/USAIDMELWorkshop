# BPI Baseline 
# Module E Household Assets

modE <- read.csv("scripts/Machine learning/modE.csv")

modEnames <- codebook(modE)

Desc(modE$ec1)
Desc(modE$e2_1)
Desc(modE$ec2)
Desc(modE$e2_2)
Desc(modE$ec3)


modE$Radio <- modE$e2_1
modE$TV_BW <- modE$e2_2
modE$TV_Col <- modE$e2_3
modE$Computer <- modE$e2_4
modE$Mobile <- modE$e2_5
modE$CDPlayer <- modE$e2_6
modE$Clock_watch <- modE$e2_7
modE$Fan <- modE$e2_8
modE$Iron <- modE$e2_9
modE$SolarPanel <- modE$e2_10
modE$Generator <- modE$e2_11
modE$Bicycle <- modE$e2_12
modE$Rickshaw <- modE$e2_13
modE$TricycleVan <- modE$e2_14
modE$Boat <- modE$e2_15
modE$Motorcycle <- modE$e2_16
modE$Car <- modE$e2_17
modE$Stove <- modE$e2_18
modE$Buckets <- modE$e2_19
modE$Pots <- modE$e2_20
modE$SewingMachine <- modE$e2_21
modE$Bed <- modE$e2_22
modE$Cabinet <- modE$e2_23
modE$Table <- modE$e2_24
modE$Trunk <- modE$e2_25
modE$Net <- modE$e2_26
modE$Saw <- modE$e2_27
modE$Hammer <- modE$e2_28
modE$Spade <- modE$e2_29
modE$Axe <- modE$e2_30
modE$Shovel <- modE$e2_31
modE$Sickle <- modE$e2_32
modE$Weeder <- modE$e2_33
modE$Harrower <- modE$e2_34
modE$Plough <- modE$e2_35
modE$Cow <- modE$e2_36
modE$Goat <- modE$e2_37
modE$Duck <- modE$e2_38
modE$Donkey <- modE$e2_39
modE$PestSprayer <- modE$e2_40
modE$Pushcart <- modE$e2_41
modE$Tiller <- modE$e2_42
modE$HandTubewell <- modE$e2_43
modE$LowLiftPump <- modE$e2_44
modE$ShallowTubewell <- modE$e2_45
modE$ElectricPump <- modE$e2_46
modE$Chicken <- modE$e2_47
modE$Quail <- modE$e2_48
modE$Pigeon <- modE$e2_49
modE$ChargerLight <- modE$e2_50
modE$TorchLight <- modE$e2_51
modE$Buffalo <- modE$e2_52
modE$Refrigerator <- modE$e2_53
modE$DressingTable <- modE$e2_54
modE$SofaSet <- modE$e2_55
modE$Pig <- modE$e2_56

assets <- dplyr::select(modE, hid:res_type2, Radio:Pig, -Donkey)
assetNames <- codebook(assets)

fa.parallel(assets[,8:62], main="Parallel analysis for asset index")

a <- principal(assets[,8:62], nfactors=1, rotate="none")
a
a[5]
a$weights


b <- principal(assets[,8:62], nfactors=1, rotate="varimax")
b

assets$Score <- as.vector(round(a$scores,2))
head(assets$Score)
attributes(assets$Score) <- NULL
psych::describe(assets$Score)
str(assets$Score)

library(gtools)
assets$Rating <- as.numeric(quantcut(assets$Score, seq(0,1,.2)))
str(assets$Rating)
Desc(assets$Rating)
table(assets$Rating)
levels(assets$Rating)

ggplot(assets, aes(Rating, Score)) + geom_point() + stat_smooth(se=F)
cor(assets$Score, assets$Ladder)


Desc(assets$l1)
Desc(dat$l1)
Desc(assets$Ladder)
cor(assets$l1, assets$Ladder)

inc <- read.csv("Total household annual income.csv")
assets <- merge(assets, inc)
str(assets)
assets <- dplyr::select(assets, hid:res_type2, Score:TotaldailyPPP, Radio:Pig)
assets <- dplyr::arrange(assets, hid)


ggplot(assets, aes(Ladder, TotaldailyUSD)) + geom_point() + stat_smooth(se=F) + scale_x_continuous(breaks=1:9)
cor(assets$Ladder, assets$TotaldailyUSD)
Desc(assets$dailyUSD)



?par
par(mfrow=c(2,1))
plot(density(a$scores))
plot(density(a4Scores$RC1))

?quantile
?quantileCut

?cut2
?cut

cor(assets$Rating, dat$l1)
plot(dat$l1, assets$Rating)

assets$Ladder <- dat$l1
ggplot(assets, aes(x=Ladder, y=Rating)) + geom_point() + stat_smooth(se=F)

assets$Quant <- quantile(assets$Score, breaks=c(.2, .4, .6, .8, 1))

cut(meaneduc02v, breaks=c(quantile(meaneduc02v,  probs = seq(0,        1, by=0.20), 
                                   na.rm=TRUE, names=TRUE, include.lowest=TRUE, right = TRUE, 
                                   labels=c("1","2","3","4","5")))) 

a4 <- principal(assets[,8:62], nfactors=4)
a4
a4Scores <- data.frame(a4$scores)
a4Scores.r <- data.frame(a4$r.scores)


plot(a4$values,type="both",ylab="Eigenvalue",xlab="Component Number", main="Scree Plot")

plot(a4$values,type="both",ylab="Eigenvalue",xlab="Component Number", main="Scree Plot",axes=FALSE)
axis(side=1,c(0:5))
axis(side=2,seq(0,2.5,0.5))
axis(side=3,c(0,5))
axis(side=4,c(0,2.5))

a4Scores <- data.frame(a4$scores)
psych::describe(a4Scores)

table(assets$dist)
assets$District <- factor(assets$dist, labels=c("Sylhet","Habiganj","Moulavibazar"))

table(assets$upazila)
assets$Subdistrict <- factor(assets$upazila, labels=c("Sylhet Sadar","Nabigonj","Sreemangal","Kamolganj"))
table(assets$Subdistrict)

unionLabel <- c("Khadimpara","Tukerbazar","Tultikor","Auskandi","Digholbak","Inathganj","Kalapur","Kamolganj", "Sreemongal")
table(assets$union)
assets$Union <- factor(assets$union, labels=unionLabel)
table(assets$Union)

assets$Female <- ifelse(assets$gender==2, 1, 0)
Desc(assets$Female)

table(assets$res_type1)
str(assets$res_type1)
assets$SurveyType <- factor(assets$res_type1, labels=c("Household survey", "Beneficiary survey"))
table(assets$SurveyType)

assets$Treatment <- ifelse(assets$res_type1==4, 1, 0)
table(assets$Treatment)

table(assets$res_type2)
assets$RespondentType <- factor(assets$res_type2, labels=c("VDO Member","Non VDO Member","Microloan","Seed funding","No support"))
table(assets$RespondentType)

assets <- dplyr::select(assets, hid, District, Subdistrict, Union, Female, SurveyType, RespondentType, Score:Pig)
assetNames <- codebook(assets)

assets$Rating2 <- as.numeric(quantcut(assets$Score, seq(0,1,.1)))
assets <- select(assets, hid:Rating, Rating2, everything())

assets$IncomeScale <- as.numeric(quantcut(assets$TotaldailyUSD, seq(0,1,.1)))
assets <- select(assets, hid:Ladder, IncomeScale, everything(), -l1)

write.csv(assets, "Income and assets.csv", row.names=F)

assets <- read.csv("Income and assets.csv")

things <- assets %>% 
  dplyr::select(Radio:Pig) %>%
  describe() %>%
  mutate(Measure=row.names(.), Mean=round(mean,3), SD=round(sd,3)) %>%
  select(., Measure, Mean, SD)
things$FactorWeight <- round(a$weights,3)
attributes(things$FactorWeight) <- NULL
str(things)
things <- arrange(things, desc(FactorWeight))
things
write.csv(things, "Asset incidence and factor weights.csv", row.names=F)

things <- read.csv("Asset incidence and factor weights.csv")

# Asset incidence and ratings ---------------------------------------------

AssetRating <- assets %>%
  group_by(Rating) %>%
  summarise(Radio=mean(Radio), TV_BW=mean(TV_BW), TV_Col=mean(TV_Col), Computer=mean(Computer), Mobile=mean(Mobile), CDPlayer=mean(CDPlayer), 
            Clock_watch=mean(Clock_watch), Fan=mean(Fan), Iron=mean(Iron), SolarPanel=mean(SolarPanel), Generator=mean(Generator), 
            Bicycle=mean(Bicycle), Rickshaw=mean(Rickshaw), TricycleVan=mean(TricycleVan), Boat=mean(Boat), Motorcycle=mean(Motorcycle), 
            Car=mean(Car), Stove=mean(Stove), Buckets=mean(Buckets), Pots=mean(Pots), SewingMachine=mean(SewingMachine), Bed=mean(Bed), 
            Cabinet=mean(Cabinet), Table=mean(Table), Trunk=mean(Trunk), Net=mean(Net), Saw=mean(Saw), Hammer=mean(Hammer), Spade=mean(Spade),
            Axe=mean(Axe), Shovel=mean(Shovel), Sickle=mean(Sickle), Weeder=mean(Weeder), Harrower=mean(Harrower), Plough=mean(Plough), 
            Cow=mean(Cow), Goat=mean(Goat), Duck=mean(Duck), PestSprayer=mean(PestSprayer), Pushcart=mean(Pushcart), Tiller=mean(Tiller), 
            HandTubewell=mean(HandTubewell), LowLiftPump=mean(LowLiftPump), ShallowTubewell=mean(ShallowTubewell), 
            ElectricPump=mean(ElectricPump), Chicken=mean(Chicken), Quail=mean(Quail), Pigeon=mean(Pigeon), ChargerLight=mean(ChargerLight), 
            TorchLight=mean(TorchLight), Buffalo=mean(Buffalo), Refrigerator=mean(Refrigerator), DressingTable=mean(DressingTable), 
            SofaSet=mean(SofaSet), Pig=mean(Pig)) 
AssetRating
write.csv(AssetRating, "Asset incidence by rating - assets as columns.csv", row.names=F)

AssetRating <- read.csv("Asset incidence by rating - assets as columns.csv", row.names=F)

ARlong <- gather(AssetRating, item, ownership, -Rating)
head(ARlong)
write.csv(ARlong, "Asset incidence by rating - long.csv", row.names=F)

?melt
?gather
?spread

ARwide <- gather(AssetRating, item, ownership, -Rating) %>%
  spread(Rating, ownership)
head(ARwide)
write.csv(ARwide, "Asset incidence by rating - wide")

par(mfcol=c(2,1))
?par

a <- ggplot(assets, aes(x=Rating)) + 
  stat_smooth(aes(y=Table, color="Table"), se=F) +
  stat_smooth(aes(y=Fan, color="Fan"), se=F) + 
  stat_smooth(aes(y=Mobile, color="Mobile"), se=F) + 
  stat_smooth(aes(y=Bed, color="Bed"), se=F) + 
  stat_smooth(aes(y=Cabinet, color="Cabinet"), se=F) +
  scale_color_manual("", 
                     breaks=c("Table","Fan", "Mobile","Bed","Cabinet"), 
                     values=c("darkblue","maroon", "darkgreen","purple","darkgoldenrod3")) + 
  ylab("") + xlab("") + scale_x_continuous(breaks=NULL) +
  theme(legend.position=c(.2,.77))


b <- ggplot(assets, aes(x=Rating)) + 
  stat_smooth(aes(y=SofaSet, color="SofaSet"), se=F) +
  stat_smooth(aes(y=Refrigerator, color="Refrigerator"), se=F) + 
  stat_smooth(aes(y=DressingTable, color="DressingTable"), se=F) + 
  stat_smooth(aes(y=ChargerLight, color="ChargerLight"), se=F) + 
  stat_smooth(aes(y=Trunk, color="Trunk"), se=F) +
  scale_color_manual("", 
                     breaks=c("SofaSet","Refrigerator", "DressingTable","ChargerLight","Trunk"), 
                     values=c("darkblue","maroon", "darkgreen","purple","darkgoldenrod")) + 
  ylab("") + 
  theme(legend.position=c(.2,.77))

multiplot(a,b)


ggplot(assets, aes(x=Ladder, y=Rating)) + geom_point() + stat_smooth(se=F)

Rat <- assets %>%
  group_by(Ladder) %>%
  summarise(Rating=mean(Rating))
Rat
ggplot(Rat, aes(Ladder, Rating)) + geom_point() + stat_smooth(se=F) + 
  scale_x_continuous(breaks=1:9) + scale_y_continuous(breaks=0:5)

dailyUSD <- assets %>%
  group_by(Ladder) %>%
  summarise(dailyUSD=mean(TotaldailyUSD))
dailyUSD

lad <- merge(Rat, dailyUSD)
lad

ggplot(lad, aes(Ladder)) + 
  stat_smooth(aes(y=Rating, color="Asset-based rating (1-5)"), se=F) + 
  stat_smooth(aes(y=dailyUSD, color="Household daily income (USD)"), se=F) +
  ylim(0,18) + ylab("") + xlab("Perceived economic situation") + scale_x_continuous(breaks=1:9) + theme(legend.position=c(.2,.9)) +
  scale_color_manual("",
                     breaks=c("Asset-based rating (1-5)", "Household daily income (USD)"), 
                     values=c("darkblue","maroon"))


# Asset scores by gas plant -----------------------------------------------

library(gtools)


mb <- filter(assets, District=="Moulavibazar")
jb <- filter(assets, District=="Sylhet")
bb <- filter(assets, District=="Habiganj")

names(mb)

a <- lapply(mb[,20:74], sum)
a

mb <- dplyr::select(mb, hid, Radio:Pig, -Generator)
mbPC <- principal(select(mb, -hid))
mb$mbScore <- mbPC$scores
str(mb$mbScore)
attributes(mb$mbScore) <- NULL
mb$mbRating <- as.numeric(quantcut(mb$mbScore, seq(0,1,.2)))
str(mb$mbRating)
mb$mbRating2 <- as.numeric(quantcut(mb$mbScore, seq(0,1,.1)))
str(mb$mbRating2)

b <- lapply(jb[,20:74], sum)
b
which(b==0)
b[40]
b[41]
b[44]

jb <- dplyr::select(jb, hid, Radio:Pig, -Pushcart, -Tiller, -ShallowTubewell)
jbPC <- principal(select(jb, -hid))
jb$jbScore <- jbPC$scores
str(jb$jbScore)
attributes(jb$jbScore) <- NULL
jb$jbRating <- as.numeric(quantcut(jb$jbScore, seq(0,1,.2)))
str(jb$jbRating)
jb$jbRating2 <- as.numeric(quantcut(jb$jbScore, seq(0,1,.1)))
str(jb$jbRating2)


c <- lapply(bb[,20:74], sum)
which(c==0)
bb <- dplyr::select(bb, Radio:Pig, -LowLiftPump, -ElectricPump, -Pig)
bbPC <- principal(bb)
bb$bbScore <- bbPC$scores
str(bb$bbScore)
attributes(bb$bbScore) <- NULL
bb$bbRating <- as.numeric(quantcut(bb$bbScore, seq(0,1,.2)))
str(bb$bbRating)
bb$bbRating2 <- as.numeric(quantcut(bb$bbScore, seq(0,1,.1)))
str(bb$bbRating2)

## incidence and factor weights by gas plant

thingsMB <- mb %>%
  dplyr::select(Radio:Pig) %>%
  describe() %>%
  mutate(mbMeasure=row.names(.), mbMean=round(mean,3), mbSD=round(sd,3)) %>%
  select(., mbMeasure, mbMean, mbSD)
thingsMB
thingsMB$FactorWeight <- round(mbPC$weights,3)
str(thingsMB$FactorWeight)
attributes(thingsMB$FactorWeight) <- NULL
thingsMB <- arrange(thingsMB, desc(FactorWeight))
write.csv(thingsMB, "Tables/income/Asset incidence and factor weights - MB.csv", row.names=F)

thingsJB <- jb %>%
  dplyr::select(Radio:Pig) %>%
  describe() %>%
  mutate(jbMeasure=row.names(.), jbMean=round(mean,3), jbSD=round(sd,3)) %>%
  select(., jbMeasure, jbMean, jbSD)
thingsJB
thingsJB$FactorWeight <- round(jbPC$weights,3)
str(thingsJB$FactorWeight)
attributes(thingsJB$FactorWeight) <- NULL
thingsJB <- arrange(thingsJB, desc(FactorWeight))
write.csv(thingsJB, "Tables/income/Asset incidence and factor weights - JB.csv", row.names=F)


thingsBB <- bb %>%
  dplyr::select(Radio:SofaSet) %>%
  describe() %>%
  mutate(bbMeasure=row.names(.), bbMean=round(mean,3), bbSD=round(sd,3)) %>%
  select(., bbMeasure, bbMean, bbSD)
thingsBB
thingsBB$FactorWeight <- round(bbPC$weights,3)
str(thingsBB$FactorWeight)
attributes(thingsBB$FactorWeight) <- NULL
thingsBB <- arrange(thingsBB, desc(FactorWeight))
write.csv(thingsBB, "Tables/income/Asset incidence and factor weights - BB.csv", row.names=F)




# income scales together --------------------------------------------------



Desc(assets$IncomeScale)

# loess

ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), se=F) + 
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), se=F) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), se=F) + 
  scale_color_manual("", 
                     breaks=c("Deciles of household daily income","Asset-based rating", "Perceived economic situation"), 
                     values=c("darkblue", "maroon","darkgoldenrod3")) + 
  ylab("") + scale_y_continuous(breaks=1:15) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.8,.2))

ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), se=F) + 
  geom_point(aes(y=IncomeScale, color="Deciles of household daily income")) +
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), se=F) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), se=F) + 
  scale_color_manual("", 
                     breaks=c("Deciles of household daily income","Asset-based rating", "Perceived economic situation"), 
                     values=c("darkblue", "maroon","darkgoldenrod3")) + 
  ylab("") + scale_y_continuous(breaks=1:15) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.8,.2))

ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), se=F) + 
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), se=F) + 
  geom_point(aes(y=Rating2, color="Asset-based rating")) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), se=F) + 
  scale_color_manual("", 
                     breaks=c("Deciles of household daily income","Asset-based rating", "Perceived economic situation"), 
                     values=c("darkblue", "maroon","darkgoldenrod3")) + 
  ylab("") + scale_y_continuous(breaks=1:15) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.8,.2))

ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), se=F) + 
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), se=F) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), se=F) + 
  geom_point(aes(y=Ladder, color="Perceived economic situation")) +
  scale_color_manual("", 
                     breaks=c("Deciles of household daily income","Asset-based rating", "Perceived economic situation"), 
                     values=c("darkblue", "maroon","darkgoldenrod3")) + 
  ylab("") + scale_y_continuous(breaks=1:15) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.8,.2))


# linear

ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), method="lm", se=F) + 
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), method="lm", se=F) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), method="lm", se=F) + 
  stat_smooth(aes(y=PPIScale, color="Deciles of Progress out of Poverty Index"), method="lm", se=F) +
  scale_color_manual("", 
                     breaks=c("Deciles of household daily income","Asset-based rating", "Perceived economic situation", "Deciles of Progress out of Poverty Index"), 
                     values=c("darkblue", "maroon","darkgoldenrod3", "purple")) + 
  ylab("") + scale_y_continuous(limits=c(1,10), breaks=1:15) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.76,.2))

Desc(assets$TotaldailyUSD)


ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), se=F) + 
  geom_point(aes(y=IncomeScale, color="Deciles of household daily income")) +
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), se=F) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), se=F) + 
  scale_color_manual("", 
                     breaks=c("Deciles of household daily income","Asset-based rating", "Perceived economic situation"), 
                     values=c("darkblue", "maroon","darkgoldenrod3")) + 
  ylab("") + scale_y_continuous(breaks=1:15) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.8,.2))

ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), se=F) + 
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), se=F) + 
  geom_point(aes(y=Rating2, color="Asset-based rating")) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), se=F) + 
  scale_color_manual("", 
                     breaks=c("Deciles of household daily income","Asset-based rating", "Perceived economic situation"), 
                     values=c("darkblue", "maroon","darkgoldenrod3")) + 
  ylab("") + scale_y_continuous(breaks=1:15) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.8,.2))

ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), se=F) + 
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), se=F) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), se=F) + 
  geom_point(aes(y=Ladder, color="Perceived economic situation")) +
  scale_color_manual("", 
                     breaks=c("Deciles of household daily income","Asset-based rating", "Perceived economic situation"), 
                     values=c("darkblue", "maroon","darkgoldenrod3")) + 
  ylab("") + scale_y_continuous(breaks=1:15) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.8,.2))


?gganimate

# individual graphs with points - linear and loess

a <- ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income"), method="lm",se=F) + 
  geom_point(aes(y=IncomeScale, color="Deciles of household daily income")) + 
  stat_smooth(aes(y=IncomeScale, color="Deciles of household daily income")) +
  scale_color_manual("", breaks="Deciles of household daily income", values="maroon") + 
  ylab("") + scale_y_continuous(limits=c(1,10), breaks=1:10) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.7,.6)) 
a

b <- ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), method="lm",se=F) + 
  geom_point(aes(y=Rating2, color="Asset-based rating")) + 
  stat_smooth(aes(y=Rating2, color="Asset-based rating"), se=F) + 
  scale_color_manual("", breaks="Asset-based rating", values="darkblue") + 
  ylab("") + scale_y_continuous(limits=c(1,10), breaks=1:10) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.7,.45)) 
b

c <- ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=PPIScale, color="Deciles of Progress out of Poverty Index"), method="lm",se=F) + 
  geom_point(aes(y=PPIScale, color="Deciles of Progress out of Poverty Index")) +
  stat_smooth(aes(y=PPIScale, color="Deciles of Progress out of Poverty Index"), se=F) + 
  scale_color_manual("", breaks="Deciles of Progress out of Poverty Index", values="darkgoldenrod3") + 
  ylab("") + scale_y_continuous(limits=c(1,10), breaks=1:10) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.7,.45)) 
c

d <- ggplot(assets, aes(x=TotaldailyUSD)) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), method="lm",se=F) + 
  geom_point(aes(y=Ladder, color="Perceived economic situation")) + 
  stat_smooth(aes(y=Ladder, color="Perceived economic situation"), se=F) +
  scale_color_manual("", breaks="Perceived economic situation", values="purple") + 
  ylab("") + scale_y_continuous(limits=c(1,10), breaks=1:10) + xlab("Total household daily income (USD)") +
  theme(legend.position=c(.7,.45)) 
d

multiplot(a,b,c,d, cols=2)
?multiplot






# income ------------------------------------------------------------------

inc <- describe(dplyr::select(assets, TotalannualTaka:TotaldailyPPP))
inc
write.csv(inc, "Aggregate income.csv", row.names=F)

inc2 <- dplyr::select(assets, TotalannualTaka:TotaldailyPPP)
str(inc2)
sjt.df(as.data.frame(inc2))

surv <- assets %>%
  group_by(SurveyType) %>%
  summarise(Taka=mean(TotalannualTaka), USD=mean(TotalannualUSD), PPP=mean(TotalannualPPP), dailyUSD=mean(TotaldailyUSD), 
            dailyPPP=mean(TotaldailyPPP))
surv

survDist <- assets %>%
  group_by(SurveyType, District) %>%
  summarise(Taka=mean(TotalannualTaka), USD=mean(TotalannualUSD), PPP=mean(TotalannualPPP), dailyUSD=mean(TotaldailyUSD), 
            dailyPPP=mean(TotaldailyPPP))
survDist

survDistM <- melt(survDist)
survDistM

ggplot(survDistM[1:6,], aes(x=value, y=District, color=SurveyType)) + geom_point(size=2.5) +
  scale_color_manual(values=c("blue", "firebrick3")) + xlab("") + ylab("") + 
  scale_x_continuous(limits=c(130000,210000), labels=comma)



EvM <- ggplot(DistEvM, aes(x=value, y=variable, color=District)) + geom_point(size=3) + 
  scale_color_manual(values=c("firebrick3","darkblue","darkgreen")) + ylab("") + xlab("") + 
  scale_y_discrete(breaks=c("Effective","Satisfied"), 
                   labels=c("VDO effectiveness","VDO operations")) +
  scale_x_continuous(limits=c(3,4), 
                     breaks=c(3,3.25, 3.5, 3.75,4),
                     labels=c("Somewhat \nsatisfied","3.25", "3.5","3.75","Very \nsatisfied"))  +
  ggtitle("Current state\n(Rating 1-4)")



