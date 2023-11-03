# Load library packages
library(rJava)
library(xlsxjars)
library(xlsx)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(repr)
library(moderndive)

# Change plot size to 3 x 3; default 7x7
# Point size to 8; default 12
# Digits to 3; default 7
options(repr.plot.width=7, repr.plot.height=7, repr.plot.pointsize=8, digits=3)

# Custom colours
d3      <- c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", "#9467BDFF", "#8C564BFF", "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF")

# Functions

add.alpha <- function(col, alpha=1){
  if(missing(col)) stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))  
}

# Load Ammonium data
dat <- read.csv("data/Data_Hubot_et_al_2021.csv", header = T)

# remove outliers
dat$Nitrite[dat$ID == "hysoscella-1T1" & dat$time == 240] <- mean(dat$Nitrite[dat$ID == "hysoscella-1T1" & dat$time %in% c(180, 300)])
dat$Nitrate[dat$ID == "hysoscella-1T1" & dat$time == 240] <- mean(dat$Nitrate[dat$ID == "hysoscella-1T1" & dat$time %in% c(180, 300)])
dat$Phosphate[dat$ID == "hysoscella-1T1" & dat$time == 240] <- mean(dat$Phosphate[dat$ID == "hysoscella-1T1" & dat$time %in% c(180, 300)])

dat$Nitrite[dat$ID == "hysoscella-1T4" & dat$time == 240] <- mean(dat$Nitrite[dat$ID == "hysoscella-1T4" & dat$time %in% c(180, 300)])
dat$Nitrate[dat$ID == "hysoscella-1T4" & dat$time == 240] <- mean(dat$Nitrate[dat$ID == "hysoscella-1T4" & dat$time %in% c(180, 300)])
dat$Phosphate[dat$ID == "hysoscella-1T4" & dat$time == 240] <- mean(dat$Phosphate[dat$ID == "hysoscella-1T4" & dat$time %in% c(180, 300)])
remove <- which(dat$ID=="fulgida-2T1")
dat <- dat[-remove,]

dat$Treatment <- as.factor(substr(dat$Replicate, 1,2)) # Create a new variable
levels(dat$Treatment)
# put time in hours
dat$time <- dat$time/60
dat$Species <- as.factor(dat$Species)

# ----------------------------- Ploting the data --------------------------------
###  Ammonium
par(mfrow=c(2,2))
for (i in levels(dat$Species)) {
  # Call empty plot (type='n')
  plot(dat$Ammonium~dat$time, main=i, ylab ="Ammonium concentration (?M)", xlab="time (h)", ylim=c(-2,20), type='n', cex.lab=1.4)
  
  # Extract only the species you are interested in
  dummy <- dat[dat$Species==i,]
  
  # Inner loop to plot the individual lines
  for(k in dummy$Replicate){
    # Inner loop that only calls one replicate at a time.
    inner <- dummy[dummy$Replicate==k,]   
    COL <- d3[inner$Treatment]
    lines(Ammonium~time, data=inner, col=COL)
  }
}

legend("topleft", col=d3[c(1,3,2,4)], lwd=1, cex=0.7, legend=c("Jellyfish","Control", "Microbes + spike", "Spike control"))

# ----------------------------- Control corrections --------------------

### Correction for ammonium changes

# plot
par(mfrow=c(1,1))
pch_sp <- c(1,17,3,0)
plot(Ammonium ~ time, type='n', dat=dat[dat$Treatment=="C1",], ylab="Ammonium concentration (?M)", xlab="time (h)", ylim=c(-2.9, 1.4), cex.lab=1.4 )

# names of controls
controls <- c("aurita-C1","fulgida-C1","hysoscella-C1","pacifica-C1")

# make matrix to save results from linear regression
mat <- matrix(dat=NA, nrow=length(controls), ncol=4)

# counter
counter <- 1

# loop for linear regression of each individual control
for(i in controls){
  points(Ammonium ~ time, dat[dat$Treatment=="C1" & dat$ID==i,], lwd=2.5, col=d3[counter],pch=pch_sp[counter],cex=1.6)
  lines(Ammonium ~ time, dat[dat$Treatment=="C1" & dat$ID==i,], lwd=2.5, col=d3[counter])
  
  lm_loop <- lm(Ammonium ~ time, dat[dat$Treatment=="C1" & dat$ID==i,])
  mat[counter,1] <- coef(lm_loop)[1]
  mat[counter,2] <- coef(lm_loop)[2]
  mat[counter,3] <- summary(lm_loop)$adj
  mat[counter,4] <- round(summary(lm_loop)$coef[2,4]*1000)/1000
  counter <- counter+1
}

# add legend to plot
legend("topleft", bty="n", cex=1.2, pt.cex=1.6, text.font=3, col=d3[1:4], pt.bg=d3[1:4], lwd=2.5, pch=pch_sp[1:4], merge=TRUE, legend=c("A. aurita","C. fulgida","C. hysoscella","C. pacifica"), border = NA)

# save output as dataframe
df <- as.data.frame(mat)
colnames(df) <- c("Intercept","Slope","R2","p")
rownames(df) <- controls
print(df)

# Add control correction
dat$Ammonium_control_corrected <- dat$Ammonium
dat$Control_correct <- 0

# for pacifica (apply to all pacifica treatments):
dat$Control_correct[dat$Species=="pacifica"] <- dat$time[dat$Species=="pacifica"]*mat[3,2]

# for flugida (apply to all fulgida treatments):
dat$Control_correct[dat$Species=="fulgida"] <- dat$time[dat$Species=="fulgida"]*mat[4,2]

# Applying correction:
dat$Ammonium_control_corrected <- dat$Ammonium_control_corrected - dat$Control_correct

### Correction for volume loss

release <- dat[dat$Treatment=="1T",]
release$Ammonium_release_ind <- NA

# make new column
release$Ammonium_release_ind <- NA

for (i in unique(release$ID)){
  # pull out index of rows for this replicate
  r <- which(release$ID==i)
  
  # make sure the rows are sorted according to time
  r <- r[order(release[r,]$time)]   
  # you can now call your 'subset' of data using "release[r,]". Think of it as: sub <- release[r,]
  
  # set release to 0 for time 0
  release[r,]$Ammonium_release_ind[1]  <- 0
  
  # loop through every row.
  for(k in 2:length(r)){
    release[r,]$Ammonium_release_ind[k] <- release[r,]$Ammonium_release_ind[k-1] + release[r,]$Vol_incub[k] * (release[r,]$Ammonium_control_corrected[k] - release[r,]$Ammonium_control_corrected[k-1])
  }
}

# display table. Comment this column out if you don't want to see the individual tables.
print(knitr::kable(release[r,][,c("Species","time","ID","Ammonium","Ammonium_control_corrected","Ammonium_release_ind")]))

# ----------------------- Rates calcualtions -------------------------

# Plot the data
par(mfrow=c(2,2))
for (i in levels(release$Species)) {
  # Call empty plot (type='n')
  plot(release$Ammonium_release_ind~release$time, main=i, ylab ="Ammonium release (?mol/ind)", xlab="time (min)", type='n')
  
  # Extract only the species you are interested in
  dummy <- release[release$Species==i,]
  
  # Inner loop to plot the individual lines
  for(k in dummy$Replicate){
    # Inner loop that only calls one replicate at a time.
    inner <- dummy[dummy$Replicate==k,]   
    COL <- d3[inner$Treatment]
    lines(Ammonium_release_ind~time, data=inner, col=COL)
  }
}
# ----------------------- Weight Specific calculations -----------------------
# Creating a new column
release$Ammonium_release_WW <- release$Ammonium_release_ind/release$WW
release$Ammonium_release_WW <- release$Ammonium_release_WW*1000

# calculate mean according to species and time
agg        <- aggregate(release$Ammonium_release_WW, by=list(release$time, release$Species), mean)
names(agg) <- c("Time","Species","Ammonium_release_WW")

# calculate standard deviation
agg$sd     <- aggregate(release$Ammonium_release_WW, by=list(release$time, release$Species), sd)$x

# calculate envelope
agg$lwr    <- agg$Ammonium_release_WW - agg$sd
agg$upr    <- agg$Ammonium_release_WW + agg$sd

# plot
par(mfrow=c(1,1))
# reset counter for colour
counter <- 1
pch_sp <- c(1,17,3,0)

# Call empty plot (type='n')
plot(agg$Ammonium_release_WW~agg$Time, ylim=c(0,570), bty="l", ylab ="Mean ammonium release (nmol/gWW)", xlab="Time (h)", type='n',cex.lab=1.3)

for (i in levels(agg$Species)) {    
  
  sub <- agg[agg$Species==i,]
  
  # add envelope
  polygon(x=c(sub$Time, rev(sub$Time)), y=c(sub$upr, rev(sub$lwr)), col=add.alpha(d3[counter], alpha=0.4), border=NA)
  
  # change counter
  counter <- counter+1
}

# plot
counter <- 1
for (i in levels(agg$Species)) {    
  
  sub <- agg[agg$Species==i,]
  
  # add line
  lines(Ammonium_release_WW ~ Time, data=sub, lwd=2, col=d3[counter])
  
  # add points
  points(Ammonium_release_WW ~ Time, data=sub, cex=1.5, lwd=2.5, pch=pch_sp[counter], col=d3[counter])
  
  # change counter
  counter <- counter+1
}
abline(v=6,lty=2, col="black")
legend(x=-0.2, y=620, bty="n", cex=1.3, pt.cex=1.5, text.font=3, col=d3[1:4], pt.bg=d3[1:4], lwd=2.5, pch=pch_sp[1:4], merge=TRUE, legend=c("A. aurita","C. fulgida","C. hysoscella","C. pacifica"), fill=add.alpha(d3[1:4], 0.4), border = NA)

# -------------  Nitrite ---------

# Plot the data
par(mfrow=c(2,2))
for (i in levels(dat$Species)) {
  # Call empty plot (type='n')
  plot(dat$Nitrite~dat$time, main=i, ylab ="Nitrite concentration (?M)", xlab="time (h)", type='n')
  
  # Extract only the species you are interested in
  dummy <- dat[dat$Species==i,]
  
  # Inner loop to plot the individual lines
  for(k in dummy$Replicate){
    # Inner loop that only calls one replicate at a time.
    inner <- dummy[dummy$Replicate==k,]   
    COL <- d3[inner$Treatment]
    lines(Nitrite~time, data=inner, col=COL)
  }
}

legend("topleft", col=d3[c(1,3,2,4)], cex=0.8, lwd=1, legend=c("Jellyfish","Control", "Microbes + spike", "Spike control"))

# ------------ Control correction ---------------

# plot
par(mfrow=c(1,1))
plot(Nitrite ~ time, type='n', dat=dat[dat$Treatment=="C1",], ylab="Nitrite concentration (?M)", xlab="time (h)", ylim=c(0,1.2), cex.lab=1.4)

# names of controls
controls <- c("aurita-C1","fulgida-C1","hysoscella-C1","pacifica-C1")

# make matrix to save results from linear regression
mat <- matrix(dat=NA, nrow=length(controls), ncol=4)

# counter
counter <- 1

# loop for linear regression of each individual control
for(i in controls){
  points(Nitrite ~ time, dat[dat$Treatment=="C1" & dat$ID==i,], lwd=2.5, col=d3[counter],pch=pch_sp[counter],cex=1.6)
  lines(Nitrite ~ time, dat[dat$Treatment=="C1" & dat$ID==i,], lwd=2.5, col=d3[counter])
  
  lm_loop <- lm(Nitrite ~ time, dat[dat$Treatment=="C1" & dat$ID==i,])
  mat[counter,1] <- coef(lm_loop)[1]
  mat[counter,2] <- coef(lm_loop)[2]
  mat[counter,3] <- summary(lm_loop)$adj
  mat[counter,4] <- round(summary(lm_loop)$coef[2,4]*1000)/1000
  counter <- counter+1
}

# add legend to plot
legend("topleft", bty="n", cex=1.2, pt.cex=1.6, text.font=3, col=d3[1:4], pt.bg=d3[1:4], lwd=2.5, pch=pch_sp[1:4], merge=TRUE, legend=c("A. aurita","C. fulgida","C. hysoscella","C. pacifica"), border = NA)

# save output as dataframe
df <- as.data.frame(mat)
colnames(df) <- c("Intercept","Slope","R2","p")
rownames(df) <- controls

### No correction: changes are negligible! 

# -------------------- Calculating release per individuals ------------------
# make new column
release$Nitrite_release_ind <- NA

for (i in unique(release$ID)){
  # pull out index of rows for this replicate
  r <- which(release$ID==i)
  
  # make sure the rows are sorted according to time
  r <- r[order(release[r,]$time)]   
  # you can now call your 'subset' of data using "release[r,]". Think of it as: sub <- release[r,]
  
  # set release to 0 for time 0
  release[r,]$Nitrite_release_ind[1]  <- 0
  
  # loop through every row.
  for(k in 2:length(r)){
    release[r,]$Nitrite_release_ind[k] <- release[r,]$Nitrite_release_ind[k-1] + release[r,]$Vol_incub[k] * (release[r,]$Nitrite[k] - release[r,]$Nitrite[k-1])
  }
  
  # display table. Comment this column out if you don't want to see the individual tables.
  #print(knitr::kable(release[r,][,c("Species","time","ID","Nitrite","Prod_no2","Nitrite_release_ind")]))
  
}

# Plot the data
par(mfrow=c(2,2))
for (i in levels(release$Species)) {
  # Call empty plot (type='n')
  plot(release$Nitrite_release_ind~release$time, main=i, ylab ="Nitrite release (?mol/ind)", xlab="time (h)", type='n')
  
  # Extract only the species you are interested in
  dummy <- release[release$Species==i,]
  
  # Inner loop to plot the individual lines
  for(k in dummy$Replicate){
    # Inner loop that only calls one replicate at a time.
    inner <- dummy[dummy$Replicate==k,]   
    COL <- d3[inner$Treatment]
    lines(Nitrite_release_ind~time, data=inner, col=COL)
  }
}
#print(knitr::kable(release[release$Species=="hysoscella",][,c("ID","time","Nitrite_release_ind","Nitrite_release_WW")]))

# ----------------------- Weight Specific calculations -----------------------

# Creating a new column
release$Nitrite_release_WW <- release$Nitrite_release_ind/release$WW
release$Nitrite_release_WW <- release$Nitrite_release_WW*1000

# calculate mean according to species and time
agg        <- aggregate(release$Nitrite_release_WW, by=list(release$time, release$Species), mean)
names(agg) <- c("Time","Species","Nitrite_release_WW")

# calculate standard deviation
agg$sd     <- aggregate(release$Nitrite_release_WW, by=list(release$time, release$Species), sd)$x

# calculate envelope
agg$lwr    <- agg$Nitrite_release_WW - agg$sd
agg$upr    <- agg$Nitrite_release_WW + agg$sd

# counter for colour
counter <- 1

# Call empty plot (type='n')
par(mfrow=c(1,1))
plot(agg$Nitrite_release_WW~agg$Time, bty="l", ylim=c(0,30), ylab ="Mean nitrite release (nmol/gWW)", xlab="Time (h)", type='n', cex.lab=1.3)

# plot
for (i in levels(agg$Species)) {    
  
  sub <- agg[agg$Species==i,]
  
  # add envelope
  polygon(x=c(sub$Time, rev(sub$Time)), y=c(sub$upr, rev(sub$lwr)), col=add.alpha(d3[counter], alpha=0.4), border=NA)
  
  # change counter
  counter <- counter+1
}

# plot
counter <- 1
for (i in levels(agg$Species)) {    
  
  sub <- agg[agg$Species==i,]
  
  # add line
  lines(Nitrite_release_WW ~ Time, data=sub, lwd=2, col=d3[counter])
  
  # add points
  points(Nitrite_release_WW ~ Time, data=sub, cex=1.5, lwd=2.5, pch=pch_sp[counter], col=d3[counter])
  
  # change counter
  counter <- counter+1
}

abline(v=6,lty=2, col="black")
legend(x=-0.2, y=33, bty="n", cex=1.3, pt.cex=1.5, text.font=3, col=d3[1:4], pt.bg=d3[1:4], lwd=2.5, pch=pch_sp[1:4], merge=TRUE, legend=c("A. aurita","C. fulgida","C. hysoscella","C. pacifica"), fill=add.alpha(d3[1:4], 0.4), border = NA)

# -------------  Nitrate ---------

# Plot the data
par(mfrow=c(2,2))
for (i in levels(dat$Species)) {
  # Call empty plot (type='n')
  plot(dat$Nitrate~dat$time, main=i, ylab ="Nitrate concentration (?M)", xlab="time (h)", type='n')
  
  # Extract only the species you are interested in
  dummy <- dat[dat$Species==i,]
  
  # Inner loop to plot the individual lines
  for(k in dummy$Replicate){
    # Inner loop that only calls one replicate at a time.
    inner <- dummy[dummy$Replicate==k,]   
    COL <- d3[inner$Treatment]
    lines(Nitrate~time, data=inner, col=COL)
  }
}

legend("topleft", col=d3[c(1,3,2,4)],cex=0.7, lwd=1, legend=c("Jellyfish","Control", "Microbes + spike", "Spike control"))

# ------------ Control correction ---------------
# plot
par(mfrow=c(1,1))
plot(Nitrate ~ time, type='n',bty="l", dat=dat[dat$Treatment=="C1",], ylab="Nitrate concentration (?M)", xlab="time (h)", ylim=c(0,27), cex.lab=1.3)

# names of controls
controls <- c("aurita-C1","fulgida-C1","hysoscella-C1","pacifica-C1")

# make matrix to save results from linear regression
mat <- matrix(dat=NA, nrow=length(controls), ncol=4)

# counter
counter <- 1

# loop for linear regression of each individual control
for(i in controls){
  points(Nitrate ~ time, dat[dat$Treatment=="C1" & dat$ID==i,], lwd=2.5, col=d3[counter],pch=pch_sp[counter],cex=1.5)
  lines(Nitrate ~ time, dat[dat$Treatment=="C1" & dat$ID==i,], lwd=2.5, col=d3[counter])
  
  lm_loop <- lm(Nitrate ~ time, dat[dat$Treatment=="C1" & dat$ID==i,])
  mat[counter,1] <- coef(lm_loop)[1]
  mat[counter,2] <- coef(lm_loop)[2]
  mat[counter,3] <- summary(lm_loop)$adj
  mat[counter,4] <- round(summary(lm_loop)$coef[2,4]*1000)/1000
  counter <- counter+1
}

# add legend to plot
legend(x=-0.2, y=29, bty="n", cex=1.3, pt.cex=1.5, text.font=3, col=d3[1:4], pt.bg=d3[1:4], lwd=2.5, pch=pch_sp[1:4], merge=TRUE, legend=c("A. aurita","C. fulgida","C. hysoscella","C. pacifica"), border = NA)

# save output as dataframe
df <- as.data.frame(mat)
colnames(df) <- c("Intercept","Slope","R2","p")
rownames(df) <- controls

# -------------------- Calculating release per individuals ------------------
# make new column
release$Nitrate_release_ind <- NA

for (i in unique(release$ID)){
  # pull out index of rows for this replicate
  r <- which(release$ID==i)
  
  # make sure the rows are sorted according to time
  r <- r[order(release[r,]$time)]   
  # you can now call your 'subset' of data using "release[r,]". Think of it as: sub <- release[r,]
  
  # set release to 0 for time 0
  release[r,]$Nitrate_release_ind[1]  <- 0
  
  # loop through every row.
  for(k in 2:length(r)){
    release[r,]$Nitrate_release_ind[k] <- release[r,]$Nitrate_release_ind[k-1] + release[r,]$Vol_incub[k] * (release[r,]$Nitrate[k] - release[r,]$Nitrate[k-1])
  }
}
# ----------------------- Weight Specific calculations -----------------------
# Creating a new column
release$Nitrate_release_WW <- release$Nitrate_release_ind/release$WW
release$Nitrate_release_WW <- release$Nitrate_release_WW*1000

# calculate mean according to species and time
agg        <- aggregate(release$Nitrate_release_WW, by=list(release$time, release$Species), mean)
names(agg) <- c("Time","Species","Nitrate_release_WW")

# calculate standard deviation
agg$sd     <- aggregate(release$Nitrate_release_WW, by=list(release$time, release$Species), sd)$x

# calculate envelope
agg$lwr    <- agg$Nitrate_release_WW - agg$sd
agg$upr    <- agg$Nitrate_release_WW + agg$sd

# counter for colour
counter <- 1

# Call empty plot (type='n')
plot(agg$Nitrate_release_WW~agg$Time, bty="l", ylim=c(-5,300), ylab ="Mean nitrate release (nmol/gWW)", xlab="Time (h)", type='n', cex.lab=1.3)

# plot
for (i in levels(agg$Species)) {    
  
  sub <- agg[agg$Species==i,]
  
  # add envelope
  polygon(x=c(sub$Time, rev(sub$Time)), y=c(sub$upr, rev(sub$lwr)), col=add.alpha(d3[counter], alpha=0.4), border=NA)
  
  # change counter
  counter <- counter+1
}

# plot
counter <- 1
for (i in levels(agg$Species)) {    
  
  sub <- agg[agg$Species==i,]
  
  # add line
  lines(Nitrate_release_WW ~ Time, data=sub, lwd=2, col=d3[counter])
  
  # add points
  points(Nitrate_release_WW ~ Time, data=sub, cex=1.5, lwd=2.5, pch=pch_sp[counter], col=d3[counter])
  
  # change counter
  counter <- counter+1
}

abline(v=6,lty=2, col="Black")
legend(x=-0.2, y=325, bty="n", cex=1.3, pt.cex=1.5, text.font=3, col=d3[1:4], pt.bg=d3[1:4], lwd=2.5, pch=pch_sp[1:4], merge=TRUE, legend=c("A. aurita","C. fulgida","C. hysoscella","C. pacifica"), fill=add.alpha(d3[1:4], 0.4), border = NA)

# ------------------ Phosphate ------------------
# Plot the data
par(mfrow=c(2,2))
for (i in levels(dat$Species)) {
  # Call empty plot (type='n')
  plot(dat$Phosphate~dat$time, main=i, ylab ="Phosphate concentration (?M)", xlab="time (h)", type='n')
  
  # Extract only the species you are interested in
  dummy <- dat[dat$Species==i,]
  
  # Inner loop to plot the individual lines
  for(k in dummy$Replicate){
    # Inner loop that only calls one replicate at a time.
    inner <- dummy[dummy$Replicate==k,]   
    COL <- d3[inner$Treatment]
    lines(Phosphate~time, data=inner, col=COL)
  }
}

legend("topleft", cex=0.7,col=d3[c(1,3,2,4)], lwd=1, legend=c("Jellyfish","Control", "Microbes + spike", "Spike control"))

# ------------ Control correction ---------------
# plot
par(mfrow=c(1,1))
plot(Phosphate ~ time, bty="l", type='n', dat=dat[dat$Treatment=="C1",], ylab="Phosphate concentration (?M)", xlab="time (h)", cex.lab=1.3, ylim=c(-0.05,1.8))

# names of controls
controls <- c("aurita-C1","fulgida-C1","hysoscella-C1","pacifica-C1")

# make matrix to save results from linear regression
mat <- matrix(dat=NA, nrow=length(controls), ncol=4)

# counter
counter <- 1

# loop for linear regression of each individual control
for(i in controls){
  points(Phosphate ~ time, dat[dat$Treatment=="C1" & dat$ID==i,], lwd=2.5, col=d3[counter],pch=pch_sp[counter],cex=1.5)
  lines(Phosphate ~ time, dat[dat$Treatment=="C1" & dat$ID==i,], lwd=2.5, col=d3[counter])
  
  lm_loop <- lm(Phosphate ~ time, dat[dat$Treatment=="C1" & dat$ID==i,])
  mat[counter,1] <- coef(lm_loop)[1]
  mat[counter,2] <- coef(lm_loop)[2]
  mat[counter,3] <- summary(lm_loop)$adj
  mat[counter,4] <- round(summary(lm_loop)$coef[2,4]*1000)/1000
  counter <- counter+1
}

# add legend to plot
legend(x=-0.2, y=2, bty="n", cex=1.3, pt.cex=1.5, text.font=3, col=d3[1:4], pt.bg=d3[1:4], lwd=2.5, pch=pch_sp[1:4], merge=TRUE, legend=c("A. aurita","C. fulgida","C. hysoscella","C. pacifica"), border = NA)

# save output as dataframe
df <- as.data.frame(mat)
colnames(df) <- c("Intercept","Slope","R2","p")
rownames(df) <- controls
# -------------------- Calculating release per individuals ------------------
# make new column
release$Phosphate_release_ind <- NA

for (i in unique(release$ID)){
  # pull out index of rows for this replicate
  r <- which(release$ID==i)
  
  # make sure the rows are sorted according to time
  r <- r[order(release[r,]$time)]   
  # you can now call your 'subset' of data using "release[r,]". Think of it as: sub <- release[r,]
  
  # set release to 0 for time 0
  release[r,]$Phosphate_release_ind[1]  <- 0
  
  # loop through every row.
  for(k in 2:length(r)){
    release[r,]$Phosphate_release_ind[k] <- release[r,]$Phosphate_release_ind[k-1] + release[r,]$Vol_incub[k] * (release[r,]$Phosphate[k] - release[r,]$Phosphate[k-1])
  }
}
# ----------------------- Weight Specific calculations -----------------------
# Creating a new column
release$Phosphate_release_WW <- release$Phosphate_release_ind/release$WW
release$Phosphate_release_WW <- release$Phosphate_release_WW*1000

# calculate mean according to species and time
agg        <- aggregate(release$Phosphate_release_WW, by=list(release$time, release$Species), mean)
names(agg) <- c("Time","Species","Phosphate_release_WW")

# calculate standard deviation
agg$sd     <- aggregate(release$Phosphate_release_WW, by=list(release$time, release$Species), sd)$x

# calculate envelope
agg$lwr    <- agg$Phosphate_release_WW - agg$sd
agg$upr    <- agg$Phosphate_release_WW + agg$sd

# counter for colour
counter <- 1

# Call empty plot (type='n')
plot(agg$Phosphate_release_WW~agg$Time,bty="l", ylim=c(0,85), ylab ="Mean phosphate release (nmol/gWW)", xlab="Time (h)", type='n', cex.lab=1.3)

# plot
for (i in levels(agg$Species)) {    
  
  sub <- agg[agg$Species==i,]
  
  # add envelope
  polygon(x=c(sub$Time, rev(sub$Time)), y=c(sub$upr, rev(sub$lwr)), col=add.alpha(d3[counter], alpha=0.4), border=NA)
  
  # change counter
  counter <- counter+1
}

# plot
counter <- 1
for (i in levels(agg$Species)) {    
  
  sub <- agg[agg$Species==i,]
  
  # add line
  lines(Phosphate_release_WW ~ Time, data=sub, lwd=2, col=d3[counter])
  
  # add points
  points(Phosphate_release_WW ~ Time, data=sub, cex=1.5, lwd=2.5, pch=pch_sp[counter], col=d3[counter])
  
  # change counter
  counter <- counter+1
}

abline(v=6,lty=2, col="black")
legend(x=-0.2,y=92, bty="n", cex=1.3, pt.cex=1.5, text.font=3, col=d3[1:4], pt.bg=d3[1:4], lwd=2.5, pch=pch_sp[1:4], merge=TRUE, legend=c("A. aurita","C. fulgida","C. hysoscella","C. pacifica"), fill=add.alpha(d3[1:4], 0.4), border = NA)

Phosphate <- recordPlot()
#knitr::kable(agg)
release_WW <- release

# ------------------------------- Weight-specific rates: linear regressions --------------
release$time <- release$time
# loop through each species
for(i in levels(release$Species)){
  
  # make matrix
  mat <- matrix(ncol=5, nrow=4)
  rownames(mat) <- c("Ammonium","Phosphate","Nitrite","Nitrate")
  colnames(mat) <- c("Slope","SD","R2","p","n")
  
  lm1 <- lm(Ammonium_release_WW ~ time, dat=release[release$Species==i & release$time<6.5,])
  lm2 <- lm(Phosphate_release_WW ~ time, dat=release[release$Species==i & release$time<6.5,])
  lm3 <- lm(Nitrite_release_WW ~ time, dat=release[release$Species==i & release$time<6.5,])
  lm4 <- lm(Nitrate_release_WW ~ time, dat=release[release$Species==i & release$time<6.5,])
  
  mat[1, 1] <- coef(lm1)[2]
  mat[2, 1] <- coef(lm2)[2]
  mat[3, 1] <- coef(lm3)[2]
  mat[4, 1] <- coef(lm4)[2]
  
  mat[1, 2] <- summary(lm1)$coef[2,2]
  mat[2, 2] <- summary(lm2)$coef[2,2]
  mat[3, 2] <- summary(lm3)$coef[2,2]
  mat[4, 2] <- summary(lm4)$coef[2,2]
  
  mat[1, 3] <- summary(lm1)$adj.r.squared
  mat[2, 3] <- summary(lm2)$adj.r.squared
  mat[3, 3] <- summary(lm3)$adj.r.squared
  mat[4, 3] <- summary(lm4)$adj.r.squared
  
  mat[1, 4] <- round(summary(lm1)$coef[2,4]*1000)/1000
  mat[2, 4] <- round(summary(lm2)$coef[2,4]*1000)/1000
  mat[3, 4] <- round(summary(lm3)$coef[2,4]*1000)/1000
  mat[4, 4] <- round(summary(lm4)$coef[2,4]*1000)/1000
  
  mat[1, 5] <- nobs(lm1)
  mat[2, 5] <- nobs(lm2)
  mat[3, 5] <- nobs(lm3)
  mat[4, 5] <- nobs(lm4)
  
  assign(i, mat)
  
}

#write.csv(aurita, "lm_aurita2.csv")
#write.csv(fulgida, "lm_fulgida2.csv")
#write.csv(hysoscella, "lm_hysoscella2.csv")
#write.csv(pacifica, "lm_pacifica2.csv")

all_rates <- rbind(aurita,fulgida,hysoscella,pacifica)
print(all_rates)
#write.csv(all_rates,"all_rates.csv")
#print(aurita)
#print(fulgida)
#print(hysoscella)
#print(pacifica)

# ---------------------------- Barplots --------------------
# take mean for each species
agg                     <- aggregate(release$Ammonium_release_WW, by=list(release$time, release$Species), mean)
names(agg)              <- c("Time","Species","Ammonium_release_WW")
agg$Nitrite_release_WW  <- aggregate(release$Nitrite_release_WW, by=list(release$time, release$Species), mean)$x
agg$Nitrate_release_WW  <- aggregate(release$Nitrate_release_WW, by=list(release$time, release$Species), mean)$x
agg$Phosphate_release_WW  <- aggregate(release$Phosphate_release_WW, by=list(release$time, release$Species), mean)$x

# make matrix
mat <- matrix(ncol=4, nrow=4)
colnames(mat) <- levels(agg$Species)
rownames(mat) <- c("Ammonium","Phosphate","Nitrite","Nitrate")

# loop through each species
for(i in levels(agg$Species)){
  sub <- agg[agg$Species==i,]
  
  lm1 <- lm(Ammonium_release_WW ~ Time, dat=sub[sub$Time<6.5,])
  lm2 <- lm(Phosphate_release_WW ~ Time, dat=sub[sub$Time<6.5,])
  lm3 <- lm(Nitrite_release_WW ~ Time, dat=sub[sub$Time<6.5,])
  lm4 <- lm(Nitrate_release_WW ~ Time, dat=sub[sub$Time<6.5,])
  
  mat[1, i] <- coef(lm1)[2]
  mat[2, i] <- coef(lm2)[2]
  mat[3, i] <- coef(lm3)[2]
  mat[4, i] <- coef(lm4)[2]
}
colnames(mat) <- c("A. aurita","C. fulgida","C. hysoscella","C. pacifica")
#mat[3,2] <- NA
mat_rates <- mat
mat

# ------------ Effect of temperature: Rates at 16?C (Q10) ---------
mat_rates16 <- mat_rates
mat_rates16[,1] <- mat_rates16[,1]*(3.1^((16-15)/10))
mat_rates16[,2] <- mat_rates16[,2]*(2.66^((16-14)/10))
mat_rates16[,3] <- mat_rates16[,3]*(2.66^((16-20)/10))
mat_rates16[,4] <- mat_rates16[,4]*(2.66^((16-16)/10))

mat_rates16[3:4,1] <- mat_rates[3:4,1]*(2.2^((16-15)/10))
mat_rates16[3:4,2] <- mat_rates[3:4,2]*(2.2^((16-14)/10))
mat_rates16[3:4,3] <- mat_rates[3:4,3]*(2.2^((16-20)/10))
mat_rates16[3:4,4] <- mat_rates[3:4,4]*(2.2^((16-16)/10))

#mat_rates16[3,1] <- 0
#mat_rates16[4,2] <- 0
mat_rates_h <- mat_rates
write.csv(mat_rates_h, "Rates_h.csv",  row.names = T)

mat_rates16_h <- mat_rates16
write.csv(mat_rates16_h, "Rates_16_h.csv",  row.names = T)
mat_rates

# Changing column order
mat_per_h <- mat_rates16
mat_per_h[2,] <- mat_rates16[3,]
mat_per_h[3,] <- mat_rates16[4,]
mat_per_h[4,] <- mat_rates16[2,]
row.names(mat_per_h) <- c("Ammonium", "Nitrite", "Nitrate", "Phosphate")

COL <- d3[c(8,11,10)]

# Changing column order
mat_rates <- mat_rates16[c(1,3,4),]
row.names(mat_rates) <- c("Ammonium", "Nitrite", "Nitrate")
mat_rates[3,2] <- 0
mat_rates[2,1]<- 0
print(mat_rates)

# ---------------------- Barplot N rates ----------------
par(mar=c(5,5,4,2))
barplot(mat_rates, ylab=expression("Nitrogen release (nmol "~gWW^{-1}~ h^{-1}~")"), legend=T, args.legend=list(bty="n", cex=1.3, x=1.7, y=140), ylim=c(0,140),
        col=c("white","black","grey"), border="black", cex.lab=1.3, cex.names=1.2, yaxt="n", font=3)
axis(2)

# ---------------------- Barplot N rates proportion ----------------
par(mar=c(5,5,4,2))
barplot(prop.table(mat_rates, 2), col=c("white","black","grey"), border="black", ylab="Proportion of nitrogen release", cex.lab=1.3, cex.names=1.2, yaxt="n", font=3)
axis(2)

# ---------------------- Allometric figures ----------------
### Ammonium
release <- release_WW

# names of replicates
replicates <- unique(release$ID)

# make matrix to save results from linear regression
mat <- matrix(dat=NA, nrow=length(replicates), ncol=7)

# counter
counter <- 1

# loop for linear regression of each individual control
for(i in replicates){
  
  lm_loop <- lm(Ammonium_release_WW ~ time, release[release$ID==i & release$time<6.5,])
  ww_extract <- release[release$ID==i,]
  mat[counter,1] <- coef(lm_loop)[1]
  mat[counter,2] <- coef(lm_loop)[2]
  mat[counter,3] <- summary(lm_loop)$adj
  mat[counter,4] <- round(summary(lm_loop)$coef[2,4]*1000)/1000
  mat[counter,5] <- unique(ww_extract$WW)
  mat[counter,6] <- summary(lm_loop)$coef[2,2]
  #mat[counter,6] <- unique(ww_extract$Species)
  counter <- counter+1
}

# save output as dataframe
df <- as.data.frame(mat)
colnames(df) <- c("Intercept","Slope","R2","p","WW","SD","Species")
rownames(df) <- replicates
df$Species <- as.factor(substr(replicates, 1,6))

### Q10 corrections
df16 <- df
df16[1:5,2] <- df[1:5,2]*(3.1^((16-15)/10))
df16[6:10,2] <- df[6:10,2]*(2.66^((16-20)/10))
df16[11:15,2] <- df[11:15,2]*(2.66^((16-16)/10))
df16[16:17,2] <- df[16:17,2]*(2.66^((16-14)/10))
print(df16)

### Plot
pch_sp <- c(1,17,3,0)
plot(log(Slope)~log(WW), bty="l",data=df16, cex=1.6, pch=pch_sp[df16$Species], lwd=2, col="black", ylab="Log ammonium release rate (nM "~gWW^{-1}~ h^{-1}~")", cex.lab=1.4, xlab="Log wet-weight (g)")
abline(lm(log(Slope)~log(WW),data=df16), lwd=2)
legend(x=5.1,y=4.7, bty="n", cex=1.4, pt.cex=1.6, text.font=3, pt.lwd=2, legend=c("A. aurita","C. hysoscella","C. pacifica","C. fulgida"), pch=c(1,3,0,17), col="black")
summary(lm(log(Slope)~log(WW), data=df16))

### Phosphate

# make matrix to save results from linear regression
mat <- matrix(dat=NA, nrow=length(replicates), ncol=6)

# counter
counter <- 1

# loop for linear regression of each individual control
for(i in replicates){
  
  lm_loop <- lm(Phosphate_release_WW ~ time, release[release$ID==i & release$time<6.2,])
  ww_extract <- release[release$ID==i,]
  mat[counter,1] <- coef(lm_loop)[1]
  mat[counter,2] <- coef(lm_loop)[2]
  mat[counter,3] <- summary(lm_loop)$adj
  mat[counter,4] <- round(summary(lm_loop)$coef[2,4]*1000)/1000
  mat[counter,5] <- unique(ww_extract$WW)
  #mat[counter,6] <- unique(ww_extract$Species)
  counter <- counter+1
}

# save output as dataframe
df <- as.data.frame(mat)
colnames(df) <- c("Intercept","Slope","R2","p","WW","Species")
rownames(df) <- replicates
df$Species <- as.factor(substr(replicates, 1,6))
write.csv(df,"lm_ind_Phosphate.csv")
lm_ind_phosphate <- df
print(df)

### Nitrate

# names of replicates
replicates <- unique(release$ID)

# make matrix to save results from linear regression
mat <- matrix(dat=NA, nrow=length(replicates), ncol=6)

# counter
counter <- 1

# loop for linear regression of each individual control
for(i in replicates){
  
  lm_loop <- lm(Nitrate_release_WW ~ time, release[release$ID==i & release$time<6.2,])
  ww_extract <- release[release$ID==i,]
  mat[counter,1] <- coef(lm_loop)[1]
  mat[counter,2] <- coef(lm_loop)[2]
  mat[counter,3] <- summary(lm_loop)$adj
  mat[counter,4] <- round(summary(lm_loop)$coef[2,4]*1000)/1000
  mat[counter,5] <- unique(ww_extract$WW)
  #mat[counter,6] <- unique(ww_extract$Species)
  counter <- counter+1
}

# save output as dataframe
df <- as.data.frame(mat)
colnames(df) <- c("Intercept","Slope","R2","p","WW","Species")
rownames(df) <- replicates
df$Species <- as.factor(substr(replicates, 1,6))

# Q10 correction on rates

df$Slope[df$Species=="aurita"] <- df$Slope[df$Species=="aurita"]*(2.2^((16-15)/10))
df$Slope[df$Species=="hysosc"] <- df$Slope[df$Species=="hysosc"]*(2.2^((16-20)/10))
df$Slope[df$Species=="pacifi"] <- df$Slope[df$Species=="pacifi"]*(2.2^((16-16)/10))
df$Slope[df$Species=="fulgid"] <- df$Slope[df$Species=="fulgid"]*(2.2^((16-14)/10))

df_d <- df

# Allometric plots

plot(log(Slope)~log(WW), data=df_d[df_d$Species!="fulgid",],cex=1.6, lwd=2, pch=pch_sp[df16$Species], col="black", ylab="Log nitrate production rate (?M gWW-1 d-1)", cex.lab=1.4, xlab="Log wet-weight (g)")
abline(lm(log(Slope)~log(WW), data=df_d[df_d$Species!="fulgid",]))
legend("topright", bty="n", cex=1.4, pt.cex=1.6, text.font=3, legend=c("A. aurita","C. hysosccella","C. pacifica"), pch=c(1,3,0), pt.lwd=2, col="black")
summary(lm(log(Slope)~log(WW), data=df_d[df_d$Species!="fulgid",]))

pch_sp <- c(1,17,3,0)
plot(log(Slope)~log(WW), bty="l",data=df_d[df_d$Species!="fulgid",], cex=1.6, pch=pch_sp[df16$Species], lwd=2, col="black", ylab="Log nitrate release rate (nM "~gWW^{-1}~ h^{-1}~")", cex.lab=1.4, xlab="Log wet-weight (g)")
abline(lm(log(Slope)~log(WW),data=df_d[df_d$Species!="fulgid",]), lwd=2)
legend(x=5.1,y=4, bty="n", cex=1.4, pt.cex=1.6, text.font=3, pt.lwd=2, legend=c("A. aurita","C. hysoscella","C. pacifica"), pch=c(1,3,0,17), col="black")
summary(lm(log(Slope)~log(WW), data=df_d[df_d$Species!="fulgid",]))

plot(log(Slope)~log(WW), bty="l",data=df_d[df_d$Species!="fulgid",], cex=1.6, pch=1, lwd=2, col=df_d$Species[df_d$Species!="fulgid"], ylab="Log nitrate release rate (nM "~gWW^{-1}~ h^{-1}~")", cex.lab=1.4, xlab="Log wet-weight (g)")
abline(lm(log(Slope)~log(WW),data=df_d[df_d$Species!="fulgid",]), lwd=2)
legend(x=5.1,y=4, bty="n", cex=1.4, pt.cex=1.6, text.font=3, pt.lwd=2, legend=c("A. aurita","C. hysoscella","C. pacifica"), pch=1 ,col=df_d$Species[df_d$Species!="fulgid"])


