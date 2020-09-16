###### Function to make transparant colour on the plots #######
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

# Somewhat transparant:
plot(rnorm(100),rnorm(100),col=addTrans(cols,200),pch=16,cex=4)

# Very transparant:
plot(rnorm(100),rnorm(100),col=addTrans(cols,100),pch=16,cex=4)
##############################################################################

########################################## plotting begins...................

####### for 2 objectives #################
##########################################
##### mopsopsa first to see the axis limits
pf2 <- dtlz2_2front(1000)

s1.2 <- mopsocds(dtlz2_2, varcnt=11, fncnt=2, lowerbound=rep(0, 11), upperbound=rep(1,11), opt=0)
plot(pf2, type = "l", lty = 1, col = addTrans(c("red"), 255), lwd = 10, xlab = "f1", ylab = "f2",cex.lab=1.5,cex.axis=1.25,
     xlim = c(0,1.5), ylim = c(0,1.5))
par(new=TRUE)
plot(s1.2$objfnvalues, type = "p", pch = 16, col = addTrans(c("springgreen2"), 255), cex = 0.75,xlim = c(0,1.5), ylim = c(0,1.5),
     xlab = "", ylab = "", xaxt ="n", yaxt = "n")

s2.2 <- mopsopsa_v1(dtlz2_2, varcnt=11, fncnt=2, lowerbound=rep(0, 11), upperbound=rep(1,11), opt=0)
plot(pf2, type = "l", lty = 1, col = addTrans(c("red"), 255), lwd = 10, xlab = "f1", ylab = "f2",cex.lab=1.5,cex.axis=1.25,
     xlim = c(0,1.5), ylim = c(0,1.5))
par(new=TRUE)
plot(s2.2$objfnvalues, type = "p", pch = 16, col = addTrans(c("springgreen2"), 255), cex = 0.75,xlim = c(0,1.5), ylim = c(0,1.5),
     xlab = "", ylab = "", xaxt ="n", yaxt = "n")

legend("topright", c("True Pareto front", "Non-dominated solutions"), pch = c(20,20), col = c("red", "springgreen2"))

############# for stored populations
plot(s1.2$storedFit, type = "p", pch = 20, col = addTrans(c("grey70"), 50), xlab = "f1", ylab = "f2", cex.lab = 1.5, cex.axis = 1.25,
     xlim = c(0, 3.5), ylim = c(0, 3.5))
par(new=TRUE)
plot(s1.2$objfnvalues, type = "p", pch = 20, col = addTrans(c("springgreen2"), 255), cex = 1.25, xlab = "", ylab = "", xlim = c(0, 3.5),
     ylim = c(0, 3.5), xaxt ="n", yaxt = "n")

s2.2 <- mopsopsa_v1(dtlz2_2, varcnt=11, fncnt=2, lowerbound=rep(0, 11), upperbound=rep(1,11), opt=0)
plot(s2.2$storedFit, type = "p", pch = 20, col = addTrans(c("grey70"), 50), xlab = "f1", ylab = "f2", cex.lab = 1.5, cex.axis = 1.25,
     xlim = c(0, 3.5), ylim = c(0, 3.5))
par(new=TRUE)
plot(s2.2$objfnvalues, type = "p", pch = 20, col = addTrans(c("springgreen2"), 255), cex = 1.25, xlab = "", ylab = "", xlim = c(0, 3.5),
     ylim = c(0, 3.5), xaxt ="n", yaxt = "n")

legend("topright", c("All solutions", "Non-dominated solutions"), pch = c(20,20), col = c("grey80", "springgreen2"))
legend("topright", c("All solutions", "Non-dominated solutions", expression("n"[PSA]==2)), pch = c(20,20,NA), col = c("grey80", "springgreen2"))
#############################

plot(s1$storedFit, type = "p", pch = 20, col = addTrans(c("grey70"), 50), xlab = "f1", ylab = "f2", cex.lab = 1.5, cex.axis = 1.25,
     main = "All and non-dominated solutions \nwith True Pareto front", xlim = c(0, 3.5), ylim = c(0, 3.5))
par(new=TRUE)

################################

######## for 3 objectives ######
################################

#### True Pareto front #########
pf3 <- dtlz2_3front(20000)
x3 <- pf3[,1]
y3 <- pf3[,2]
z3 <- pf3[,3]

####### MOPSOCD ####################
s1 <- mopsocds(dtlz2_3, varcnt=13, fncnt=3, lowerbound=rep(0, 13), upperbound=rep(1,13), opt=0)
s1.xpf <- s1$objfnvalues[,1]
s1.ypf <- s1$objfnvalues[,2]
s1.zpf <- s1$objfnvalues[,3]
par(new=TRUE)

scatter3D(x3, y3, z3, xlab='f1', ylab='f2', zlab='f3', col = addTrans(c("red"), 255), axis.scales=TRUE, theta=30, phi = 20, pch = 20, 
          bty = "b2", ticktype = "detailed", axis.scales = TRUE, xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))
par(new=TRUE)
scatter3D(s1.xpf, s1.ypf, s1.zpf, xlab='', ylab='', zlab='', col = addTrans(c("springgreen2"), 255), theta=30, phi = 20, pch = 20, 
          bty = "n", xaxt = "n", yaxt = "n", zaxt ="n", xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))

scatter3D(x3, y3, z3, xlab='f1', ylab='f2', zlab='f3', col = addTrans(c("red"), 255), axis.scales=TRUE, theta=120, phi = 30, pch = 20, 
          bty = "b2", ticktype = "detailed", axis.scales = TRUE, xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))
par(new=TRUE)
scatter3D(s1.xpf, s1.ypf, s1.zpf, xlab='', ylab='', zlab='', col = addTrans(c("springgreen2"), 255), theta=120, phi = 30, pch = 20, 
          bty = "n", xaxt = "n", yaxt = "n", zaxt ="n", xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))

######## MOPSOPSA V1 ###############
s2.v1 <- mopsopsa_v1(dtlz2_3, varcnt=13, fncnt=3, lowerbound=rep(0, 13), upperbound=rep(1,13), opt=0)
s2.v1.xpf <- s2.v1$objfnvalues[,1]
s2.v1.ypf <- s2.v1$objfnvalues[,2]
s2.v1.zpf <- s2.v1$objfnvalues[,3]

scatter3D(x3, y3, z3, xlab='f1', ylab='f2', zlab='f3', col = addTrans(c("red"), 255), axis.scales=TRUE, theta=30, phi = 20, pch = 20, 
          bty = "b2", ticktype = "detailed", axis.scales = TRUE, xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))
par(new=TRUE)
scatter3D(s2.v1.xpf, s2.v1.ypf, s2.v1.zpf, xlab='', ylab='', zlab='', col = addTrans(c("springgreen2"), 255), theta=30, phi = 20, pch = 20, 
          bty = "n", xaxt = "n", yaxt = "n", zaxt ="n", xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))

scatter3D(x3, y3, z3, xlab='f1', ylab='f2', zlab='f3', col = addTrans(c("red"), 255), axis.scales=TRUE, theta=120, phi = 30, pch = 20, 
          bty = "b2", ticktype = "detailed", axis.scales = TRUE, xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))
par(new=TRUE)
scatter3D(s2.v1.xpf, s2.v1.ypf, s2.v1.zpf, xlab='', ylab='', zlab='', col = addTrans(c("springgreen2"), 255), theta=120, phi = 30, pch = 20, 
          bty = "n", xaxt = "n", yaxt = "n", zaxt ="n", xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))


legend("topright", c("True Pareto front", "Non-dominated solutions"), pch = c(20,20), col = c("red", "springgreen2"))
################################

########## MOPSOPSA V2 #############
s2.v2 <- mopsopsa_v2(dtlz2_3, varcnt=13, fncnt=3, lowerbound=rep(0, 13), upperbound=rep(1,13), opt=0)
s2.v2.xpf <- s2.v2$objfnvalues[,1]
s2.v2.ypf <- s2.v2$objfnvalues[,2]
s2.v2.zpf <- s2.v2$objfnvalues[,3]
par(new=TRUE)
scatter3D(s2.v2.xpf, s2.v2.ypf, s2.v2.zpf, xlab='', ylab='', zlab='', col = addTrans(c("springgreen2"), 255), theta=30, phi = 20, pch = 20, 
          bty = "n", xaxt = "n", yaxt = "n", zaxt ="n", xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))
scatter3D(s2.v2.xpf, s2.v2.ypf, s2.v2.zpf, xlab='', ylab='', zlab='', col = addTrans(c("springgreen2"), 255), theta=120, phi = 30, pch = 20, 
          bty = "n", xaxt = "n", yaxt = "n", zaxt ="n", xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))


######### Plotting stored populations, probably not so visible in the 3D #################
s1.x <- s1$storedFit[,1]
s1.y <- s1$storedFit[,2]
s1.z <- s1$storedFit[,3]
scatter3D(s1.x, s1.y, s1.z, xlab='f1', ylab='f2', zlab='f3', col = addTrans(c("grey70"), 50), axis.scales=TRUE, theta=30, phi = 25, pch = 20, 
          bty = "b2", ticktype = "detailed", axis.scales = TRUE, xlim = c(0,1.5), ylim = c(0,1.5), zlim = c(0,1.5))
