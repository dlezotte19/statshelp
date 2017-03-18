#This function will perform statistical tests with given data.
#Unlike current R functions, a step by step explanation on how to solve the problem will be given.
#Unlike other online statistics help resources, this package shows you how to solve your specific problem using your unique data.
#This package is geared towards highschool and college students learning or reviewing statistics.

#a built in z table for reference.
ztable <- data.frame(".00" = c(.5000, .5398, .5793, .6179, .6554, .6915,
                               .7257, .7580, .7881, .8159, .8413, .8643,
                               .8849, .9032, .9192, .9332, .9452, .9554,
                               .9641, .9713, .9772, .9821, .9861, .9893,
                               .9918, .9938, .9953, .9965, .9974, .9981,
                               .9987, .9990, .9993, .9995, .9997 ),
                     ".01" = c(.5040, .5438, .5832, .6217, .6591, .6950,
                               .7291, .7611, .7910, .8186, .8438, .8665,
                               .8869, .9049, .9207, .9345, .9463, .9564,
                               .9649, .9719, .9778, .9826, .9864, .9896,
                               .9920, .9940, .9955, .9966, .9975, .9982,
                               .9987, .9991, .9993, .9995, .9997),
                     ".02" = c(.5080, .5478, .5871, .6255, .6628, .6985,
                               .7324, .7642, .7939, .8212, .8461, .8686,
                               .8888, .9066, .9222, .9357, .9474, .9573,
                               .9656, .9726, .9783, .9830, .9868, .9898,
                               .9922, .9941, .9956, .9967, .9976, .9982,
                               .9987, .9991, .9994, .9995, .9997),
                     ".03" = c(.5120, .5517, .5910, .6293, .6664, .7019,
                               .7357, .7673, .7967, .8238, .8485, .8708,
                               .8907, .9082, .9236, .9370, .9484, .9582,
                               .9664, .9732, .9788, .9834, .9871, .9901,
                               .9925, .9943, .9957, .9968, .9977, .9983,
                               .9988, .9991, .9994, .9996, .9997),
                     ".04" = c(.5160, .5557, .5948, .6331, .6700, .7054,
                               .7389, .7704, .7995, .8264, .8508, .8729,
                               .8925, .9099, .9251, .9382, .9495, .9591,
                               .9671, .9738, .9793, .9838, .9875, .9904,
                               .9927, .9945, .9959, .9969, .9977, .9984,
                               .9988, .9992, .9994, .9996, .9997),
                     ".05" = c(.5199, .5596, .5987, .6368, .6736, .7088,
                               .7422, .7734, .8023, .8289, .8531, .8749,
                               .8944, .9115, .9265, .9394, .9505, .9599,
                               .9678, .9744, .9798, .9842, .9878, .9906,
                               .9929, .9946, .9960, .9970, .9978, .9984,
                               .9989, .9992, .9994, .9996, .9997),
                     ".06" = c(.5239, .5636, .6026, .6406, .6772, .7123,
                               .7454, .7764, .8051, .8315, .8554, .8770,
                               .8962, .9131, .9279, .9406, .9515, .9608,
                               .9686, .9750, .9803, .9846, .9881, .9909,
                               .9931, .9948, .9961, .9971, .9979, .9985,
                               .9989, .9992, .9994, .9996, .9997),
                     ".07" = c(.5279, .5675, .6064, .6443, .6808, .7157,
                               .7486, .7794, .8078, .8340, .8577, .8790,
                               .8980, .9147, .9292, .9418, .9525, .9616,
                               .9693, .9756, .9808, .9850, .9884, .9911,
                               .9932, .9949, .9962, .9972, .9979, .9985,
                               .9989, .9992, .9995, .9996, .9997),
                     ".08" = c(.5319, .5714, .6103, .6480, .6844, .7190,
                               .7517, .7823, .8106, .8365, .8599, .8810,
                               .8997, .9162, .9306, .9429, .9535, .9625,
                               .9699, .9761, .9812, .9854, .9887, .9913,
                               .9934, .9951, .9963, .9973, .9980, .9986,
                               .9990, .9993, .9995, .9996, .9997),
                     ".09" = c(.5359, .5753, .6141, .6517, .6879, .7224,
                               .7549, .7852, .8133, .8389, .8621, .8830,
                               .9015, .9177, .9319, .9441, .9545, .9633,
                               .9706, .9767, .9817, .9857, .9890, .9916,
                               .9936, .9952, .9964, .9974, .9981, .9986,
                               .9990, .9993, .9995, .9997, .9998)
                     , row.names = seq(0,3.4,by=.1))

  #Standard Error of a dataset
stdEr <- function(s,n) {
  formula <- "The formula for standard error is: SE = s/sqrt(n)"
  Instance <- paste("In this case", s,"/","sqrt(",n,")","=",s/sqrt(n))
  Final <- c(formula, Instance)
  Final
}

#Standard Error example
stdEr(15,200)

#Confidence Interval function
confidInv <- function(xbar,s,n,level=.95) {
  formula <- "The formula for a confidence interval is: xbar +/- Z_alpha/2*(s/sqrt(n))"
  Instance <- paste("In this case", xbar, "+/-", "Z_(",(level+(1-level)/2),")","* (",s,"/sqrt(",n,"))")
  s1 <- c(formula,Instance)
  zcrit <- "To find the z critical value find the closest value to yours inside the table and see what row and column it corresponds with"
  sc2 <- paste("Here the z critical value is",round(qnorm(level+(1-level)/2),3))
  s2 <- c(zcrit,sc2)
  s3 <- paste("So now we have: ", xbar, "+/-", round(qnorm(level+(1-level)/2),3),"*",s/sqrt(n))
  s4 <- paste("To find the upper end of the interval we do:",xbar,"+",round(qnorm(level+(1-level)/2),3),"*",s/sqrt(n),"=",xbar+round(qnorm(level+(1-level)/2),3)*(s/sqrt(n)))
  s5 <- paste("To find the lower end of the interval we do:",xbar,"-",round(qnorm(level+(1-level)/2),3),"*",s/sqrt(n),"=",xbar-round(qnorm(level+(1-level)/2),3)*(s/sqrt(n)))
  Con <- paste("We are",(level*100),"% certain that the true mean is within (",xbar-round(qnorm(level+(1-level)/2),3)*(s/sqrt(n)),",",xbar+round(qnorm(level+(1-level)/2),3)*(s/sqrt(n)),")")
  final <- list(step1 = s1,step2 = s2,step3 = s3,step4 = s4,step5 = s5,Conclusion = Con)
  final
}

#Confidence Interval Example
confidInv(2,3,100,level=.90)

#Mean of a data set function
meanHelp <- function(x) {
  s1 <- paste('To find the mean, divide the sum of the data set by the size of the data set: sum(x)/n ')
  s2 <- paste(sum(x),'/',length(x),'=',sum(x)/length(x))
  final <- c(s1,s2)
  final
}

#Example of Mean Function
meanHelp(c(1:17,1:5,rep(6,6)))

#Median of a data set function
medianHelp <- function(x) {
  s1 <- paste('the median is the number in the middle of the data set when the numbers are arranged in increasing order.')
  s2 <- 'So first order the data from smallest value to largest value:'
  s3 <- sort(x)
  sx <- sort(x)
  s4 <- paste('now use the formula to find the middle number: n+1/2, where n is the size of the sample.')
  s5 <- paste('in this case:', length(x),'+ 1 /',2,'=',(length(x)+1)/2)
  if(length(x)%%2 == 1) {s6 <- paste('The median is the',(length(x)+1)/2,'th value in the data set, which is:',median(sx))}
  else {s6 <- paste('Because n+1/2 is not a whole number then we take the average of the two numbers it is between:'
                    ,sx[floor((length(sx)+1)/2)],'+',sx[ceiling((length(sx)+1)/2)],'/',2,'=',median(sx))}
  final <- list('step1'=s1,'step2'=s2,'sorted data'=s3,'step3'=s4,'step4'=s5,'step5'=s6)
  final
}

#Median example
medianHelp(c(1:15,1:5,rep(1:4,4)))

#Mode of a data set
modeHelp <- function(x) {
  s1 <- 'The mode is the number in the dataset that occurs the most. It helps to sort the data first:'
  s2 <- sort(x)
  sx <- table(x)
  ux <- unique(x)
  if(length(sx[sx == max(sx)]) > 1) {
    s3 <- paste('The highest count happens in multiple places, therefore there is more than one mode:')
  } else {
    s3 <- paste('The number that occurs the most is:',ux[which.max(tabulate(match(x,ux)))])
  }
  s4 <- names(sx[sx == max(sx)])
  final <- list('Step1' = s1,'sorted Data' = s2,'frequency table' = sx,'Step2' = s3,'mode(s):'=s4)
  final
}

#example of Mode
modeHelp(c(1:15,3:5,rep(1:4,4)))

#ztest for means
ztest <- function(n,s,xbar,mu,SE=NULL,a=.05){
  Describe <- "A Z test is used when you know the standard deviation of the population, if you only know the standard deviation of the sample then you use a T test."
  
  if(is.null(SE)) {
    Step1 <- c("First find the Standard Error: SE = s/sqrt(n)",
               paste("In this case:",s,"/sqrt(",n,") =", s/sqrt(n)))
    SE <- s/sqrt(n)} else{
      Step1 <- "Becuase the standard error is given we don't have to compute it"
    }
  
  Step2 <- c("Now compute the the test statistic: Z=(xbar-mu)/SE",
             paste("Which would be Z=(",xbar,"-",mu,") /",SE,"=",round(((xbar-mu)/SE),2)))
  z = round(((xbar-mu)/SE),2)
  
  Step3 <- c("Find your Z statistic on the table, using the first 2 digits for the row name and the last digit  for the column name.", 
             paste("Row:", floor(z*10)/10, "Column:", round((z-(floor(z*10))/10),3)), paste("P(Z <",z,")","=",round(pnorm(z),5)), "***For any Z score above 3.5 use .9999***",
             "the table shows the probability of getting a z score LESS THAN", "use 1-p(z<|Z|) to find a negative z score")
  
  notequal <- c(">>Testing sample mean is NOT EQUAL to population mean<<",
                "Null hypothesis(H0): xbar = mu & Alternative hypothesis(HA or H1): xbar != mu",
                "P-value = 2*(P(z < -|Z|))",
                paste("P-Value=2*(",round(pnorm(-abs(z)),4),") = ",2*(round(pnorm(-abs(z)),5))))
  
  greater <- c(">>Testing sample mean is GREATER THAN the population mean<<",
               "Null hypothesis(H0): xbar = mu & Alternative hypothesis(HA or H1): xbar > mu",
               "P-value = (1-P(z < Z))",
               paste("P-Value=1-(",round(pnorm(z),4),") = ",1-(round(pnorm(z),5))))
  
  less <- c(">>Testing sample mean is LESS THAN the population mean<<",
            "Null hypothesis(H0): xbar = mu & Alternative hypothesis(HA or H1): xbar < mu",
            "P-value = P(z < Z)",
            paste("P-Value = ",(round(pnorm(z),5))))
  
  pvalue <- "If the P-value is < the alpha value(default value = .05), then reject the Null Hypothesis"
  
  Decision <- list('notequal' = notequal,'greater than' = greater,'less than' = less, 'pvalue' = pvalue)
  Final <- list(Describe,"Step 1: Standard Error"=Step1,"Step 2: Z score"=Step2,"Step3: finding z on table"=Step3, "Step4:Decision" = Decision)
  Final
}

#ztest example
ztest(n = 46,s = 1800,xbar = 13200,mu = 12800)
ztest(SE = 265.4,xbar = 12800,mu = 13200)

#ttest for means(function is not finished yet, does not run correctly.)
ttest <- function(n,s,xbar,mu,SE = NULL,a=.05){
  tbetween <- function(t,n,i=1) {
    ttable <<- c(1,.4,.25,.1,.05,.025,.01,.005,.0025,.001,.0005,0)
    while(ttable[i] > 1.00001-round((pt(t,(n-1))),4)) {
      i <- i+1
    } 
    if(ttable[i] < 1-pt(t, (n-1))){
      print(paste("The p-value is between ", ttable[i], "and ",ttable[i-1]))
    }
  }
  Describe <- "A T test is used when you dont know the standard deviation of the population, if you do know the poplation standard deviation then use a z test instead."
  
  if(is.null(SE)) {Step1 <- c("First find the Standard Error: SE = s/sqrt(n)",
                              paste("In this case:",s,"/sqrt(",n,") =", s/sqrt(n)))
  SE <- s/sqrt(n)} else {
    Step1 <- "The standard error is already given so we don't need to compute it."
  }
  Step2 <- c("Now compute the the test statistic: T=(xbar-mu)/SE",
             paste("Which would be T=(",xbar,"-",mu,") /",SE,"=",round(((xbar-mu)/SE),2)))
  t = round(((xbar-mu)/SE),2)
  
  Step3 <- c("Compute the degrees of freedom: n-1, this is the row in the table you will use.",
             "Then find your T value in this row. It will almost always be between two columns.", 
             paste("Row:", n-1, "Columns between value:", t, paste("P(T <",t,")","=",round(pnorm(t),5)), "***Always round up degrees of freedom if exact value is not on table.***",
                   "the table shows the probability of getting a z score LESS THAN", "use 1-p(t<|T|) to find a negative z score.",tbetween(t,n,i=1)))
  
  notequal <- c(">>Testing sample mean is NOT EQUAL to population mean<<",
                "Null hypothesis(H0): xbar = mu & Alternative hypothesis(HA or H1): xbar != mu",
                "P-value = 2*(P(t < -|T|))",
                paste("P-Value=2*(",round(pnorm(-abs(t)),4),") = ",2*(round(pnorm(-abs(t)),5))))
  
  greater <- c(">>Testing sample mean is GREATER THAN the population mean<<",
               "Null hypothesis(H0): xbar = mu & Alternative hypothesis(HA or H1): xbar > mu",
               "P-value = (1-P(t < T))",
               paste("P-Value=1-(",round(pnorm(t),4),") = ",1-(round(pnorm(t),5))))
  
  less <- c(">>Testing sample mean is LESS THAN the population mean<<",
            "Null hypothesis(H0): xbar = mu & Alternative hypothesis(HA or H1): xbar < mu",
            "P-value = P(t < T)",
            paste("P-Value = ",(round(pnorm(t),5))))
  
  pvalue <- "If the P-value is < the alpha value(default value = .05), then reject the Null Hypothesis"
  
  Decision <- list('notequal' = notequal,'greater than' = greater,'less than' = less, 'pvalue' = pvalue)
  Final <- list(Describe,"Step 1: Standard Error"=Step1,"Step 2: Z score"=Step2,"Step3: finding z on table"=Step3, "Step4:Decision" = Decision)
  Final
}

#ztest example
ztest(n=46,s=1800,xbar=13200,mu=12800)
