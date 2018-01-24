#' Confidence Interval of a dataset
#'
#' This function shows you how to compute a confidence interval.
#' @param xbar - average of the data, s - standard deviation, n - sample size, level = level(default=.95)
#' @keywords Confidence Interval
#' @export
#' @examples confidInv(2,3,100,level=.90)
#' confidInv()

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

#' Mean of a dataset
#'
#' This function shows you how to the mean of a data set.
#' @param x - a data set, as a vector
#' @keywords mean
#' @export0
#' @examples mean(c(1:188,1:15,rep(6,4)))
#' mean()

meanHelp <- function(x) {
  s1 <- paste('To find the mean, divide the sum of the data set by the size of the data set: sum(x)/n ')
  s2 <- paste(sum(x),'/',length(x),'=',sum(x)/length(x))
  final <- c(s1,s2)
  final
}

#' Median of a dataset
#'
#' This function shows you how to compute the median of a dataset.
#' @param x - data set, as a vector
#' @keywords Median
#' @export
#' @examples medianHelp(c(1:12,4:19))
#' medianHelp()

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

#' mode of a dataset
#'
#' This function shows you how to compute the mode of a dataset.
#' @param x - dataset, as a vector
#' @keywords mode
#' @export
#' @examples modeHelp(c(1:15,3:5,rep(1:4,4)))
#' modeHelp()
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

#' Z test for means
#'
#' This function shows you how to perform a one sample ztest for means.
#' @param n - sample size, s - standard deviation, xbar - sample average, mu - population average, SE - Standard error(NULL), a - alpha(default = .05)
#' @keywords mean, one sample, ztest
#' @export
#' @examples ztest(n = 46,s = 1800,xbar = 13200,mu = 12800), ztest(SE = 265.4,xbar = 12800,mu = 13200)
#' ztest()

ztest <- function(n,s,xbar,mu,SE=NULL,a=.05){
  Describe <- "A Z test is used when you know the standard deviation of the population, if you only know the standard deviation of the sample then you use a T test."

  if(is.null(SE)) {
    Step1 <- c("First find the Standard Error: SE = s/sqrt(n)",
               paste("In this case:",s,"/sqrt(",n,") =", s/sqrt(n)))
    SE <- s/sqrt(n)}
    else{
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

#' t test for means
#'
#' This function shows you how to perform a one sample ttest for means.
#' @param n - sample size, s - standard deviation, xbar - sample average, mu - population average, SE - Standard error(NULL), a - alpha(default = .05)
#' @keywords mean, one sample, ttest
#' @export
#' @examples ttest(n = 46,s = 1800,xbar = 13200,mu = 12800), ttest(SE = 265.4,xbar = 12800,mu = 13200)
#' ttest()

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
             paste("Row:", n-1, "Columns between value:", t, paste("1-P(T <",t,")","=",1-round(pnorm(t),5)),"The exact value shown can only be computed through software", "***Always round up degrees of freedom if exact value is not on table.***",
                   "use 1-p(t<|T|) to find a negative t  score.",tbetween(t,n,i=1)))

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
  Final <- list(Describe,"Step 1: Standard Error"=Step1,"Step 2: t score"=Step2,"Step3: finding t on table"=Step3, "Step4:Decision" = Decision)
  Final
}

#' Z test for proportions
#'
#' This function shows you how to perform a one sample ztest for proportions.
#' @param n - sample size, phat - sample proportion, p - population proportion, SE - Standard error(NULL), a - alpha(default = .05)
#' @keywords proportion, one sample, ztest
#' @export
#' @examples ztestp(n = 46,phat = .18,p = .165), ztestp(SE = .07,xbar = .18,mu = .14)
#' ztestp()
#ztest for proportions
ztestp <- function(n,phat,p,SE=NULL,a=.05){
  Describe <- "Because testing for proportions does not require a standard deviation, there is not t test for porportions, only a z test."

  if(is.null(SE)) {
    Step1 <- c("First find the Standard Error: SE = s/sqrt(n)",
               paste("In this case: sqrt(",p,"(",1-p,")/",n, ") =", sqrt(p*(1-p)/n)))
    SE <- sqrt(p*(1-p)/n)}
  else{
    Step1 <- "Becuase the standard error is given we don't have to compute it"
  }

  Step2 <- c("Now compute the the test statistic: Z=(phat-p)/SE",
             paste("Which would be Z=(",phat,"-",p,") /",SE,"=",round(((phat-p)/SE),2)))
  z = round(((phat-p)/SE),2)

  Step3 <- c("Find your Z statistic on the table, using the first 2 digits for the row name and the last digit  for the column name.",
             paste("Row:", floor(z*10)/10, "Column:", round((z-(floor(z*10))/10),3)), paste("P(Z <",z,")","=",round(pnorm(z),5)), "***For any Z score above 3.5 use .9999***",
             "the table shows the probability of getting a z score LESS THAN", "use 1-p(z<|Z|) to find a negative z score")

  notequal <- c(">>Testing sample mean is NOT EQUAL to population mean<<",
                "Null hypothesis(H0): phat = p & Alternative hypothesis(HA or H1): phat != p",
                "P-value = 2*(P(z < -|Z|))",
                paste("P-Value=2*(",round(pnorm(-abs(z)),4),") = ",2*(round(pnorm(-abs(z)),5))))

  greater <- c(">>Testing sample mean is GREATER THAN the population mean<<",
               "Null hypothesis(H0): phat = p & Alternative hypothesis(HA or H1): phat > p",
               "P-value = (1-P(z < Z))",
               paste("P-Value=1-(",round(pnorm(z),4),") = ",1-(round(pnorm(z),5))))

  less <- c(">>Testing sample mean is LESS THAN the population mean<<",
            "Null hypothesis(H0): phat = p & Alternative hypothesis(HA or H1): phat < p",
            "P-value = P(z < Z)",
            paste("P-Value = ",(round(pnorm(z),5))))

  pvalue <- "If the P-value is < the alpha value(default value = .05), then reject the Null Hypothesis"

  Decision <- list('notequal' = notequal,'greater than' = greater,'less than' = less, 'pvalue' = pvalue)
  Final <- list(Describe,"Step 1: Standard Error"=Step1,"Step 2: Z score"=Step2,"Step3: finding z on table"=Step3, "Step4:Decision" = Decision)
  Final
}

#' two sample Z test for means
#'
#' This function shows you how to perform a two sample ztest for means.
#' @param n1 - sample 1 size, n2 - sample 2 size,s1 - standard deviation of sample 1,s2 - standard deviation of sample 2, xbar1 - average of sample 1,xbar2 - average of sample 2, a - alpha(default = .05)
#' @keywords mean, one sample, ztest
#' @export
#' @examples ztest2s(n1=45,n2=52,s1=1.4,s2=1.6,xbar1=42.1,xbar2=42.7)
#' ztest2s()

ztest2s <- function(n1,n2,s1,s2,xbar1,xbar2,SE=NULL,a=.05){
  Describe <- "A Z test is used when you know the standard deviation of the population, if you only know the standard deviation of the sample then you use a T test."

  if(is.null(SE)) {
    Step1 <- c("First find the Standard Error: SE = sqrt((s1/n1)+(s2/n2))",
               paste("In this case: sqrt((",s1,"/",n1,")+(",s2,"/",n2,")) =", sqrt((s1/n1)+(s2/n2))))
    SE <- sqrt((s1/n1)+(s2/n2))}
  else{
    Step1 <- "Becuase the standard error is given we don't have to compute it"
  }

  Step2 <- c("Now compute the the test statistic: Z=(xbar1-xbar2)/SE",
             paste("Which would be Z=(",xbar1,"-",xbar2,") /",SE,"=",round(((xbar1-xbar2)/SE),2)))
  z = round(((xbar1-xbar2)/SE),2)

  Step3 <- c("Find your Z statistic on the table, using the first 2 digits for the row name and the last digit  for the column name.",
             paste("Row:", floor(z*10)/10, "Column:", round((z-(floor(z*10))/10),3)), paste("P(Z <",z,")","=",round(pnorm(z),5)), "***For any Z score above 3.5 use .9999***",
             "the table shows the probability of getting a z score LESS THAN", "use 1-p(z<|Z|) to find a negative z score")

  notequal <- c(">>Testing sample mean is NOT EQUAL to population mean<<",
                "Null hypothesis(H0): xbar1 = xbar2 & Alternative hypothesis(HA or H1): xbar1 != xbar2",
                "P-value = 2*(P(z < -|Z|))",
                paste("P-Value=2*(",round(pnorm(-abs(z)),4),") = ",2*(round(pnorm(-abs(z)),5))))

  greater <- c(">>Testing sample mean is GREATER THAN the population mean<<",
               "Null hypothesis(H0): xbar1 = xbar2 & Alternative hypothesis(HA or H1): xbar1 > xbar2",
               "P-value = (1-P(z < Z))",
               paste("P-Value=1-(",round(pnorm(z),4),") = ",1-(round(pnorm(z),5))))

  less <- c(">>Testing sample mean is LESS THAN the population mean<<",
            "Null hypothesis(H0): xbar1 = xbar2 & Alternative hypothesis(HA or H1): xbar1 < xbar2",
            "P-value = P(z < Z)",
            paste("P-Value = ",(round(pnorm(z),5))))

  pvalue <- "If the P-value is < the alpha value(default value = .05), then reject the Null Hypothesis"

  Decision <- list('notequal' = notequal,'greater than' = greater,'less than' = less, 'pvalue' = pvalue)
  Final <- list(Describe,"Step 1: Standard Error"=Step1,"Step 2: Z score"=Step2,"Step3: finding z on table"=Step3, "Step4:Decision" = Decision)
  Final
}

#' two sample t test for means
#'
#' This function shows you how to perform a two sample ttest for means.
#' @param n1 - sample 1 size, n2 - sample 2 size,s1 - standard deviation of sample 1,s2 - standard deviation of sample 2, xbar1 - average of sample 1,xbar2 - average of sample 2, a - alpha(default = .05)
#' @keywords mean, two sample, ttest
#' @export
#' @examples ttest2s(n1=45,n2=52,s1=1.4,s2=1.6,xbar1=42.1,xbar2=42.7)
#' ttest2s()
ttest2s <- function(n1,n2,s1,s2,xbar1,xbar2,SE = NULL,a=.05){
  tbetween2 <- function(t,n1,n2,i=1) {
    ttable <<- c(1,.4,.25,.1,.05,.025,.01,.005,.0025,.001,.0005,0)
    while(ttable[i] > 1.00001-round((pt(t,(n1+n2-2))),4)) {
      i <- i+1
    }
    if(ttable[i] < 1-pt(t, (n1+n2-2))){
      print(paste("The p-value is between ", ttable[i], "and ",ttable[i-1]))
    }
  }
  Describe <- "A T test is used when you dont know the standard deviation of the population, if you do know the poplation standard deviation then use a z test instead."

  if(is.null(SE)) {Step1 <- c("First find the Standard Error: SE = sqrt((s1^2/n1)+(s2^2/n2))",
                              paste("In this case: sqrt((",s1*s1,"/",n1,")+(",s2*s2,"/",n2, "))=", sqrt((s1*s2/n1)+(s2*s2/n2))))
  SE <- sqrt((s1*s2/n1)+(s2*s2/n2))} else {
    Step1 <- "The standard error is already given so we don't need to compute it."
  }
  Step2 <- c("Now compute the the test statistic: T=(xbar1-xbar2)/SE",
             paste("Which would be T=(",xbar1,"-",xbar2,") /",SE,"=",round(((xbar1-xbar2)/SE),2)))
  t = round(((xbar1-xbar2)/SE),2)

  Step3 <- c("Compute the degrees of freedom: n1+n2-2, this is the row in the table you will use.",
             "Then find your T value in this row. It will almost always be between two columns.",
             paste("Row:", n1+n2-2, "Columns between value:", t, paste("1-P(T <",t,")","=",1-round(pnorm(t),5)),"The exact value shown can only be computed through software", "***Always round up degrees of freedom if exact value is not on table.***",
                   "use 1-p(t<|T|) to find a negative t score.",tbetween2(t,n1,n2,i=1)))

  notequal <- c(">>Testing sample mean is NOT EQUAL to population mean<<",
                "Null hypothesis(H0): xbar1 = xbar2 & Alternative hypothesis(HA or H1): xbar1 != xbar2",
                "P-value = 2*(P(t < -|T|))",
                paste("P-Value=2*(",round(pnorm(-abs(t)),4),") = ",2*(round(pnorm(-abs(t)),5))))

  greater <- c(">>Testing sample mean is GREATER THAN the population mean<<",
               "Null hypothesis(H0): xbar1 = xbar2 & Alternative hypothesis(HA or H1): xbar1 > xbar2",
               "P-value = (1-P(t < T))",
               paste("P-Value=1-(",round(pnorm(t),4),") = ",1-(round(pnorm(t),5))))

  less <- c(">>Testing sample mean is LESS THAN the population mean<<",
            "Null hypothesis(H0): xbar = mu & Alternative hypothesis(HA or H1): xbar1 < xbar2",
            "P-value = P(t < T)",
            paste("P-Value = ",(round(pnorm(t),5))))

  pvalue <- "If the P-value is < the alpha value(default value = .05), then reject the Null Hypothesis"

  Decision <- list('notequal' = notequal,'greater than' = greater,'less than' = less, 'pvalue' = pvalue)
  Final <- list(Describe,"Step 1: Standard Error"=Step1,"Step 2: t score"=Step2,"Step3: finding t on table"=Step3, "Step4:Decision" = Decision)
  Final
}

#' matched pairs t test
#'
#' This function shows you how to perform a two sample matched pairs ttest for means.
#' @param n - sample size,s - standard deviation, dbar - average difference of matched x's ans y's,SE = Standard error,a - alpha(default = .05)
#' @keywords mean, two sample, ttest, matched pairs
#' @export
#' @examples ttestmatch(15,10,6)
#' ttestmatch()
ttestmatch <- function(n,s,dbar,SE = NULL,a=.05){
  tbetween <- function(t,n,i=1) {
    ttable <<- c(1,.4,.25,.1,.05,.025,.01,.005,.0025,.001,.0005,0)
    while(ttable[i] > 1.00001-round((pt(t,(n-1))),4)) {
      i <- i+1
    }
    if(ttable[i] < 1-pt(t, (n-1))){
      print(paste("The p-value is between ", ttable[i], "and ",ttable[i-1]))
    }
  }
  Describe <- "A matched pairs t test is used when two measurements are taken from the same subject. dbar is the average of the differences between the two observations"

  if(is.null(SE)) {Step1 <- c("First find the Standard Error: SE = s/sqrt(n)",
                              paste("In this case:",s,"/sqrt(",n,") =", s/sqrt(n)))
  SE <- s/sqrt(n)} else {
    Step1 <- "The standard error is already given so we don't need to compute it."
  }
  Step2 <- c("Now compute the the test statistic: T=(dbar)/SE",
             paste("Which would be T=",dbar, "/",SE,"=",round(((dbar)/SE),2)))
  t = round(((dbar)/SE),2)

  Step3 <- c("Compute the degrees of freedom: n-1, this is the row in the table you will use.",
             "Then find your T value in this row. It will almost always be between two columns.",
             paste("Row:", n-1, "Columns between value:", t, paste("1-P(T <",t,")","=",1-round(pnorm(t),5)),"The exact value shown can only be computed through software", "***Always round up degrees of freedom if exact value is not on table.***",
                   "use 1-p(t<|T|) to find a negative z score.",tbetween(t,n,i=1)))

  notequal <- c(">>Testing sample mean is NOT EQUAL to population mean<<",
                "Null hypothesis(H0): dbar = 0 & Alternative hypothesis(HA or H1): dbar != 0",
                "P-value = 2*(P(t < -|T|))",
                paste("P-Value=2*(",round(pnorm(-abs(t)),4),") = ",2*(round(pnorm(-abs(t)),5))))

  greater <- c(">>Testing sample mean is GREATER THAN the population mean<<",
               "Null hypothesis(H0): dbar = 0 & Alternative hypothesis(HA or H1): dbar > 0",
               "P-value = (1-P(t < T))",
               paste("P-Value=1-(",round(pnorm(t),4),") = ",1-(round(pnorm(t),5))))

  less <- c(">>Testing sample mean is LESS THAN the population mean<<",
            "Null hypothesis(H0): dbar = 0 & Alternative hypothesis(HA or H1): xbar < 0",
            "P-value = P(t < T)",
            paste("P-Value = ",(round(pnorm(t),5))))

  pvalue <- "If the P-value is < the alpha value(default value = .05), then reject the Null Hypothesis"

  Decision <- list('notequal' = notequal,'greater than' = greater,'less than' = less, 'pvalue' = pvalue)
  Final <- list(Describe,"Step 1: Standard Error"=Step1,"Step 2: t score"=Step2,"Step3: finding t on table"=Step3, "Step4:Decision" = Decision)
  Final
}


