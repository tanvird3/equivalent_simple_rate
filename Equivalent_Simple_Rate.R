# for recurring simple deposit
# fv=(R+R*i*n) + (R+R*i*(n-1))+ (R+R*i*(n-2))+ .....+ (R+R*i*1)
# fv=n*R+ iR ( n+(n-1)+(n-2)+.....+1)
# fv=n*R+ iR * n(n+1)/2
# fv= R {n+i* n(n+1)/2}
# if Simple FV=Compound FV
# R {n+i* n*(n+1)/2}=compound FV
# i = (FV/R-n)/ ((n*(n+1)/2))


library(FinCal)
library(openxlsx)
library(dplyr)

simple_interest <- function(rate, term, inst, comp_freq) {
  t <- term * 12 # turn the year to month
  
  ip <-
    (1 + rate / comp_freq) ^ (comp_freq / 12) - # interest rate per period
    1
  
  comp_fv <-
    fv.annuity(ip, t,-inst, type = 1) # calculate the future value at compound rate
  
  simple_r <-
    (comp_fv / inst - t) / (t * (t + 1) / 2)  # now get the equivalent simple rate per instalment
  
  # now get the yearly rate
  simple_r <- simple_r * 12
  
  return(simple_r)
}


# now find it for several year and rates

full_table <- function(int, year, inst, comp_freq) {
  fl <- matrix(nrow = length(int), ncol = length(year))
  
  for (i in 1:length(int)) {
    for (j in 1:length(year)) {
      fl[i, j] <- simple_interest(int[i], year[j], inst, comp_freq)
    }
  }
  
  fl <- data.frame(fl)
  
  row.names(fl) <- paste0(int * 100, "%", "")
  colnames(fl) <- paste(year, "Y", " ")
  
  return(fl)
}

# now get the summary table

summary_table <- function(int, year, inst, comp_freq) {
  simp_rate <- c()
  simp_fv <- c()
  comp_fv <- c()
  for (i in 1:length(int)) {
    # get the simp int from func simple_interest
    simp_rate[i] <-
      simple_interest(int[i], year[i], inst, comp_freq)
    # get the simple rate fv
    simp_fv[i] <-
      inst * ((year[i] * 12 + simp_rate[i]  / 12 * (year[i] * 12) * (year[i] * 12 + 1) / 2))
    # get int per payment
    ip <-
      (1 + int[i] / comp_freq) ^ (comp_freq / 12) - 1 # interest rate per period
    # get the comp rate fv
    comp_fv[i] <-
      fv.annuity(ip, year[i] * 12, -inst, type = 1) # calculate the future value at compound rate
  }
  # generate the summary table
  Summary_Table <-
    data.frame(
      Tenure = year,
      Comp_Rate = int,
      Simple_Rate = simp_rate,
      Comp_FV = comp_fv,
      Simp_FV = simp_fv
    )
  return(Summary_Table)
}

# Now get the result

year <- c(3, 5, 10)

int <- c(5.50, 6.00, 6.5) / 100

inst <- 25000

comp_freq <- 1

Simple_Rate <- full_table(int, year, inst, comp_freq)

Simple_Rate

glance_table <- summary_table(int, year, inst, comp_freq)

glance_table

if (as.vector(Sys.info()['sysname']) == "Windows") {
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
}

write.xlsx(Simple_Rate,
           "full_table.xlsx",
           row.names = T,
           asTable = T)

# unit test
Int.Per_Payment <-
  (1 + .065 / 1) ^ (1 / 12) - # interest rate per period for compound rate
  1
comp_fv <- fv.annuity(Int.Per_Payment, 120, -25000, type = 1)
comp_fv

fv <- 25000 * (120 + Simple_Rate[3, 3] / 12 * 120 * (120
                                                     + 1) / 2)
fv
