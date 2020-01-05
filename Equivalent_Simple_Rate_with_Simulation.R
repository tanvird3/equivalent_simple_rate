# for recurring deposit at simple rate
# fv=(R+R*i*n) + (R+R*i*(n-1))+ (R+R*i*(n-2))+ .....+ (R+R*i*1)
# fv=n*R+ iR ( n+(n-1)+(n-2)+.....+1)
# fv=n*R+ iR * n(n+1)/2
# fv= R {n+i* n(n+1)/2}

library(FinCal)
library(openxlsx)
library(plyr)
library(dplyr)

options(scipen = 999)

# maturity of the product
tenure <- c(5, 10)

# compound interest rate of the product
int <- c(.06, .065)

# compounding frequency of the product
comp <- 1

# installment size of the product
inst <- 25000


equivalent_rate <- function(int, comp, tenure, inst) {
  fva <- c()
  future_val <- c()
  equivalent_int <- c()
  
  # loop over the given interest rates and tenure
  for (j in 1:length(int)) {
    # a sequence of interest rate to check the closest equivalent simple int rate
    int_try <-
      seq(from = int[j],
          to = int[j] + .15,
          by = .000001)
    
    # calculate the interest rate per period
    Int.Per_Payment <-
      (1 + int[j] / comp) ^ (comp / 12) - # interest rate per period for compound rate
      1
    # future value of the compound int
    fva[j] <-
      fv.annuity(Int.Per_Payment,
                 pmt = -inst,
                 tenure[j] * 12,
                 type = 1)
    
    # now loop over the vector of the probalbe simple interest rates and calculate the future vavlues
    fvs <- c()
    for (i in 1:length(int_try)) {
      # future value for the simple int
      fvs[i] <-
        inst * ((tenure[j] * 12 + int_try[i]  / 12 * (tenure[j] * 12) * (tenure[j] * 12 + 1) / 2))
    }
    
    # check the closest difference between the compound rate fv and the simulated simple rate fv and get the fv and simple rate
    k <- abs(fvs - fva[j])
    xx <-
      which(k == min(k))
    future_val[j] <- fvs[xx]
    equivalent_int[j] <-
      int_try[xx]
  }
  
  # now get the summary table
  sum_tab <-
    data.frame(
      Tenure = tenure,
      Com_Int_Rate = int,
      Simple_Int_Rate = equivalent_int,
      FV_Comp = round(fva, 2),
      FV_Simple = round(future_val, 2)
    )
  return(sum_tab)
}

ER <- equivalent_rate(int, comp, tenure, inst)

if (as.vector(Sys.info()['sysname']) == "Windows") {
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
}

openxlsx::write.xlsx(ER, "mao1.xlsx", row.names = F)
