# for recurring simple deposit
# fv=(R+R*i*n) + (R+R*i*(n-1))+ (R+R*i*(n-2))+ .....+ (R+R*i*1)
# fv=n*R+ iR ( n+(n-1)+(n-2)+.....+1)
# fv=n*R+ iR * n(n+1)/2
# fv= R {n+i* n(n+1)/2}
# if Simple FV=Compound FV

# load packages
library(FinCal)
library(openxlsx)
library(dplyr)


# set the parameter
simple_int_rate <- 6.0267 / 100

installment <- 1000

tenure <- 3

# compounding frequency
compounding <- 1

# gap for checking the installment size
gap_check <- .000001

# simple_fv <- 39344.82

# or calculate it
# fv=R {n+i* n(n+1)/2}
simple_fv <-
  installment * (tenure * 12 + simple_int_rate / 12 * (tenure * 12) * (tenure * 12 +
                                                                         1) / 2)

# the blank vectors
# for fv compounding
fv_comp <- c()

# the compounding rate
comp_rate <- c()

# the first element compounding rate and compounding future value
comp_rate[1] <- simple_int_rate - gap_check

Int.Per_Payment <-
  (1 + comp_rate[1] / compounding) ^ (compounding / 12) - # interest rate per period for compound rate
  1

fv_comp[1] <- fv.annuity(Int.Per_Payment,
                         pmt = -installment,
                         tenure * 12,
                         type = 1)

# the second element of compounding rate and compounding future value
comp_rate[2] <- comp_rate - gap_check

Int.Per_Payment <-
  (1 + comp_rate[2] / compounding) ^ (compounding / 12) - # interest rate per period for compound rate
  1

fv_comp[2] <- fv.annuity(Int.Per_Payment,
                         pmt = -installment,
                         tenure * 12,
                         type = 1)

# now set a while loop to find the closest one, as long as the new fv has smaller gap with
# the last one, keep runnign the loop,
while (abs(fv_comp[length(fv_comp)] - simple_fv) < abs(fv_comp[length(fv_comp) -
                                                               1] - simple_fv)) {
  comp_rate[length(comp_rate) + 1] <-
    comp_rate[length(comp_rate)] - gap_check
  Int.Per_Payment <-
    (1 + comp_rate[length(comp_rate)] / compounding) ^ (compounding / 12) - # interest rate per period for compound rate
    1
  
  fv_comp[length(fv_comp) + 1] <- fv.annuity(Int.Per_Payment,
                                             pmt = -installment,
                                             tenure * 12,
                                             type = 1)
  
}

# the equivalent compounding future values
fv_comp[length(fv_comp) - 1]

# the equivalnet compounding rate
comp_rate[length(comp_rate) - 1]