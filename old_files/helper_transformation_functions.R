### I DON'T THINK WE NEED THIS FILE!!!! ###

library(tidyverse)

# inverse hyperbolic since function
ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}

# hyperbolic sine function
hs = function(x) {
  0.5*exp(-x)*(exp(2*x) - 1)
}

# data
dfX = data_frame(x = seq(-2, 2, 0.01), 
                 ihs = ihs(x), 
                 hs1 = sinh(x), 
                 hs2 = hs(x))

# plot
ggplot(data = dfX, aes(x = x)) +
  stat_function(aes(color = "Inverse Hyperbolic Sine"), fun = ihs, ) +
  stat_function(aes(color = "Hyperbolic Sine (Manual)"), fun = hs) +
  stat_function(aes(color = "Hyperbolic Sine (Base)"), fun = sinh) +
  theme_bw() 


# The inverse hyperbolic sin (asinh in R) is defined as

# [asinh = ln(x + sqrt{1 + x^2})]

# Is a transformation that is asymptotically identical to (ln(2x)) 
# for large x, but still defined for 0 and negative values. This makes it a good transformation for the count data we often encounter (and which often includes 0 counts).

# https://www.nber.org/system/files/working_papers/w29998/w29998.pdf

