# Stolen from statpsych
# Delete when jamovi has a version of statpsych with this function included
# https://github.com/dgbonett/statpsych/blob/26913a8e69961ed4e27260475c98d6940e275f0b/R/statpsych1.R

esci_ci.2x2.mean.mixed <- function(alpha, y11, y12, y21, y22) {
  n1 <- length(y11)
  n2 <- length(y21)
  diff1 <- y11 - y12
  diff2 <- y21 - y22
  ave1 <- (y11 + y12)/2
  ave2 <- (y21 + y22)/2
  vd1 <- var(diff1)
  vd2 <- var(diff2)
  va1 <- var(ave1)
  va2 <- var(ave2)
  est1 <- mean(diff1) - mean(diff2)
  se1 <- sqrt(vd1/n1 + vd2/n2)
  df1 <- (se1^4)/(vd1^2/(n1^3 - n1^2) + vd2^2/(n2^3 - n2^2))
  tcrit1 <- qt(1 - alpha/2, df1)
  t1 <- est1/se1
  p1 <- 2*(1 - pt(abs(t1), df1))
  LL1 <- est1 - tcrit1*se1
  UL1 <- est1 + tcrit1*se1
  row1 <- c(est1, se1, t1, df1, p1, LL1, UL1)
  est2 <- (mean(diff1) + mean(diff2))/2
  se2 <- sqrt(vd1/n1 + vd2/n2)/2
  df2 <- (se2^4)/(vd1^2/((n1^3 - n1^2)*16) + vd2^2/((n2^3 - n2^2)*16))
  tcrit2 <- qt(1 - alpha/2, df2)
  t2 <- est2/se2
  p2 <- 2*(1 - pt(abs(t2), df2))
  LL2 <- est2 - tcrit2*se2
  UL2 <- est2 + tcrit2*se2
  row2 <- c(est2, se2, t2, df2, p2, LL2, UL2)
  est3 <- mean(ave1) - mean(ave2)
  se3 <- sqrt(va1/n1 + va2/n2)
  df3 <- (se3^4)/(va1^2/(n1^3 - n1^2) + va2^2/(n2^3 - n2^2))
  tcrit3 <- qt(1 - alpha/2, df3)
  t3 <- est3/se3
  p3 <- 2*(1 - pt(abs(t3), df3))
  LL3 <- est3 - tcrit3*se3
  UL3 <- est3 + tcrit3*se3
  row3 <- c(est3, se3, t3, df3, p3, LL3, UL3)
  est4 <- mean(diff1)
  se4 <- sqrt(vd1/n1)
  df4 <- n1 - 1
  tcrit4 <- qt(1 - alpha/2, df4)
  t4 <- est4/se4
  p4 <- 2*(1 - pt(abs(t4), df4))
  LL4 <- est4 - tcrit4*se4
  UL4 <- est4 + tcrit4*se4
  row4 <- c(est4, se4, t4, df4, p4, LL4, UL4)
  est5 <- mean(diff2)
  se5 <- sqrt(vd2/n2)
  df5 <- n2 - 1
  tcrit5 <- qt(1 - alpha/2, df5)
  t5 <- est5/se5
  p5 <- 2*(1 - pt(abs(t5), df5))
  LL5 <- est5 - tcrit5*se5
  UL5 <- est5 + tcrit5*se5
  row5 <- c(est5, se5, t5, df5, p5, LL5, UL5)
  est6 <- mean(y11) - mean(y21)
  se6 <- sqrt(var(y11)/n1 + var(y21)/n2)
  df6 <- (se6^4)/(var(y11)^2/(n1^3 - n1^2) + var(y21)^2/(n2^3 - n2^2))
  tcrit6 <- qt(1 - alpha/2, df6)
  t6 <- est6/se6
  p6 <- 2*(1 - pt(abs(t6), df6))
  LL6 <- est6 - tcrit6*se6
  UL6 <- est6 + tcrit6*se6
  row6 <- c(est6, se6, t6, df6, p6, LL6, UL6)
  est7 <- mean(y12) - mean(y22)
  se7 <- sqrt(var(y12)/n1 + var(y22)/n2)
  df7 <- (se7^4)/(var(y12)^2/(n1^3 - n1^2) + var(y22)^2/(n2^3 - n2^2))
  tcrit7 <- qt(1 - alpha/2, df7)
  t7 <- est7/se7
  p7 <- 2*(1 - pt(abs(t7), df7))
  LL7 <- est7 - tcrit7*se7
  UL7 <- est7 + tcrit7*se7
  row7 <- c(est7, se7, t7, df7, p7, LL7, UL7)
  out <- rbind(row1, row2, row3, row4, row5, row6, row7)
  rownames(out) <- c("AB:", "A:", "B:", "A at b1:", "A at b2:", "B at a1:", "B at a2:")
  colnames(out) = c("Estimate", "SE", "t", "df", "p", "LL", "UL")
  return(out)
}
