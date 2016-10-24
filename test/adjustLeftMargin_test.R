testdat <-
  data.frame(xval = factor(rep(c("A", "B"), each= 200)),
             yval = c(rnorm(200), rnorm(200, mean= .8)))

plotdat_test <-
  ggplot(testdat, aes(x = xval, y = yval)) +
  geom_point()

plotdat_test

plotdat_test2 <- adjustLeftMargin(plotdat_test)
plot(plotdat_test2)
