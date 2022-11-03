#An empty matrix is created.
mat = matrix( ,nrow=500, ncol=2)

#The matrix is filled with the coefficient of variation in the first column and
#the standard deviation if the log data.
for (i in 1:500) {
  x = rnorm(n=100, mean=10, sd=2)
  cv = sqrt(var(x))/mean(x)
  sd_log = sd(log(x))
  mat[i,1] = cv
  mat[i,2] = sd_log
}

#The matrix is plotted
plot(mat,xlab="CV", ylab="SD_log")

# f(x) = x line is added to the plot
lines(0:1,0:1)

