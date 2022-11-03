set.seed(1)
x = rnorm(50, 10, 2)

out = c()
for (i in 1:1000) {
  sam = sample(x,replace=TRUE)
  out[i] = (sd(sam))/(mean(sam))
}

print(quantile(out, c(0.025, 0.975)))
