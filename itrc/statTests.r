## Thinking shiny app with changeable sample sizes, etc
## to illustarte tests and CIs, how you can get all three
## stars with virutally zero actual diff, as f(sample size)

sampleA <- rnorm(n=5000, mean=70, sd=2)
sampleB <- rnorm(n=5000, mean=71, sd=2)

par(mfrow=c(1,2))
hist(sampleA)
hist(sampleB)


wilcox.test(x=sampleA, y=sampleB)

	Wilcoxon rank sum test with continuity correction

data:  sampleA and sampleB
W = 8958700, p-value < 2.2e-16
alternative hypothesis: true location shift is not equal to 0


t.test(sampleA, conf.level=0.95)$conf.int
[1] 69.88890 69.99893
attr(,"conf.level")
[1] 0.95

t.test(sampleB, conf.level=0.95)$conf.int
[1] 70.90056 71.00983
attr(,"conf.level")
