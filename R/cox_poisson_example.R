
dat <- data.frame(enter = rep(0, 4), exit = 1:4,
                  event = rep(1, 4), x = c(0, 1, 0, 1))
dat

#cox
library(eha)

fit <- coxreg(Surv(enter, exit, event) ~ x, data = dat)
print(summary(fit), short = TRUE)

haz <- hazards(fit, cum = FALSE)
haz

# poisson

datB <- toBinary(dat)
datB

fit2 <- glm(event ~ riskset + x, family = poisson,
            data = datB)
(co <- coefficients(fit2))

co[2:4] <- co[2:4] + co[1] # Calculate the hazard atoms.
haz.glm <- exp(co[1:4])

xx <- cbind(haz[[1]], haz.glm)
colnames(xx) <- c("Time", "coxreg", "glm")
xx

swe <- merge(swepop,swedeaths)

swe$age <- factor(swe$age)
fit <- glm(deaths ~ offset(log(pop)) + year + sex + age,
           family = poisson, data = swe)
drop1(fit, test = "Chisq")

round(summary(fit)$coefficients[c(1:3), ], 3) # First 3 rows.
round(summary(fit)$coefficients[c(19:21), ], 3) # Last three rows


fit1 <- glm(deaths ~ offset(log(pop)) + sex + year * age,
            family = poisson, data = swe)
drop1(fit1, test = "Chisq")


beta <- coefficients(fit)[2:3]
alpha <- coefficients(fit)[-(2:3)] # Remove sex and year
alpha[2:length(alpha)] <- alpha[2:length(alpha)] + alpha[1]
lambda.2019 <- exp(alpha)
lambda.2020 <- exp(alpha + beta[1])


par(las = 1)
plot(ages, lambda.2019, ylim = c(0, 0.15), type = "S",
     xlab = "Age", ylab = "Mortality")
lines(ages, lambda.2020, type = "S", lty = 2)
abline(h = 0)
legend("topleft", legend = c(2019, 2020), lty = 1:2)
