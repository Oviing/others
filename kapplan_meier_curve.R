nn <- dnnsurv(data = simsurvdata(100))

y_pred <- predict(nn, simsurvdata(100), type = "all")
y_pred



avg <- colMeans(y_pred$surv, na.rm = FALSE, dims = 1)

km_trt_fit <- survfit(Surv(time, status) ~ trt, data=simsurvdata(10))

cox <- coxph(Surv(time, status) ~ trt + sexF + age, data = simsurvdata(100))

cox_fit <- survfit(cox)
plot(cox_fit, main = "cph model", xlab="Days")
plot(avg, type = "s")

plot(cox_fit, col="red" )
par(new=TRUE)
plot(avg, type = "s", col="green" )

plot(cox_fit, col="orange")
points(avg, col="blue", pch="*")
lines(avg, col="blue",lty=1)

plot(avg, type = "s", col="red" )
points(cox_fit, col="blue", pch="*")
lines(cox_fit, col="blue",lty=1)

