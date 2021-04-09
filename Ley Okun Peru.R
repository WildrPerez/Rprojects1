set.seed(123)

library(ggplot2)
library(ggthemes)
okun <- read.csv('/Users/cyobero/Desktop/okun.csv')
okun <- na.omit(okun)

g <- ggplot(okun, aes(y=y.pct.chg, x=unemp.chg)) + geom_point(col='blue') + theme_economist()
g + xlab('??? in Unemployment Rate (percentage points)') + ylab('%??? in real GDP') 

okun.lm <- lm(y.pct.chg ~ unemp.chg, data = okun)
summary(okun.lm)

okun$yhat <- okun.lm$fitted.values
okun.predict <- predict.lm(okun.lm, interval = 'confidence', level = .95, se.fit = TRUE)
okun$upr <- okun.predict$fit[, 3]
okun$lwr <- okun.predict$fit[, 2]
g <- ggplot(okun, aes(y=y.pct.chg, x=unemp.chg)) + geom_point(col='blue') + theme_economist()
g <- g + geom_line(aes(y=yhat), col='red') 
g <- g + xlab('??? in Unemployment Rate (Percentage Points)') + ylab('???% in real GDP')
g + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.1)