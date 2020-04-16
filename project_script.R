wbfp <- read.csv("C:/Users/reidw/Desktop/stats_project_1/body_fat_data.csv")
linearMod <- lm(Percent_Body_Fat ~ Waist_Size, data=wbfp)
plot(x=wbfp$Waist_Size, y=wbfp$Percent_Body_Fat, pch = 16, cex = 1.3, col = "blue", main="Waist vs Body Fat", xlab = "Waist Size (in)", ylab="Body Fat Percentage")
abline(linearMod)
lm_summary <- summary(linearMod)
print(lm_summary)

anova_one_way <- aov(Percent_Body_Fat ~ Waist_Size, data=wbfp)
summary(anova_one_way)
# for t test with 28 dof, we have the 95% confidence interval bounded above and below the point estimate by
# t* = 2.048
t_crit  <- 2.048
se_int <- lm_summary$coefficients[1,2]
se_waist <- lm_summary$coefficients[2,2]

int_lower_95 <- coef(linearMod)[1] - (t_crit*se_int)
int_upper_95 <- coef(linearMod)[1] + (t_crit*se_int)
print(int_lower_95)
print(int_upper_95)

waist_lower_95 <- coef(linearMod)[2] - (t_crit*se_waist)
waist_upper_95 <- coef(linearMod)[2] + (t_crit*se_waist)
print(waist_lower_95)
print(waist_upper_95)






