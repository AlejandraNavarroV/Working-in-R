##Data UNESCO
###Installing packages

install.packages("foreign")

#Loading packages

library(tidyverse)
library(arsenal)
library(pander)
library(texreg)
library(ggeffects)
library(car)
library(effectsize)
library(broom)
library(MultinomialCI)
library(foreign)

##Cargando 

survey <- read_csv(file = "../RM3/survey.csv")
summary(survey)
names(survey)

###creating factor variables

# create a factor variable for treatment group and gender 
survey <- survey %>% 
  mutate(sex_child = factor(sex_child, 
                              levels = c(1,2), 
                              labels = c("Female", "Male")),
         ethnicity_child = factor(ethnicity_child,
         levels=c(1,2,3,4,5),
         labels=c("White",
                  "Mixed or multiple ethnic groups",
                  "Asian or Asian British",
                  "Black, Black British, Caribbean or African",
                  "Other ethnic group")),
        sex_HRP = factor(sex_HRP,
                       levels = c(1,2),
                       labels = c("Female","Male")),
        parent_smoke=factor(parent_smoke,
                            levels=c(0,1),
                            labels=c("No smokers","Smoking parent")),
        housing_damp=factor(housing_damp,
                            levels = c(0,1),
                            labels=c("No damp","Damp housing")))

# 1) Use a table to describe the sample characteristics using the appropriate descriptive statistics. Visualise the distributions of the two key variables of interest with appropriate charts.----

##Sample characteristics----

###Verifying null values

is.na(survey) %>% sum()
is.na(survey) %>% mean()

###Creating a descriptive statistics table 

Sample_car <- tableby(~ sex_child +ethnicity_child+sex_HRP+
                        parent_smoke+income+housing_damp+cd_score, 
                      data = survey, na.rm = T)

summary(object = Sample_car, 
        text = T, 
        labelTranslations = c(
          sex_child = "Sex of the child", 
          ethnicity_child = "Ethnic group of the child (2021 Census Categories)",
          sex_HRP="Sex of the Household Reference Person",
          parent_smoke="Presence of a smoker parent in the home",
          income="Weekly household income (£)",
          housing_damp="Presence of damp in the home",
          cd_score="Child development score"), 
        digits = 2, 
        title = "Table 1.Sample Descriptive Statistics",
        control=tableby.control(numeric.stats=c("meansd","range"),
                                cat.stats=c("countpct"),
                                total=FALSE))

###Visualising distribution of variables of interest----

####Income

ggplot(data=survey)+
  aes(x=income,y=stat(density))+
  geom_histogram(bindwith = 5,col="black",fill="LightBlue",alpha=0.8)+
  labs(title="Income distribution",
       x="Income",
       caption="Source:Fictional database by Fransham (2021)")+
  theme(plot.title = element_text(hjust = 0.5))

###Child development outcomes

ggplot(data=survey)+
  aes(x=cd_score,y=stat(density))+
  geom_histogram(bindwith = 5,col="black",fill="LightBlue",alpha=0.8)+
  labs(title="Child development outcomes",
       x="Child development score",
       caption="Source:Fictional database by Fransham (2021)")+
  theme(plot.title = element_text(hjust = 0.5))

# 2) Assess the difference in mean child development score between male and female children, using both estimation and hypothesis testing approaches. What does this tell us about the difference between male and female children in the population? Consider carefully the meaning of confidence intervals and statistical significance.----

###Estimation

####Confidence interval for child development scores

summary(survey$cd_score)
t.test(survey$cd_score)$conf.int

####Confidence interval for sex of the child

prop.table(table(survey$sex_child))

multinomialCI(survey$sex_child %>% table(), alpha = 0.05)

###Hypothesis test

####Two sample t test

t.test(cd_score ~ sex_child, data = survey)%>% 
  tidy() %>% 
  pander(caption = "T test of child development score and sex of the child")

# 3) Using a regression model, investigate the relationship between child development score and income. Investigate whether there is evidence of a non-linear relationship----

###Simple regression

model_1 <- lm(formula = cd_score ~ income, 
              data = survey)

###Checking assumptions 

plot(model_1)

###Assuming non-linear relationship

non_linear_m1 <- lm(cd_score ~ income + I(income^2), 
         data = survey)
vif (non_linear_m1)

###Table of both tables

knitreg(list(model_1, non_linear_m1), digits = 4 )

###Plotting relationship

ggplot(data = survey) +
  aes(x = cd_score, y = income) + 
  geom_point()+
labs(title="Child development outcomes, by income",
     x="Child development score",
     caption="Source:Fictional database by Fransham (2021)")+
  geom_smooth()

     ###Analysis: xxxx. Multicollinearity in models with squared terms
# Assess with week8b. From last week you may remember that we hope to avoid multicollinearity, which describes a situation when two or more explanatory variables are strongly linearly correlated. When including the square of a variable that only takes positive values (as is the case with age), it is possible that a multicollinearity problem will arise. This can cause the standard errors to be larger than they would otherwise be, which could lead you to fail to reject the null hypothesis (i.e. cause a false negative result).

m2 <- lm(cd_score ~ scale(income) + I(scale(income)^2), 
         data = survey)

vif(m2)

# # 4) Using a second regression model, investigate the relationship between child development score----
# and income, controlling for the demographic and household characteristics of the children in the survey.
# Pay close attention to the types of variables and ensure you consider size, uncertainty, direction and significance.

model_2 <- lm(formula = cd_score ~ income+sex_child+ethnicity_child+sex_HRP+parent_smoke+housing_damp, 
              data = survey)

coef(model_2)

confint(model_2)[12]

###Checking multicollinearity

vif(model_2)

###Comparing models

knitreg(list(model_1, model_2), 
        caption = "Linear regression models of child development scores", 
        custom.coef.names = c("(Intercept)", "Weekly income (£)", 
                              "Male", 
                              "Mixed or multiple ethnic groups",
                              "Asian or Asian British",
                              "Black, Black British, Caribbean or African",
                              "Other ethnic group",
                              "Male",
                              "Smoking parent",
                              "Damp housing"), 
        groups = list("Sex of the child (ref. category: Female)" = c(3),
                      "Ethnicity of the child (ref.category: White)"=c(4,5,6,7),
                      "Sex of the Household Reference Person(ref. category: Female)"=c(8),
                      "Presence of a smoker parent (ref. category: No smoking)"=c(9),
                      "Evidence of damp in the home (ref. category: no damp)"=10))
##Verify categories ethnicity and sex
# 5) Use the results of the model to consider whether income causes differences in child development scores.----
###Conclusions:Assess the relationship according to HM6

# 6) Create an indicator variable for being at an expected level of development (0 = below, 1 = at or above).----

###Creating variable
survey <- survey %>% 
  mutate(exp_dev_level=ifelse(cd_score>=160,yes = 1, no=0))
summary(survey$exp_dev_level)

###Converting into factor variable

survey <- survey %>% 
  mutate(exp_dev_level = factor(exp_dev_level, 
                            levels = c(0,1), 
                            labels = c("Below expected level", "Above expected level")))

# 7) What proportion of children are at the expected level of development in this sample?----

###Creating table

Expected_dev_table <- tableby(
  formula = ~exp_dev_level, 
  data = survey)

Expected_dev_table

summary(object = Expected_dev_table,
        text = T, 
        labelTranslations = c(
          exp_dev_level = "Level of development"), 
        digits = 2, 
        title = "Expected level of child development scores")

###Conclusion: what proportion is above: 85.8%

# 8) Using a regression model, analyse how the probability of being at the expected level of development is associated with income. ----
# Calculate two models, one without controls, and one model controlling for the demographic and household characteristics of the children in the survey. 
# Pay close attention to the types of variables and ensure you consider size, uncertainty, direction and significance. Report a more interpretable measure 
# (not raw model coefficients).

###Model 1: Only level of development and income

#### simple model, one explanatory variable. This is a logistic regression

model_3 <- glm(exp_dev_level ~ income, 
          family = binomial(link = "logit" ), 
          data = survey)

coef(model_3)

####convert to odds ratio

exp(coef(model_3))

###Confidence intervals 

exp(confint(model_3))

###Complex model, controlling by demographic and household variables

model_4 <- glm(exp_dev_level ~ income+sex_child+ethnicity_child+sex_HRP+parent_smoke+housing_damp, 
          family = binomial(link = "logit"), 
          data = survey)
coef(model_4)
exp(coef(model_4))
summary(model_4)

###Exploring confidence intervals

exp(confint(model_4))
coef(model_4)

###Checking multicollinearity

vif(model_4)

###Checking outliers

influence.measures(model_4)$is.inf[, "cook.d"] %>% sum()

###Reporting results of both models

knitreg(list(model_3, model_4), 
        override.coef = list(exp(coef(model_3) ), 
                             exp(coef(model_4) ) ), 
        override.se = list(summary(model_3)$coefficients[, "z value"], 
                           summary(model_4)$coefficients[, "z value"]),
        caption = "Odds ratios for logistic regression models of expected level of child development", 
        custom.coef.names = c("(Intercept)", "Weekly income (£)", 
                              "Male", 
                              "Mixed or multiple ethnic groups",
                              "Asian or Asian British",
                              "Black, Black British, Caribbean or African",
                              "Other ethnic group",
                              "Male",
                              "Smoking parent",
                              "Damp housing"), 
        groups = list("Sex of the child (ref. category: Female)" = c(3),
                      "Ethnicity of the child(ref.category: White)"=c(4,5,6,7),
                      "Sex of the Household Reference Person(ref. category: Female)"=c(8),
                      "Presence of a smoker parent (ref. category: No smoking)"=c(9),
                      "Evidence of damp in the home (ref. category: no damp)"=10),
        custom.note = "%stars. z-values in brackets", 
        digits = 3)

#Verify categories ethnicity and sex
###Conclusion: Types of variables and ensure you consider size, uncertainty, direction and significance. Report a more interpretable measure 
# (not raw model coefficients).

# 9) Visualise the results of the model in a predicted probabilities graph that shows how the probability of being at the expected level of development is related to household income. ----
# Ensure that there are at least two lines on the graph, showing the relationship for children with different characteristics of your choice.

summary(survey$income)

m4_predict <- ggpredict(model = model_4, 
                        terms = c("income [0:400]","parent_smoke"))

m4_predict

plot(m4_predict) + ylim(0,1) 

attr(m4_predict, which = "constant.values")

###Pending this

# 10) Use the results of the model to consider whether income causes a change ----
# in the probability of being at the expected level of development.

###Pending: Assess the relationship according to HM6.Reading model fit statistics core exercises week6.See also week 8 for all models

# Ensure you check the robustness of the all above analyses carefully,
# particularly that the underlying assumptions are met.

#Part B----
##Working with the survey.csv dataset
##Loading data

rct <- read_csv(file = "../RM3/rct.csv")
summary(rct)
names(rct)

# 1) Check and report the sample flow through the study ----
# (i.e. the number in each treatment group at baseline and post-intervention).

###Tranforming variables as factors

rct <- rct %>% 
  mutate(treatment=factor(treatment,
                          levels=c(0,1),
                          labels=c("Control group", "Treatment group")),
                          sex_child = factor(sex_child, 
                            levels = c(1,2), 
                            labels = c("Female", "Male")),
         ethnicity_child = factor(ethnicity_child,
                                  levels=c(1,2,3,4,5),
                                  labels=c("White",
                                           "Mixed or multiple ethnic groups",
                                           "Asian or Asian British",
                                           "Black, Black British, Caribbean or African",
                                           "Other ethnic group")),
         sex_HRP = factor(sex_HRP,
                          levels = c(1,2),
                          labels = c("Female","Male")),
         parent_smoke=factor(parent_smoke,
                             levels=c(0,1),
                             labels=c("No smokers","Smoking parent")),
         housing_damp=factor(housing_damp,
                             levels = c(0,1),
                             labels=c("No damp","Damp housing")))


### Creating a table

summary(rct$treatment)
table(rct$treatment, useNA = "always")

summary(rct$cd_score1)

rct %>% filter(treatment == "Control group") %>% pull(cd_score1) %>% summary()

Sample_characteristics <- tableby(
  formula = treatment ~ cd_score1+ cd_score2, 
  data = rct,
  tableby.control(total = TRUE))

summary(object = Sample_characteristics,
        text = T, 
        labelTranslations = c(cd_score1="Child development score, measured before the intervention",
                              cd_score2="Child development score, measured after the intervention"), 
        digits = 2, 
        test=F,
        total=F,
        title = "Table:Baseline and Intervention flow, by intervention condition")


###Creating a descriptive statistics table 


Sample_characteristics <- tableby(
  formula = treatment ~ sex_child +ethnicity_child+sex_HRP+
    parent_smoke+income+housing_damp+cd_score1+ cd_score2, 
  data = rct)

summary(object = Sample_characteristics,
        text = T, 
        labelTranslations = c(
          sex_child = "Sex of the child", 
          ethnicity_child = "Ethnic group of the child (2021 Census Categories)",
          sex_HRP="Sex of the Household Reference Person",
          parent_smoke="Presence of a smoker parent in the home",
          income="Weekly household income (£)",
          housing_damp="Presence of damp in the home",
          cd_score1="Child development score, measured before the intervention",
          cd_score2="Child development score, measured after the intervention"), 
        digits = 2, 
        test=F,
        total=F,
        title = "Table:Sample Descriptive Statistics, by treatment")

# 2) Assess the balance between treatment and control groups in terms of the pre-intervention characteristics ----
# of the participants
##Pending description

# 3) Calculate a regression-based estimate of the treatment effect that accounts for baseline differences. What if any is the intervention effect? ----
# Ensure you consider size, uncertainty, direction and significance.

###Modeling treatment effect

install.packages("jtools")
library(jtools)

model_5 <- lm(cd_score2 ~ cd_score1 + treatment, data = rct) 

summary(model_5)

knitreg(model_5,
        caption = "Regression-based estimate of treatment effect in child development scores", 
        custom.coef.names = c("(Intercept)", 
                              "Baseline child development score",
                              "Treatment"),
        digits=3)

###Checking assumptions

plot(model_5)
vif(model_5)

###Constructing Confidence Intervals

confint(model_5)

###Analysis: Pending to identify de effect including size, uncertainty, direction and significance

# 4) Calculate and Report a standardised measure of the effect size, and interpret what it means.----
# create a second linear model, scaling the outcome variable to z scores using scale()

model_6 <- lm(scale(cd_score2) ~ cd_score1 + treatment, data = rct) 
summary(model_6)
coef(model_6)

confint(model_6)

###Checking assumptions

vif(model_6)

###Calculating Cohen's d


cd_score2_sdp <- sd_pooled(cd_score2 ~ treatment, 
                           data = rct)

coef(model_5)[3] / cd_score2_sdp
confint(model_5)[3, ] / cd_score2_sdp

cd_score2_sdp
sd(rct$cd_score2, na.rm = T)

###Analysis: Pending depending on week 8 video and verify CI

# 5) Create a chart that visualises the treatment effect.----

ggplot(rct) +
  aes(x = (cd_score2 - cd_score1), y = treatment)+
  geom_point(position = position_jitter(height = 0.1) ) +
  stat_summary(fun = "mean", colour = "Blue")+
  labs(title="Treatment assignment",
       x="Change in Baseline and Post-intervention child development scores",
       caption="Source:Fictional database by Fransham (2021)")
  
