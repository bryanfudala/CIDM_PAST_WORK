library(readxl)
library(AER)

#import data
df <- read_xlsx("Firm data_v2.xlsx")

#Defining the panel
df$`Key` <- as.factor(df$`Key`)
attach(df)


library(plm)
df <- pdata.frame(df, index = c("Key","Year"))

#create variables
#eliminate obs where assets and sales are 0

df1<-subset(df, df$Assets!=0 & df$Sales!=0)


#BF EDIT - log of total assets to create firm size
df1$firm_size = log(df1$Assets)

df1$profit = 100*(df1$Net_income+df1$Ad)/df1$Assets
df1$cap = 100*df1$Assets/df1$Sales
df1$ad_int = 100*df1$Ad/df1$Sales
df1$rd_int = 100*df1$RD/df1$Sales
df1$debt_mgt = 100*df1$Bad_debt/df1$Sales
df1$Inv_mgt <- 100*df1$Inv/df1$Sales



library(dplyr)
growth_rate <- function(x)(x/lag(x)-1)*100 
df1$growth <- growth_rate(df1$Sales) 

df1$GIC_Sectors <- as.factor(df1$GIC_Sectors)



#calculate relative market share within GIC sector 
df1 <- df1 %>%
  group_by(GIC_Sectors,Year) %>%
  mutate(share = (Sales/sum(Sales)) * 100)

#Descriptive statistics table
library(stargazer)
stargazer(as.data.frame(df1),type='html',digits=1,out='Descriptive statistics.html')


#pooled OLS with all Variables
bfmod1 <- lm(profit~ Assets + Net_income + RD + Bad_debt + Sales + Ad +  Inv + firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + GIC_Industries , data = df1)
coeftest(bfmod1, vcov. = vcovHC, type = "HC1")

#pooled OLS with created variables from formula logic 
bfmod2 <- lm(profit~  GIC_Industries +  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share, data = df1)
coeftest(bfmod2, vcov. = vcovHC, type = "HC1")

#pooled OLS using all variables along with dummy variable for GIC Sector
bfmod3 <- lm(profit~ Assets + Net_income + RD + Bad_debt + Sales + Ad + Inv + firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + GIC_Industries + GIC_Sectors  -1, data = df1)
coeftest(bfmod3, vcov. = vcovHC, type = "HC1")

#pooled OLS with created variables and using GIC Sector as dummy variable
bfmod4 <- lm(profit~  GIC_Industries +  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + GIC_Industries + GIC_Sectors  -1, data = df1)
coeftest(bfmod4, vcov. = vcovHC, type = "HC1")

# gather clustered standard errors in a list for Pooled OLS regressoin 
rob_se <- list(sqrt(diag(vcovHC(bfmod1, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod2, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod3, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod4, type = "HC1"))))


# generate the Pooled OLS table
stargazer(bfmod1, bfmod2, bfmod3, bfmod4,
          digits = 3,
          header = FALSE,
          type = "html", 
          se = rob_se,
          title = "OLS Regression models to analyze all variables including dummy on the effect of a Firm's profitability",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)"),
          out='Section 2 Regression OLS.html')

library(plm)





#individual fixed effects model
bfmod5 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share,
                 data = df1,
                 index = c("Key", "Year"),
                 model = "within")
coeftest(bfmod5, vcov. = vcovHC, type = "HC1")

#individual fixed effects model with interaction terms
bfmod6 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + (ad_int * share) + (ad_int * cap),
               data = df1,
               index = c("Key", "Year"),
               model = "within")
coeftest(bfmod6, vcov. = vcovHC, type = "HC1")

#individual and time fixed effects model
bfmod7 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share,
                    data = df1,
                    index = c("Key", "Year"),
                    model = "within",
                    effect="twoway")
coeftest(bfmod7, vcov. = vcovHC, type = "HC1")

#individual and time fixed effects model with interaction terms
bfmod8 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + (ad_int * share) + (ad_int * cap),
               data = df1,
               index = c("Key", "Year"),
               model = "within",
               effect="twoway")
coeftest(bfmod8, vcov. = vcovHC, type = "HC1")


# gather clustered standard errors in a list for Pooled OLS regressoin  & Time Effects
rob_se <- list(sqrt(diag(vcovHC(bfmod2, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod4, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod5, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod6, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod7, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod8, type = "HC1"))))


# generate OLS and Time Effects Model
stargazer(bfmod2, bfmod4, bfmod5, bfmod6, bfmod7, bfmod8,
          digits = 3,
          header = FALSE,
          type = "html", 
          se = rob_se,
          title = "Individual & Time Effects model on the effect of a Firm's profitability",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
          out='Section 2 Time Effects model.html')

# SECTION 3A

#Use only 1987-2006
df1$year2 <- as.numeric(df1$Year)
df2 <- subset(df1,df1$year2<=20)

#pooled OLS with created variables from formula logic 
bfmod12 <- lm(profit~  GIC_Industries +  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share, data = df2)
coeftest(bfmod12, vcov. = vcovHC, type = "HC1")

#pooled OLS with created variables and using GIC Sector as dummy variable
bfmod14 <- lm(profit~  GIC_Industries +  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + GIC_Industries + GIC_Sectors  -1, data = df2)
coeftest(bfmod14, vcov. = vcovHC, type = "HC1")

#individual fixed effects model
bfmod15 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share,
               data = df2,
               index = c("Key", "Year"),
               model = "within")
coeftest(bfmod15, vcov. = vcovHC, type = "HC1")

#individual fixed effects model with interaction terms
bfmod16 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + (ad_int * share) + (ad_int * cap),
                data = df2,
                index = c("Key", "Year"),
                model = "within")
coeftest(bfmod16, vcov. = vcovHC, type = "HC1")

#individual and time fixed effects model
bfmod17 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share,
               data = df2,
               index = c("Key", "Year"),
               model = "within",
               effect="twoway")
coeftest(bfmod17, vcov. = vcovHC, type = "HC1")

#individual and time fixed effects model with interaction terms
bfmod18 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + (ad_int * share) + (ad_int * cap),
               data = df2,
               index = c("Key", "Year"),
               model = "within",
               effect="twoway")
coeftest(bfmod18, vcov. = vcovHC, type = "HC1")

# gather clustered standard errors in a list for Pooled OLS regressoin  & Time Effects
rob_se <- list(sqrt(diag(vcovHC(bfmod12, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod14, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod15, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod16, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod17, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod18, type = "HC1"))))


# generate OLS and Time Effects Model
stargazer(bfmod12, bfmod14, bfmod15, bfmod16, bfmod17, bfmod18,
          digits = 3,
          header = FALSE,
          type = "html", 
          se = rob_se,
          title = "(1987-2006) Individual & Time Effects model on the effect of a Firm's profitability",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
          out='Section 3A Time Effects model.html')


stargazer(as.data.frame(df2),type='html',digits=1,out='Descriptive statistics subset.html')

#SECTION 3B 




#Drop observations with missing values
df3 <- na.omit(df1)


#pooled OLS with created variables from formula logic 
bfmod32 <- lm(profit~  GIC_Industries +  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share, data = df3)
coeftest(bfmod32, vcov. = vcovHC, type = "HC1")

#pooled OLS with created variables and using GIC Sector as dummy variable
bfmod34 <- lm(profit~  GIC_Industries +  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + GIC_Industries + GIC_Sectors  -1, data = df3)
coeftest(bfmod34, vcov. = vcovHC, type = "HC1")

#individual fixed effects model
bfmod35 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share,
                data = df3,
                index = c("Key", "Year"),
                model = "within")
coeftest(bfmod35, vcov. = vcovHC, type = "HC1")

#individual fixed effects model with interaction terms
bfmod36 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + (ad_int * share) + (ad_int * cap),
                data = df3,
                index = c("Key", "Year"),
                model = "within")
coeftest(bfmod36, vcov. = vcovHC, type = "HC1")

#individual and time fixed effects model
bfmod37 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share,
                data = df3,
                index = c("Key", "Year"),
                model = "within",
                effect="twoway")
coeftest(bfmod37, vcov. = vcovHC, type = "HC1")

#individual and time fixed effects model with interaction terms
bfmod38 <- plm( profit ~  firm_size + cap + ad_int + rd_int + debt_mgt + Inv_mgt + growth + share + (ad_int * share) + (ad_int * cap),
                data = df3,
                index = c("Key", "Year"),
                model = "within",
                effect="twoway")
coeftest(bfmod38, vcov. = vcovHC, type = "HC1")

# gather clustered standard errors in a list for Pooled OLS regressoin  & Time Effects
rob_se <- list(sqrt(diag(vcovHC(bfmod32, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod34, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod35, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod36, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod37, type = "HC1"))),
               sqrt(diag(vcovHC(bfmod38, type = "HC1"))))


# generate OLS and Time Effects Model
stargazer(bfmod32, bfmod34, bfmod35, bfmod36, bfmod37, bfmod38,
          digits = 3,
          header = FALSE,
          type = "html", 
          se = rob_se,
          title = "(Null variables Omitted) Individual & Time Effects model on the effect of a Firm's profitability",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
          out='Section 3B Time Effects model.html')
