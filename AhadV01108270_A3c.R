setwd("C:\\Users\\Ahad\\OneDrive\\Pictures\\Documents\\Desktop\\scma")
#install.packages('AER')
library(AER)

data("Affairs")
head(Affairs)
unique(Affairs$affairs)
table(Affairs$affairs)

## from Table 22.4 in Greene (2003)
fm.tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating ,
                  data = Affairs)
fm.tobit2 <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
                   right = 4, data = Affairs)

summary(fm.tobit)
summary(fm.tobit2)

#Fit a Tobit Model to real data
unique(df$state_1)

df = read.csv('NSSO68.csv', header=TRUE)
dput(names(df))
df_CHTSD = df[df$state_1== 'CHTSD',]
vars <- c("Sector", "hhdsz", "Religion", "Social_Group", "MPCE_URP", "Sex", "Age", "Marital_Status", "Education", "chicken_q", "chicken_v")

df_CHTSD_p = df_CHTSD[vars]
names(df_CHTSD_p)

df_CHTSD_p$price = df_CHTSD_p$chicken_v / df_CHTSD_p$chicken_q
names(df_CHTSD_p)

summary(df_CHTSD_p)

head(table(df_CHTSD_p$chicken_q))

dim(df_CHTSD_p)
# Fitting a Multiple Linear regression Model

fit = lm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price , data=df_CHTSD_p)
summary(fit)

# Fitting a Tobit Model to the data
install.packages('GGally')
install.packages('VGAM')
install.packages('ggplot2')
exp(-1.104e+00)
sd(df_CHTSD_p$chicken_q)

#var(require(ggplot2)
require(GGally)
require(VGAM)

ggpairs(df_CHTSD_p[, c("chicken_q", "MPCE_URP", "price")])

m <- vglm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price, tobit(Lower = 0), data = df_CHTSD_p)
summary(m)

exp(-1.032e+00)
sd(df_CHTSD_p$chicken_q)
df_CHTSD_p$price[is.na(df_CHTSD_p$price)] <- 0

m <- vglm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price, tobit(Lower = 0), data = df_CHTSD_p)
summary(m)