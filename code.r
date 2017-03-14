


# Import Data -------------------------------------------------------------

meat = read.csv("meatspec.txt")

#Take a Look

head(meat)
str(meat)
class(meat)
mode(meat)
typeof(meat)


# Basic Stats -------------------------------------------------------------

summary(meat)

#fat will be our respons eVariable
#The other 100 var will be our feature vector

#predict the content of fat in each atpsample based on its near-infrared spectrum (sampled in 
#100 freq's')

#Spectra
?matplot

matplot(t(meat[,-101]), type = "l", lty=1)

#functional linear model , feature are functions
#FDA the more contex

# Correlation Analysis ----------------------------------------------------

#linear relationship betweeen features/covariates

cspec =  cor(meat[,-101])
round(cspec,2)

#Difficult to understand
#Summary : average corr

mean(cspec [upper.tri(cspec)]) #multicol in your face

#Visualizations
image(cspec)

# Simple Linear Regression ------------------------------------------------

#1 wavelength as feature and predict
# To cehck empirically the performance of our predictor
#We split our dataset in 2 parts (sample Splitting)
# Training Set = Used to build the model
# Test Set =  used to check the performanceuseful to make reliable inference in high dim setup

tr.idx = 1:150
tr.data = meat[tr.idx,]
te.data = meat[-tr.idx,]

#consider just v as a feature
mod1 = lm(fat~ V10,data = tr.data)
mod1
summary(mod1)
plot(tr.data$V10,tr.data$fat,
     pch=19,col="blue")
abline(mod1,lwd=3,col="green")


#Training Error
mean((tr.data$fat-fitted(mod1))^2)
#Test Error
fat.te = predict(mod1,te.data)
mean((te.data$fat-fat.te)^2)
#More reliable measure of predictive perf, more theory


# All IN ! ----------------------------------------------------------------

mod.all = lm(lm(fat~ .,data = tr.data))
mod.all
summary(mod.all)

#1. unstable OLS
#2. Overfitting (tracking the noise)
#Training Error
mean((tr.data$fat-fitted(mod.all))^2)
#Test Error
fat.te = predict(mod.all,te.data)
mean((te.data$fat-fat.te)^2)
















