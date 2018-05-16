source('load_packages_files_and_functions.R')

#######################################
## choose model: Version 1 = 15, Version 2 = 16, Version 3 = 17
models <- 15:17
#-------edit---------#-#
which_model <- models[1] 
#--------------------#-#
fit.file <- paste0('fitYear',which_model,'.Rdata')
fit <- readRDS(fit.file)

#######################################
x_axes <- c("strike_age","strike_male","roadtype","cas_severity","year","cas_male","cas_mode","strike_mode","cas_age")
#------EDIT------#--------------------------#
## Step 1: choose what to plot over
x_axis        <- x_axes[9]
## Step 2: choose which categories to keep from each covariate
years         <- 2015 # 2005:2015
severity      <- 'Fatal' # c('Fatal','Serious','Slight')
road          <- c("Motorway/A(M)", "A","B, C, Unclassified")
cas_gen       <- c(0,1) ## 0 is female, 1 is male
str_gen       <- c(0,1) ## 0 is female, 1 is male
casualty_mode <- 'car/taxi' # c('pedestrian',"cyclist","motorcycle","car/taxi","light goods","bus","heavy goods")
str_mode      <- 'car/taxi' # c('pedestrian',"cyclist","motorcycle","car/taxi","light goods","bus","heavy goods")
cas_ages      <- unique(test_data$cas_age)
str_ages      <- unique(test_data$strike_age)
#----------------#--------------------------#

#------RUN------#
## Step 3: subset data
test_data0 <- subset(test_data,year%in%years&cas_male%in%cas_gen&cas_severity%in%severity&cas_mode%in%casualty_mode&
    strike_mode%in%str_mode&strike_age%in%str_ages&cas_age%in%cas_ages&roadtype%in%road&strike_male%in%str_gen)
## Step 4: plot result
plot_observed_and_prediction(x_axis=x_axis,test_data0=test_data0,fit=fit,rate_per_casualty=T,rate_per_striker=F)
#---------------#

#######################################
#------EXAMPLES------#
## Ex 1
## plot rate of car/car fatality, by striker age per striker distance
x_axis <- 'strike_age'
plot_observed_and_prediction(x_axis=x_axis,test_data0=test_data0,fit=fit,rate_per_casualty=F,rate_per_striker=T)

## Ex 2
## plot rate of ped/car fatality, by year (on road type 'B, C, etc')
x_axis <- 'year'
road <- 'B, C, Unclassified'
years <- 2005:2015
casualty_mode <- 'pedestrian'
test_data0 <- subset(test_data,year%in%years&cas_male%in%cas_gen&cas_severity%in%severity&cas_mode%in%casualty_mode&
    strike_mode%in%str_mode&strike_age%in%str_ages&cas_age%in%cas_ages&roadtype%in%road&strike_male%in%str_gen)
plot_observed_and_prediction(x_axis=x_axis,test_data0=test_data0,fit=fit,rate_per_casualty=F,rate_per_striker=F)
## same again, with rate per casualty distance
plot_observed_and_prediction(x_axis=x_axis,test_data0=test_data0,fit=fit,rate_per_casualty=T,rate_per_striker=F)

## Ex 3
## compare with cyclist
casualty_mode <- 'cyclist'
test_data0 <- subset(test_data,year%in%years&cas_male%in%cas_gen&cas_severity%in%severity&cas_mode%in%casualty_mode&
    strike_mode%in%str_mode&strike_age%in%str_ages&cas_age%in%cas_ages&roadtype%in%road&strike_male%in%str_gen)
plot_observed_and_prediction(x_axis=x_axis,test_data0=test_data0,fit=fit,rate_per_casualty=F,rate_per_striker=F)
plot_observed_and_prediction(x_axis=x_axis,test_data0=test_data0,fit=fit,rate_per_casualty=T,rate_per_striker=F)

## Ex 4
## plot rate of car/car fatality, by roadtype, per striker distance, comparing striker genders
x_axis <- 'roadtype'
road          <- c("Motorway/A(M)", "A","B, C, Unclassified")
str_gen       <- 0 ## 0 is female, 1 is male
casualty_mode <- 'car/taxi' # c('pedestrian',"cyclist","motorcycle","car/taxi","light goods","bus","heavy goods")
test_data0 <- subset(test_data,year%in%years&cas_male%in%cas_gen&cas_severity%in%severity&cas_mode%in%casualty_mode&
    strike_mode%in%str_mode&strike_age%in%str_ages&cas_age%in%cas_ages&roadtype%in%road&strike_male%in%str_gen)
plot_observed_and_prediction(x_axis=x_axis,test_data0=test_data0,fit=fit,rate_per_casualty=F,rate_per_striker=T)

str_gen       <- 1 ## 0 is female, 1 is male
test_data0 <- subset(test_data,year%in%years&cas_male%in%cas_gen&cas_severity%in%severity&cas_mode%in%casualty_mode&
    strike_mode%in%str_mode&strike_age%in%str_ages&cas_age%in%cas_ages&roadtype%in%road&strike_male%in%str_gen)
plot_observed_and_prediction(x_axis=x_axis,test_data0=test_data0,fit=fit,rate_per_casualty=F,rate_per_striker=T)
#--------------------#

