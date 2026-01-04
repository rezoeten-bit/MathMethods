##############################################################################################################
##############################################################################################################
#Install Package (do only once)
##############################################################################################################
install.packages("apollo")

##############################################################################################################
##############################################################################################################
#Launch Code (do not touch)
##############################################################################################################

### Clear memory
rm(list = ls())
### Load Apollo library
library(apollo)
### Initialise code
apollo_initialise()

##############################################################################################################
##############################################################################################################
#Define Model 
##############################################################################################################

###Here yo can have to give your model a name, a description and identifiers for individuals
###ID are important, when one single individual provided more than an answer.
apollo_control = list(
  modelName  ="MNL1",
  modelDescr ="Simple MNL",
  indivID    ="ID")

###Read your dataset. It is important that the dataset be stored in a variable names database (otherwise the code does not work)
database = read.table("C:/Users/zoete/Documents/ReMa SBSS Y1/dataresidentiallocation.txt",header=TRUE)

###Calculating the number of observations
obs=nrow(database)
obs

###Defining an ID variable (required by the code)
###(in your case ID is not important as each observation is independent and that why
###we are creating this here. In case your dataset indeed contains ID-identifiers you should remove this)
database$ID=c(1:obs)

###Defining an availability variable
###(in your case ID is not important as all alternatives are always available (that'S why we are assigning
###an availability of 1 - otherwise the availability would be zero. In case your dataset indeed contains
###availability information, you should use that (keep in mind that availability is alternative-specific).
database$av=1

##############################################################################################################
##############################################################################################################
#Defining New Variables (not strictly necessary but useful many times) 
##############################################################################################################

###Defining dummy variable male, based on gender
#Note that it is multiplied by 1, just to tell the model that it is a number (1 or 0) and not a boolean (T or F)
database$male = (database$Gender==1)*1

###Defining dummy variable for people older than 70 years
database$old71 = (database$Age==4)*1

###Defining dummy variable for people older than 54 years
database$old55 = (database$Age==3)+(database$Age==4)

###Defining variable equal to the product of low income and price
database$Price1_lowInc = database$Price1*(database$Income==1)
database$Price2_lowInc = database$Price2*(database$Income==1)
database$Price3_lowInc = database$Price3*(database$Income==1)

###Defining variable equal to the natural logarithm of the distance (in R log is ln and never log10)
database$logDistance1=log(database$Distance1)
database$logDistance2=log(database$Distance2)
database$logDistance3=log(database$Distance3)

##############################################################################################################
##############################################################################################################
#Defining Parameters to be estimated
##############################################################################################################

### Vector of parameters, including any that are kept fixed in estimation
### Keep in mind that you can initilize them with values different from 0
apollo_beta=c(asc1   = 0,
              asc2   = 0,
              asc3   = 0,
              b_price= 0,
              b_highinc1 = 0,
              b_highinc2 = 0,
              b_highinc3 = 0)


### Defining that some parameters will not be estimated (hence they will stayed fixed at their initial value)
### For instance, here we are fixing one of the ASCs at zero as required by the model

apollo_fixed = c("asc1","b_highinc1")


##############################################################################################################
##############################################################################################################
#Checking you dataset and your model (do not touch) 
##############################################################################################################

apollo_inputs = apollo_validateInputs()

##############################################################################################################
##############################################################################################################
#Defining you model
##############################################################################################################

##Do not touch this part (and mind the identation - i.e. that spacing - in the following lines)
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  #######################
  ###Here you define the utility functions of the different alternatives
  ######################
    ###Feel free to add more rows if you've got more alternatives
    ###You can use variables from the dataset directly, variable previously created in "Defining New Variables" and
    ###you can also compute variables directly here
    ###When calling variable here, you don'T need to call the database every time (compare with "Defining New Variables")
    ###Remind to incorporate both the explanatory variables as well as the parameters to be estimated
    ###All parameters previously defined mus appear here (but you can basically takes them away but fixing them at zero
  V = list()
  V[['Alt1']]  = asc1 + b_price * Price1 + b_highinc1 * (Income==3)
  V[['Alt2']]  = asc2 + b_price * Price2 + b_highinc2 * (Income==3)
  V[['Alt3']]  = asc3 + b_price * Price3 + b_highinc3 * (Income==3)
  ######################
  
  
  #######################
  ###Define model settings
  #######################
    #Under "choiceVar", you define which variable in you dataset contains the choices
    #Under "alternatives", you associate the utility functions (previously defined), with the numbers in choiceVar
        #Note that you can freely decide on the name of the variable
    #Under "avail" you associate the utility functions with their availability
        #Note that we only use 1s as all alternatives are available for all observations

  mnl_settings = list(
    alternatives  = c(Alt1=1, Alt2=2, Alt3=3), 
    avail         = list(Alt1=1, Alt2=1, Alt3=1), 
    choiceVar     = choice,
    V             = V
  )
  
  #######################
  ###Specification of probabilities 
  #######################
  ###In this case, we use MNL (Multinomial Logit), but other options are possible (e.g. NL, ML, Probit, etc.)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  ### Do not change this part
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

##############################################################################################################
##############################################################################################################
#Model Estimation and Results
##############################################################################################################

#This line estimate the model (do not change)
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
#This line shows the results
apollo_modelOutput(model)
#This lines prints the results in a file (not necessary to print all your tries)
apollo_saveOutput(model)


###Constants-only model

### Vector of parameters only including constants
apollo_beta=c(asc1   = 0,
              asc2   = 0,
              asc3   = 0)

### Fixing asc1 to 0
apollo_fixed = c("asc1")

### Constants-only utility functions in apollo_probabilities
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  
  V = list()
  V[['Alt1']]  = asc1
  V[['Alt2']]  = asc2
  V[['Alt3']]  = asc3

  
  mnl_settings = list(
    alternatives  = c(Alt1=1, Alt2=2, Alt3=3), 
    avail         = list(Alt1=1, Alt2=1, Alt3=1), 
    choiceVar     = choice,
    V             = V
  )
  

  P[['model']] = apollo_mnl(mnl_settings, functionality)

  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Constants-only model estimation
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
#results
apollo_modelOutput(model)
#results in a file
apollo_saveOutput(model)

### Market shares
prop.table(table(database$choice))
