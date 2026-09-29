### Clear memory
rm(list = ls())
#####################################################################################################
# Manuscript: "Assessing Preferences for Reforming Water Subsidy Eligibility Rules in Delhi"
# Corresponding author:Saumitra Sinha
# Email: saumitra@alumni.unc.edu
#
# Output: Extended Tables and Figures Table, MNL Interacted with Indicator for Status Quo Position
#
# Original analysis on:
# ------------------------
# R version: R 4.3.2
# Apollo package version: 0.3.2
#
# Tested on:
#-------------------------
# R version: R 4.6
# Apollo package versions: 0.3.2 to 0.3.8
#
# Apollo package information on Hess & Palma (2019) DOI 10.1016/j.jocm.2019.100170
# www.ApolloChoiceModelling.com

##########################################################
file_name <- "SubsidyEligibility_03_MNL_SQ"

### Install packages if required
# install.packages("apollo")
# install.packages("openxlsx")

### Load libraries
library(apollo)
library(openxlsx)

##########################################################
########## Importing data ################################
##########################################################

# Gets current directory
current_path = rstudioapi::getActiveDocumentContext()$path 
print( getwd() )
setwd(dirname(current_path))

# Read Dataset
database = read.csv("../Data/SubsidyEligibility.csv")
########## END: Importing data ###########################
##########################################################


#################################################################
#### Analysis using Apollo package for Modeling #################
#################################################################

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = file_name,
  modelDescr      = "SubsidyEligibility_MNL_SQ",
  indivID         = "ID",
  weights         = "Weights",
  analyticGrad    = FALSE,
  workInLogs      = TRUE,
  outputDirectory = "../Output_Apollo"
)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters
apollo_beta = c(
  
                b_Income              = 0, 
                b_HH_Member           = 0, 
                b_Caste               = 0, 
                b_Elderly_DiffAble    = 0, 
                b_NotPlanned          = 0,
                b_10k_All             = 0,
                b_CurrentPolicy       = 0,
                
                mu_worst              = 1,
                
                b_Income_SQ              = 0,
                b_HH_Member_SQ           = 0,
                b_Caste_SQ               = 0,
                b_Elderly_DiffAble_SQ    = 0,
                b_NotPlanned_SQ          = 0,
                b_10k_All_SQ             = 0
                
                )
                

apollo_fixed = c("b_CurrentPolicy")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### list of probabilities P
  P = list()
  
  ### List of utilities:
  V = list()
  V[["Income"]]           = b_Income           + b_Income_SQ*(BWS_1_SQ ==1)      
  
  V[["HH_Members"]]       = b_HH_Member        + b_HH_Member_SQ*(BWS_1_SQ ==1)      
  
  V[["Caste_Based"]]      = b_Caste            + b_Caste_SQ*(BWS_1_SQ ==1)           
  
  V[["Elderly_DiffAble"]] = b_Elderly_DiffAble + b_Elderly_DiffAble_SQ*(BWS_1_SQ ==1) 
  
  V[["Not_in_Planned"]]   = b_NotPlanned       + b_NotPlanned_SQ*(BWS_1_SQ ==1)       
  
  V[["Universal"]]        = b_10k_All          + b_10k_All_SQ*(BWS_1_SQ ==1)         
  
  V[["Keep_Current"]]     = b_CurrentPolicy

  ### FIRST BEST: Compute probabilities for "best" choice using MNL model
  mnl_settings_best = list(
    alternatives  = c(Income           =1, 
                      HH_Members       =2, 
                      Caste_Based      =3, 
                      Elderly_DiffAble =4, 
                      Not_in_Planned   =5,
                      Universal        =6, 
                      Keep_Current     =7),
    
    avail         = list(Income             =(Inc==1), 
                         HH_Members         =(HH==1), 
                         Caste_Based        =(Caste==1), 
                         Elderly_DiffAble   =(ElderDiffAble==1), 
                         Not_in_Planned     =(NotPlanned==1),
                         Universal          =(First10k==1), 
                         Keep_Current       =(CurrentPolicy==1) ),
    
    choiceVar     = BWS_1_F_Most,
    utilities     = V,
    componentName = "best"
  )
  P[["choice_best"]] = apollo_mnl(mnl_settings_best, functionality)

  
  ### FIRST WORST: Compute probabilities for "worst" choice using MNL model
  mnl_settings_worst = list(
    alternatives  = c(Income           =1, 
                      HH_Members       =2, 
                      Caste_Based      =3, 
                      Elderly_DiffAble =4, 
                      Not_in_Planned   =5,
                      Universal        =6, 
                      Keep_Current     =7),
    
    avail         = list(Income            =(Inc==1           & BWS_1_F_Most!=1), 
                         HH_Members        =(HH==1            & BWS_1_F_Most!=2),
                         Caste_Based       =(Caste==1         & BWS_1_F_Most!=3),                                            
                         Elderly_DiffAble  =(ElderDiffAble==1 & BWS_1_F_Most!=4),                         
                         Not_in_Planned    =(NotPlanned==1    & BWS_1_F_Most!=5),
                         Universal         =(First10k==1      & BWS_1_F_Most!=6),                                             
                         Keep_Current      =(CurrentPolicy==1 & BWS_1_F_Most!=7)),                        
                                    
    choiceVar     = BWS_1_F_Least, 
    utilities     = list(Income            = -mu_worst*V[["Income"]],
                         HH_Members        = -mu_worst*V[["HH_Members"]],
                         Caste_Based       = -mu_worst*V[["Caste_Based"]],
                         Elderly_DiffAble  = -mu_worst*V[["Elderly_DiffAble"]],
                         Not_in_Planned    = -mu_worst*V[["Not_in_Planned"]],
                         Universal         = -mu_worst*V[["Universal"]],
                         Keep_Current      = -mu_worst*V[["Keep_Current"]]),
    componentName = "worst" 
  )
  P[["choice_worst"]] = apollo_mnl(mnl_settings_worst, functionality)
  
  
  ### SECOND BEST: Compute probabilities for "Second Best" choice using MNL model
  mnl_settings_best_2 = list(
    alternatives  = c(Income           =1, 
                      HH_Members       =2, 
                      Caste_Based      =3, 
                      Elderly_DiffAble =4, 
                      Not_in_Planned   =5,
                      Universal        =6, 
                      Keep_Current     =7),
    
    avail         = list(Income            =(Inc==1           & BWS_1_F_Most!=1 & BWS_1_F_Least!=1), 
                         HH_Members        =(HH==1            & BWS_1_F_Most!=2 & BWS_1_F_Least!=2),
                         Caste_Based       =(Caste==1         & BWS_1_F_Most!=3 & BWS_1_F_Least!=3),                                            
                         Elderly_DiffAble  =(ElderDiffAble==1 & BWS_1_F_Most!=4 & BWS_1_F_Least!=4),                         
                         Not_in_Planned    =(NotPlanned==1    & BWS_1_F_Most!=5 & BWS_1_F_Least!=5),
                         Universal         =(First10k==1      & BWS_1_F_Most!=6 & BWS_1_F_Least!=6),                                             
                         Keep_Current      =(CurrentPolicy==1 & BWS_1_F_Most!=7 & BWS_1_F_Least!=7)),  
    
    choiceVar     = BWS_1_S_Most, 
    utilities     = V,
    componentName = "second_best"
  )
  P[["choice_best_2"]] = apollo_mnl(mnl_settings_best_2, functionality)
  
  
  ### SECOND WORST: Compute probabilities for "Second Worst" choice using MNL model
  mnl_settings_worst_2 = list(
    alternatives  = c(Income           =1, 
                      HH_Members       =2, 
                      Caste_Based      =3, 
                      Elderly_DiffAble =4, 
                      Not_in_Planned   =5,
                      Universal        =6, 
                      Keep_Current     =7),
    
    avail         = list(Income            =(Inc==1           & BWS_1_F_Most!=1 & BWS_1_F_Least!=1 & BWS_1_S_Most!=1), 
                         HH_Members        =(HH==1            & BWS_1_F_Most!=2 & BWS_1_F_Least!=2 & BWS_1_S_Most!=2),
                         Caste_Based       =(Caste==1         & BWS_1_F_Most!=3 & BWS_1_F_Least!=3 & BWS_1_S_Most!=3),                                            
                         Elderly_DiffAble  =(ElderDiffAble==1 & BWS_1_F_Most!=4 & BWS_1_F_Least!=4 & BWS_1_S_Most!=4),                         
                         Not_in_Planned    =(NotPlanned==1    & BWS_1_F_Most!=5 & BWS_1_F_Least!=5 & BWS_1_S_Most!=5),
                         Universal         =(First10k==1      & BWS_1_F_Most!=6 & BWS_1_F_Least!=6 & BWS_1_S_Most!=6),                                             
                         Keep_Current      =(CurrentPolicy==1 & BWS_1_F_Most!=7 & BWS_1_F_Least!=7 & BWS_1_S_Most!=7)),  
    
    choiceVar     = BWS_1_S_Least, 
    utilities     = list(Income            = -mu_worst*V[["Income"]],
                         HH_Members        = -mu_worst*V[["HH_Members"]],
                         Caste_Based       = -mu_worst*V[["Caste_Based"]],
                         Elderly_DiffAble  = -mu_worst*V[["Elderly_DiffAble"]],
                         Not_in_Planned    = -mu_worst*V[["Not_in_Planned"]],
                         Universal         = -mu_worst*V[["Universal"]],
                         Keep_Current      = -mu_worst*V[["Keep_Current"]]),
    componentName = "second worst"
  )
  P[["choice_worst_2"]] = apollo_mnl(mnl_settings_worst_2, functionality)
  
  
  ### Combined model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Including individual weights
  P = apollo_weighting(P, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings= list(maxIterations = 300,
                                                                                                                estimationRoutine = "BFGS"))
apollo_modelOutput(model)
apollo_saveOutput(model)

# model = apollo_loadModel("SubsidyEligibility_MNL_SQ")
# apollo_modelOutput(model)
# 
# 
# 
#################################################################
#### Formatting Table ######################################
#################################################################
table_input <-  read.csv(paste0("../Output_Apollo/",file_name,"_estimates.csv"))

# Formating table with SE and significance. Have p-values separate

table_input <- table_input[,-c(3,4)]
names(table_input) <- c("Variable", "Estimate", "Robust_SE", "Robust_T_Ratio")


table_input$p_value <- 2*pnorm(abs(table_input$Robust_T_Ratio), lower.tail=FALSE)

for(n in 2:4){
  table_input[,n] <- sprintf("%.3f", as.numeric(table_input[,n]))
}

table_input$Coefficient <- ifelse(table_input$p_value < 0.01, 
                                  paste0(table_input$Estimate, "***"),
                                  ifelse(table_input$p_value < 0.05 & table_input$p_value >= 0.01, 
                                         paste0(table_input$Estimate, "**"),
                                         ifelse(table_input$p_value < 0.1 & table_input$p_value >= 0.05,
                                                paste0(table_input$Estimate, "*"), 
                                                paste0(table_input$Estimate))))

table_input$SE <- ifelse(is.na(table_input$Robust_SE), 
                         table_input$Robust_SE, 
                         paste0("(", table_input$Robust_SE, ")") )


# 1) For tables with no classes
var_names <- c(
  "Income",
  "Household size",
  "Caste",
  "Elderly or differently abled members",
  "Geographical (to informal settlements)",
  "10,000 L/month unconditional subsidy",
  "Current subsidy policy (SQ)",
  
  "Variable for worst scaling",
  
  
  "Income * (SQ: First Position)",
  "Household size * (SQ: First Position)",
  "Caste * (SQ: First Position)",
  "Elderly or differently abled members * (SQ: First Position)",
  "Geographical (to informal settlements) * (SQ: First Position)",
  "10,000 L/month unconditional subsidy * (SQ: First Position"
  
  
)
table_input$Variable <- var_names

table_formatted <- data.frame(matrix(NA, nrow = 2*nrow(table_input), ncol = 2))
names(table_formatted) <- c("Variable", "Coefficient")

j <-1
for(i in 1:nrow(table_input)){
  # print(j)
  table_formatted[j, "Variable"] <- table_input[i,"Variable"]
  table_formatted[j, "Coefficient"] <- table_input[i,"Coefficient"]
  j <- j+1
  # print(j)
  table_formatted[j,1] <- " "
  table_formatted[j, "Coefficient"] <- table_input[i,"SE"]
  j <- j+1
  # print(j)
}

table_formatted[,] <- lapply(table_formatted[,], function(x) ifelse(is.na(x), "NA", x))
table_formatted[13, "Coefficient"] <- 0

# LL of the model
table_formatted[(nrow(table_formatted)+1), "Variable"] = "LL (model)"
table_formatted[(nrow(table_formatted)), "Coefficient"] = round(model$LLout[1], 2)

# AIsof the model
table_formatted[(nrow(table_formatted)+1), "Variable"] = "AIC"
table_formatted[(nrow(table_formatted)), "Coefficient"] = round(model$AIC[1], 2)

# BIC of the model
table_formatted[(nrow(table_formatted)+1), "Variable"] = "BIC"
table_formatted[(nrow(table_formatted)), "Coefficient"] = round(model$BIC[1], 2)


##### Writing Formatted Table ###########
write.xlsx(table_formatted, paste0("../Output_Tables_Fig_Manuscript/", file_name,"_Extended_Table_4.xlsx"))

#################################################################
#### END: Formatting Table ######################################
#################################################################


