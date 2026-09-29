### Clear memory
rm(list = ls())

#####################################################################################################
# Manuscript: "Assessing Preferences for Reforming Water Subsidy Eligibility Rules in Delhi"
# Corresponding author:Saumitra Sinha
# Email: saumitra@alumni.unc.edu
# Date: 2 July 2026

# Output: Table 9: Latent Class Model with a covariate indicating whether respondents were unsure if they received the subsidy or not. 
#
# Original analysis on:
# ------------------------
# R version: R 4.6.0
# Apollo package version: 0.3.8
#
# Apollo package information on Hess & Palma (2019) DOI 10.1016/j.jocm.2019.100170
# www.ApolloChoiceModelling.com

########### Change the name of the files (model, export, etc, based on name below) #####

file_name <- "SubsidyEligibility_07_LCM_DK"

### Install packages if required
# install.packages("apollo")
# install.packages("openxlsx")

# install.packages("dplyr")
# 
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("ggtext")
# install.packages("tidyr")
# install.packages("scales")
# install.packages("ggrepel")
# install.packages("ggh4x")
### Load libraries
library(apollo)
library(openxlsx)
library(dplyr)

library(stringr)
library(ggplot2)
library(ggtext)
library(tidyr)
library(scales)
library(ggrepel)
library(ggh4x)

##########################################################
########## Importing data ################################
##########################################################

# Gets current directory
current_path = rstudioapi::getActiveDocumentContext()$path
print( getwd() )
setwd(dirname(current_path))


# # Read Dataset
database = read.csv("../Data/SubsidyEligibility.csv")

########## END: Importing data ###########################
##########################################################


##########################################################
########## Modifying variables for analysis ##############
##########################################################

# First wealth quintile
database$FirstQuin = ifelse(database$Wealth_Quin == 1, 1, 0)

# Household size > = 4
database$HH_4_OrMore_Y = ifelse(database$hh_size >=4, 1, 0)

# Caste
database$Marginalized_Caste_Y = ifelse(database$hh_caste == "Yes", 1, 0)
database$Marginalized_Caste_Miss = ifelse(database$hh_caste == "Missing", 1, 0)

# Differently abled members (Yes = 35)
database$DiffAbled_Y = ifelse(database$hh_diff_abled == "Yes", 1, 0)
database$DiffAbled_Miss = ifelse(database$hh_diff_abled == "Missing", 1, 0)

# Elderly members
database$Elderly_Y = ifelse(database$hh_size_60 >0, 1, 0)

# Getting subsidy
database$Benefit_Subsidy_Y = ifelse(database$Got_Subsidy == "Yes", 1, 0)
database$Benefit_Subsidy_DK = ifelse(database$Got_Subsidy == "DK", 1, 0)


########## END: Modifying variables for analysis ##############
###############################################################


#################################################################
#### Analysis using Apollo package for Modeling #################
#################################################################

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = file_name,
  modelDescr      = file_name,
  indivID         = "ID",
  weights         = "Weights",
  analyticGrad    = FALSE,
  workInLogs      = TRUE,
  outputDirectory = "../Output_Apollo"
)



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

# Vector of parameters, including any that are kept fixed in estimation

apollo_beta = c(

                ###################### CLASS C_1 (One) #######################################    
                b_Income_C_1              = 0.85903, 
                b_HH_Member_C_1           = 0.08217, 
                b_Caste_C_1               = -2.82649, 
                b_Elderly_DiffAble_C_1    = 0.21167, 
                b_NotPlanned_C_1          = 0.99187,
                b_10k_All_C_1             = 1.22662,
                b_CurrentPolicy_C_1       = 0, #Fixed
                
                mu_worst_C_1                  = 0.39944,
                
                # # # ----- Version: Fairness ----- ----- ----- ----- #
                # # gamma_Ver_Fair_C_1            = 0, #Fixed
                # # 
                # # # # ----- SQ: Position First ----- ----- ----- ----- #
                # # gamma_SQ_C_1           = 0, #Fixed

                # ----- Wealth indicators ----- ----- ----- ----- #
                # gamma_WI_FirstTwo_C_1         = 0, #Fixed
                gamma_WI_Q1_C_1               = 0, #Fixed

                # # ----- Household size -----    
                gamma_HH_4_OrMore_Y_C_1       = 0, #Fixed
                # 
                # # ----- Marginalized caste ----- ----- -----  ----- #
                # gamma_Caste_Y_C_1             = 0, #Fixed
                # # gamma_Caste_Miss_C_1          = 0, #Fixed
                # 
                # # ----- Elderly or differetnly abled  ----- ----- ----- #
                # gamma_Elderly_Y_C_1           = 0, #Fixed
                # 
                # # ----- Informal Settlement ----- ----- #
                gamma_Informal_C_1            = 0, #Fixed

                # # -----  Ownership ----- ----- #
                # gamma_Sharing_C_1             = 0,
                # gamma_Ownership_C_1           = 0, #Fixed
                # 
                # # ----- Benefiting from the subsidy ----- ----- ----- #
                gamma_Benefit_Subsidy_Y_C_1   = 0, #Fixed
                gamma_Benefit_Subsidy_DK_C_1  = 0, #Fixed
                
                delta_C_1                     = 0, #Fixed
                

                ###################### END: CLASS C_1 ########################################
                
                ###################### CLASS C_2 (Two) #######################################    
                b_Income_C_2              = -1.18413, 
                b_HH_Member_C_2           = -3.91778, 
                b_Caste_C_2               = -5.93506, 
                b_Elderly_DiffAble_C_2    = -4.61478, 
                b_NotPlanned_C_2          = -2.04222,
                b_10k_All_C_2             = -3.01522,
                b_CurrentPolicy_C_2       = 0, #Fixed
                
                mu_worst_C_2                  =  0.59315,  

                # # # ----- Version: Fairness ----- ----- ----- ----- #
                # # gamma_Ver_Fair_C_2            = 0.1, #Fixed
                # # 
                # # # # ----- SQ: Position First ----- ----- ----- ----- #
                # # gamma_SQ_C_2                  = 0.1, #Fixed
                # 
                # ----- Wealth indicators ----- ----- ----- ----- #
                # gamma_WI_FirstTwo_C_2         = 1.3449,
                gamma_WI_Q1_C_2               = 0.96790,
                # 
                # # ----- Household size ----- ----- ----- ----- #   
                gamma_HH_4_OrMore_Y_C_2       = 0.62279,
                # 
                # # ----- Marginalized caste ----- ----- -----  ----- #
                # gamma_Caste_Y_C_2             = 0.1,
                # # gamma_Caste_Miss_C_2          = 0.1,
                # 
                # # ----- Elderly or differetnly abled  ----- ----- ----- #  
                # gamma_Elderly_Y_C_2           = 0.1,
                # 
                # # ----- Informal Settlement ----- ----- #
                gamma_Informal_C_2            = 2.28962,
                # 
                # # ----- Ownership ----- ----- #
                # gamma_Sharing_C_2             = 0.1,
                # gamma_Ownership_C_2           = 0.62460,
                # 
                # # ----- Benefiting from the subsidy ----- ----- ----- #
                gamma_Benefit_Subsidy_Y_C_2   = -0.03484,
                gamma_Benefit_Subsidy_DK_C_2  = -0.1,
                
                delta_C_2                     = -3.43457,

                ###################### END: CLASS C_2 ########################################
                
                ###################### CLASS C_3 (Three) #######################################    
                b_Income_C_3              = -0.98739, 
                b_HH_Member_C_3           = -2.38821, 
                b_Caste_C_3               = -5.56696, 
                b_Elderly_DiffAble_C_3    = -1.57143, 
                b_NotPlanned_C_3          = -3.43537,
                b_10k_All_C_3             = -3.21690,
                b_CurrentPolicy_C_3       = 0, #Fixed
                
                mu_worst_C_3                  =  0.84059,
                
                # # # ----- Version: Fairness ----- ----- ----- ----- #
                # # gamma_Ver_Fair_C_3            = -0.1, #Fixed
                # # 
                # # # # ----- SQ: Position First ----- ----- ----- ----- #
                # # gamma_SQ_C_3                  = -0.1, #Fixed
                # 
                # # ----- Wealth indicators ----- ----- ----- ----- #
                # gamma_WI_FirstTwo_C_3         = 4.3142,
                gamma_WI_Q1_C_3               = 1.71258,
                # 
                # # ----- Household size ----- ----- ----- ----- #   
                gamma_HH_4_OrMore_Y_C_3       = 4.55203,
                # 
                # # ----- Marginalized caste ----- ----- -----  ----- #
                # gamma_Caste_Y_C_3             = 4.3121,
                # # gamma_Caste_Miss_C_3          = -0.1,
                # 
                # # ----- Elderly or differetnly abled  ----- ----- ----- #  
                # gamma_Elderly_Y_C_3           = -0.1,
                # 
                # # ----- Informal Settlement ----- ----- #
                gamma_Informal_C_3            = -5.20882,

                # # ----- Ownership ----- ----- #
                # gamma_Sharing_C_3             = -0.1,
                # gamma_Ownership_C_3           = -2.67388,
                # 
                # # ----- Benefiting from the subsidy ----- ----- ----- #
                gamma_Benefit_Subsidy_Y_C_3   = 3.80303,
                gamma_Benefit_Subsidy_DK_C_3  = 0.1,
                
                delta_C_3                     =  -5.64276
                ###################### END: CLASS C_3 ########################################
                )


### Fixed Parameters During Estimation
apollo_fixed = c("b_CurrentPolicy_C_1", 
                 "b_CurrentPolicy_C_2", 
                 "b_CurrentPolicy_C_3",
                 
                 "delta_C_1",
                 
                 # # ----- Version: Fairness ----- ----- ----- ----- #
                 # "gamma_Ver_Fair_C_1",
                 #
                 # # # ----- SQ: Position First ----- ----- ----- ----- #
                 # "gamma_SQ_C_1",

                 # # ----- Wealth indicators ----- ----- ----- ----- #
                 # "gamma_WI_FirstTwo_C_1",
                 "gamma_WI_Q1_C_1",
                 # 
                 # # ----- Household size ---- ----- ----- ----- #   
                 "gamma_HH_4_OrMore_Y_C_1",
                 # 
                 # # ----- Marginalized caste ----- ----- -----  ----- #
                 # "gamma_Caste_Y_C_1",
                 # # "gamma_Caste_Miss_C_1",
                 
                 # # ----- Elderly or differetnly abled  ----- ----- ----- #  
                 # "gamma_Elderly_Y_C_1",
                 # 
                 # # ----- Informal Settlement ----- ----- #
                 "gamma_Informal_C_1",

                 # # ----- Ownership ----- ----- #
                 # "gamma_Sharing_C_1",
                 # "gamma_Ownership_C_1",
                 # 
                 # # ----- Benefiting from the subsidy ----- ----- ----- #
                 "gamma_Benefit_Subsidy_Y_C_1",
                 "gamma_Benefit_Subsidy_DK_C_1"#,
                 
                 )

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  lcpars[["b_Income"]]           = list(b_Income_C_1          , b_Income_C_2          , b_Income_C_3)
  lcpars[["b_HH_Member"]]        = list(b_HH_Member_C_1       , b_HH_Member_C_2       , b_HH_Member_C_3)
  lcpars[["b_Caste"]]            = list(b_Caste_C_1           , b_Caste_C_2           , b_Caste_C_3)
  lcpars[["b_Elderly_DiffAble"]] = list(b_Elderly_DiffAble_C_1, b_Elderly_DiffAble_C_2, b_Elderly_DiffAble_C_3)
  lcpars[["b_NotPlanned"]]       = list(b_NotPlanned_C_1      , b_NotPlanned_C_2      , b_NotPlanned_C_3)
  lcpars[["b_10k_All"]]          = list(b_10k_All_C_1         , b_10k_All_C_2         , b_10k_All_C_3)
  lcpars[["b_CurrentPolicy"]]    = list(b_CurrentPolicy_C_1   , b_CurrentPolicy_C_2   , b_CurrentPolicy_C_3)
  lcpars[["mu_worst"]]           = list(mu_worst_C_1          , mu_worst_C_2          , mu_worst_C_3)
  
  V=list()
  V[["Class_1"]] = delta_C_1 + 
    
                    # # # ----- Version: Fairness ----- ----- ----- ----- #
                    # # gamma_Ver_Fair_C_1            *(BWS_1_Version == 1) +
                    # # 
                    # # # ----- SQ: Position First ----- ----- ----- ----- #
                    # # gamma_SQ_C_1                 *(BWS_1_SQ ==1) +

                    # # ----- Wealth indicators ----- ----- ----- ----- #
                    # gamma_WI_FirstTwo_C_1         *(WI== 1) +
                    gamma_WI_Q1_C_1               *(FirstQuin== 1) +
                    # 
                    # # ----- Household size --------- ----- ----- ----- #    
                    gamma_HH_4_OrMore_Y_C_1       *(HH_4_OrMore_Y== 1) +
                    # 
                    # # ----- Marginalized caste ----- ----- -----  ----- #  
                    # gamma_Caste_Y_C_1             *(Marginalized_Caste_Y== 1) +
                    # # gamma_Caste_Miss_C_1          *(Marginalized_Caste_Miss== 1) +
                    # 
                    # # ----- Elderly or differetnly abled  ----- ----- ----- #  
                    # gamma_Elderly_Y_C_1           *(Elderly_Y== 1) +
                    # 
                    # # ----- Informal Settlement ----- ----- #
                    gamma_Informal_C_1            *(Informal_Settlement==1)+

                    # # ----- Owmership ----- ----- #
                    # gamma_Sharing_C_1              *(sharing=="Yes")+
                    # gamma_Ownership_C_1           *(ownership=="Owned")+
                    # 
                    # # ----- Benefiting from the subsidy ----- ----- ----- #
                    gamma_Benefit_Subsidy_Y_C_1   *(Benefit_Subsidy_Y== 1) +
                    gamma_Benefit_Subsidy_DK_C_1  *(Benefit_Subsidy_DK== 1)

  V[["Class_2"]] = delta_C_2 + 
    
                    # # # ----- Version: Fairness ----- ----- ----- ----- #
                    # # gamma_Ver_Fair_C_2            *(BWS_1_Version == 1) +
                    # # 
                    # # # ----- SQ: Position First ----- ----- ----- ----- #
                    # # gamma_SQ_C_2                 *(BWS_1_SQ ==1) +

                    # # ----- Wealth indicators ----- ----- ----- ----- #
                    # gamma_WI_FirstTwo_C_2         *(WI== 1) +
                    gamma_WI_Q1_C_2               *(FirstQuin== 1) +
                    # 
                    # # ----- Household size --------- ----- ----- ----- #    
                    gamma_HH_4_OrMore_Y_C_2       *(HH_4_OrMore_Y== 1) +
                    # 
                    # # ----- Marginalized caste ----- ----- -----  ----- #  
                    # gamma_Caste_Y_C_2             *(Marginalized_Caste_Y== 1) +
                    # # gamma_Caste_Miss_C_2          *(Marginalized_Caste_Miss== 1) +
                    # 
                    # # ----- Elderly or differetnly abled  ----- ----- ----- #  
                    # gamma_Elderly_Y_C_2           *(Elderly_Y== 1) +
                    # 
                    # # ----- Informal Settlement ----- ----- #
                    gamma_Informal_C_2            *(Informal_Settlement==1)+

                    # # ----- Owmership ----- ----- #
                    # gamma_Sharing_C_2              *(sharing=="Yes")+
                    # gamma_Ownership_C_2           *(ownership=="Owned")+
                    # 
                    # # ----- Benefiting from the subsidy ----- ----- ----- #
                    gamma_Benefit_Subsidy_Y_C_2   *(Benefit_Subsidy_Y== 1) +
                    gamma_Benefit_Subsidy_DK_C_2  *(Benefit_Subsidy_DK== 1)
  
  
  V[["Class_3"]] = delta_C_3 + 
    
                  #   # # ----- Version: Fairness ----- ----- ----- ----- #
                  #   # gamma_Ver_Fair_C_3            *(BWS_1_Version == 1) +
                  #   # 
                  #   # # ----- SQ: Position First ----- ----- ----- ----- #
                  #   # gamma_SQ_C_3                 *(BWS_1_SQ ==1) +

                  #   # ----- Wealth indicators ----- ----- ----- ----- #
                    # gamma_WI_FirstTwo_C_3         *(WI== 1) +
                    gamma_WI_Q1_C_3               *(FirstQuin== 1) +
                  #   
                  #   # ----- Household size --------- ----- ----- ----- #    
                    gamma_HH_4_OrMore_Y_C_3       *(HH_4_OrMore_Y== 1) +
                  #   
                  #   # ----- Marginalized caste ----- ----- -----  ----- #  
                    # gamma_Caste_Y_C_3             *(Marginalized_Caste_Y== 1) +
                  #   # gamma_Caste_Miss_C_3          *(Marginalized_Caste_Miss== 1) +
                  #   
                  #   # ----- Elderly or differetnly abled  ----- ----- ----- #  
                    # gamma_Elderly_Y_C_3           *(Elderly_Y== 1) +
                  #   
                  #   # ----- Informal Settlement ----- ----- #
                    gamma_Informal_C_3            *(Informal_Settlement==1)+

                  #   # ----- Owmership ----- ----- #
                    # gamma_Sharing_C_3              *(sharing=="Yes")+
                    # gamma_Ownership_C_3           *(ownership=="Owned")+
                  #   
                  #   # ----- Benefiting from the subsidy ----- ----- ----- #
                    gamma_Benefit_Subsidy_Y_C_3   *(Benefit_Subsidy_Y== 1) +
                    gamma_Benefit_Subsidy_DK_C_3  *(Benefit_Subsidy_DK== 1)

 
  classAlloc_settings = list(
    classes      = c(Class_1=1, Class_2=2, Class_3=3), 
    utilities    = V
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

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

  ### Create list of probabilities P
  P = list()
  
  #Loop over classes
  
  for(s in 1:3){
    
  P_LCM = list() # So that I can combine the BWBW for each class
  
  ### List of utilities:
  V = list()
  V[["Income"]]           = b_Income[[s]]
  
  V[["HH_Members"]]       = b_HH_Member[[s]]
  
  V[["Caste_Based"]]      = b_Caste[[s]]
  
  V[["Elderly_DiffAble"]] = b_Elderly_DiffAble[[s]]
  
  V[["Not_in_Planned"]]   = b_NotPlanned[[s]]
  
  V[["Universal"]]        = b_10k_All[[s]]
  
  V[["Keep_Current"]]     = b_CurrentPolicy[[s]]  

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
    componentName = paste0("Class_",s, "_best")
  )
  P_LCM[["choice_best"]] = apollo_mnl(mnl_settings_best, functionality)
  
  
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
    utilities     = list(Income            = -mu_worst[[s]]*V[["Income"]],
                         HH_Members        = -mu_worst[[s]]*V[["HH_Members"]],
                         Caste_Based       = -mu_worst[[s]]*V[["Caste_Based"]],
                         Elderly_DiffAble  = -mu_worst[[s]]*V[["Elderly_DiffAble"]],
                         Not_in_Planned    = -mu_worst[[s]]*V[["Not_in_Planned"]],
                         Universal         = -mu_worst[[s]]*V[["Universal"]],
                         Keep_Current      = -mu_worst[[s]]*V[["Keep_Current"]]),
    componentName = paste0("Class_",s, "_worst") 
  )
  P_LCM[["choice_worst"]] = apollo_mnl(mnl_settings_worst, functionality)
  
  
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
    componentName = paste0("Class_",s, "_second_best")
  )
  P_LCM[["choice_best_2"]] = apollo_mnl(mnl_settings_best_2, functionality)
  
  
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
    utilities     = list(Income            = -mu_worst[[s]]*V[["Income"]],
                         HH_Members        = -mu_worst[[s]]*V[["HH_Members"]],
                         Caste_Based       = -mu_worst[[s]]*V[["Caste_Based"]],
                         Elderly_DiffAble  = -mu_worst[[s]]*V[["Elderly_DiffAble"]],
                         Not_in_Planned    = -mu_worst[[s]]*V[["Not_in_Planned"]],
                         Universal         = -mu_worst[[s]]*V[["Universal"]],
                         Keep_Current      = -mu_worst[[s]]*V[["Keep_Current"]]),
    componentName = paste0("Class_",s, "_second worst")
  )
  P_LCM[["choice_worst_2"]] = apollo_mnl(mnl_settings_worst_2, functionality)

    
  ### Combined model
  P[[paste0("Class_",s)]] = apollo_combineModels(P_LCM, apollo_inputs, functionality, asList = FALSE)
  
  }
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
    # ### Including individual weights
  P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


################################################################ #
## MODEL ESTIMATION                                            ####
################################################################ #
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings= list(maxIterations = 300,
                                                                                                                estimationRoutine = "BFGS"))
apollo_modelOutput(model)
apollo_saveOutput(model)

# model = apollo_loadModel(file_name)
# apollo_modelOutput(model)


#################################################################
#### Posterior probabilities ############
#################################################################

## NOTE: Unweighted
## Posterior porbabilities of beloinging to each class for each individual
posteriors <- apollo_lcConditionals(model, apollo_probabilities, apollo_inputs)

### Join the posterior probabs to the dataset with blocks
database_prob <- merge(posteriors, database, by = "ID")

latent_classes <- setdiff(colnames(posteriors), "ID")

######################################################################################
# Formatting Latent Class table with SE and significance. Have p-values separate
######################################################################################
table_input <-  read.csv(paste0("../Output_Apollo/",file_name,"_estimates.csv"))

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


n = length(latent_classes) #### NUMBER OF CLASSES
LC <- list()
table_LC <- table_input
var_names <- c(
  "Income",
  "Household size",
  "Caste",
  "Elderly or differently abled members",
  "Geographical (to informal settlements)",
  "10,000 L/month unconditional subsidy",
  "Current subsidy policy (SQ)",
  
  "Worst scaling",
  
  # "Version: Fairness",
  
  "Lower wealth (Q1)",
  
  "Four or more household members",
  
  # "Marginalized caste",
  # "Has elderly household members",
  
  "Informal settlement",

  
  "Received the subsidy",
  "Received the subsidy (DK)",
  
  "Delta"
)


for (s in 1:n){
  
  table_input <- subset(table_LC, str_sub_all(table_LC$Variable, -3, -1) == paste0("C_",s))
  table_input$Variable_2 <- table_input$Variable
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
  
  LC[[s]] <- table_formatted
  
}

table_formatted <- bind_cols(LC)

## Dros the useless columns based on numb of classes
n = length(latent_classes) #### ENTER NUMBER OF CLASSES
cols_to_drop <- seq(from = 3, by = 2, length.out = n - 1)
table_formatted <- table_formatted[, -cols_to_drop]

### Rename the columns to clean them up
names(table_formatted) <- c("Variable", paste0("Class ", 1:n))


s= ncol(table_formatted) # Number of columns in the table
# The reference level subsidy = 0, not NA
r = which(table_formatted$Variable == "Current subsidy policy (SQ)")
table_formatted[r,2:s] <- 0


# ## Loop to include class membership
# table_formatted[(nrow(table_formatted) + 1), "Variable"] <- "Probability of Class Membership"
# current_row <- nrow(table_formatted)

n = length(latent_classes) #### NUMBER OF CLASSES
l <- nrow(table_formatted)+1

for (i in 1:n) {
  # Extract probability (Class 1 is at index 4, Class 2 at index 5, etc.)
  class_prob <- substr(model[["componentReport"]][["model"]][["param"]][[3 + i]], 14, 18)
  
  # Assign to the corresponding Class column dynamically
  column_name <- paste0("Class ", i)
  table_formatted[l, column_name] <- class_prob
}


# LL of the model
table_formatted[(nrow(table_formatted)+1), "Variable"] = "LL"
table_formatted[(nrow(table_formatted)), "Class 1"] = round(model$LLout[1], 2)

# AIsof the model
table_formatted[(nrow(table_formatted)+1), "Variable"] = "AIC"
table_formatted[(nrow(table_formatted)), "Class 1"] = round(model$AIC[1], 2)

# BIC of the model
table_formatted[(nrow(table_formatted)+1), "Variable"] = "BIC"
table_formatted[(nrow(table_formatted)), "Class 1"] = round(model$BIC[1], 2)

##### Writing Formatted Table ###########
write.xlsx(table_formatted, paste0("../Output_Tables_Fig_Manuscript/", file_name,"_Extended_Table_9.xlsx"))

######################################################################################
# END: Formatting Latent Class table with SE and significance. Have p-values separate
######################################################################################