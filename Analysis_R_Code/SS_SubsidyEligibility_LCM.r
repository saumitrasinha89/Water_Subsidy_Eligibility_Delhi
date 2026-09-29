### Clear memory
rm(list = ls())

#####################################################################################################
# Manuscript: "Assessing Public Preferences for Reforming Water Subsidy Eligibility Rules in Delhi"
# Authors: Saumitra Sinha, Richard T. Carson, Joseph Cook, Dale Whittington

# Corresponding author:Saumitra Sinha
# Email: saumitra@alumni.unc.edu
# Date: 28 September 2026

# Output: Table 3, results of the Latent Class Model (LCM) in the main manuscript
# NOTE: 
# This is the model which used the values obtained from a start search performed on a university server.
# The code is written so that the model results are directly loaded rather than run the full start search.
# If you would like to run the start search, please do the following:
#   1) Remove the "#" from the code under the section "MODEL ESTIMATION"
#   2) Delete the lines in the section "LOADING MODEL OUTPUT FROM START SEARCH" or turn them into comments by putting "#" at the begining of each line
#   3) Remove the "#" from the code under the section "Start Search"
#   4) Remove the "#" from the code under the section "Final Model with the best start values from start search"
#   5) Run the code
#   6) You can copy the original outputs and .RDS files so that they are not overwritten.



# Original analysis on:
# ------------------------
# Apollo 0.3.8 on R 4.4.0 for Linux
#
# Apollo package information on Hess & Palma (2019) DOI 10.1016/j.jocm.2019.100170
# www.ApolloChoiceModelling.com
########### Change the name of the files (model, export, etc, based on name below) #####

file_name <- "SS16_SubElig_20260613_1+1_Q1_HH"

### Load libraries
library(apollo)
library(readxl)
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

# current_path = "/users/s/a/saumitra/SS"
# setwd(current_path)

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

###################################################################################################################
## Calculating wealth quintile groups

database$FirstQuin = ifelse(database$Wealth_Quin == 1, 1, 0)
database$SecondQuin = ifelse(database$Wealth_Quin == 2, 1, 0)

###################################################################################################################


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
  outputDirectory = "../Start_Search_Files_and_Output"
)



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

# Vector of parameters, including any that are kept fixed in estimation

apollo_beta = c(

                ###################### CLASS C_1 (One) #######################################    
                b_Income_C_1              = 0.77405, 
                b_HH_Member_C_1           = -0.02375, 
                b_Caste_C_1               = -2.78654, 
                b_Elderly_DiffAble_C_1    = 0.12559, 
                b_NotPlanned_C_1          = 0.88893,
                b_10k_All_C_1             = 1.11544,
                b_CurrentPolicy_C_1       = 0, #Fixed
                
                mu_worst_C_1                  = 0.42178,
                
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
                # # gamma_Benefit_Subsidy_DK_C_1  = 0, #Fixed
                
                delta_C_1                     = 0, #Fixed
                

                ###################### END: CLASS C_1 ########################################
                
                ###################### CLASS C_2 (Two) #######################################    
                b_Income_C_2              = -1.22912, 
                b_HH_Member_C_2           = -4.12854, 
                b_Caste_C_2               = -6.50590, 
                b_Elderly_DiffAble_C_2    = -4.86333, 
                b_NotPlanned_C_2          = -2.19976,
                b_10k_All_C_2             = -3.28573,
                b_CurrentPolicy_C_2       = 0, #Fixed
                
                mu_worst_C_2                  =  0.51476,  

                # # # ----- Version: Fairness ----- ----- ----- ----- #
                # # gamma_Ver_Fair_C_2            = 0.1, #Fixed
                # # 
                # # # # ----- SQ: Position First ----- ----- ----- ----- #
                # # gamma_SQ_C_2                  = 0.1, #Fixed
                # 
                # ----- Wealth indicators ----- ----- ----- ----- #
                # gamma_WI_FirstTwo_C_2         = 1.3449,
                gamma_WI_Q1_C_2               = 1.08474,
                # 
                # # ----- Household size ----- ----- ----- ----- #   
                gamma_HH_4_OrMore_Y_C_2       = 0.67864,
                # 
                # # ----- Marginalized caste ----- ----- -----  ----- #
                # gamma_Caste_Y_C_2             = 0.1,
                # # gamma_Caste_Miss_C_2          = 0.1,
                # 
                # # ----- Elderly or differetnly abled  ----- ----- ----- #  
                # gamma_Elderly_Y_C_2           = 0.1,
                # 
                # # ----- Informal Settlement ----- ----- #
                gamma_Informal_C_2            = 5.11433,
                # 
                # # ----- Ownership ----- ----- #
                # gamma_Sharing_C_2             = 0.1,
                # gamma_Ownership_C_2           = 0.62460,
                # 
                # # ----- Benefiting from the subsidy ----- ----- ----- #
                gamma_Benefit_Subsidy_Y_C_2   = -0.26505,
                # # gamma_Benefit_Subsidy_DK_C_2  = 0.1,
                
                delta_C_2                     = -6.80965,

                ###################### END: CLASS C_2 ########################################
                
                ###################### CLASS C_3 (Three) #######################################    
                b_Income_C_3              = -1.04338, 
                b_HH_Member_C_3           = -2.43374, 
                b_Caste_C_3               = -5.65110, 
                b_Elderly_DiffAble_C_3    = -1.66738, 
                b_NotPlanned_C_3          = -3.39778,
                b_10k_All_C_3             = -3.30582,
                b_CurrentPolicy_C_3       = 0, #Fixed
                
                mu_worst_C_3                  =  0.85144,
                
                # # # ----- Version: Fairness ----- ----- ----- ----- #
                # # gamma_Ver_Fair_C_3            = -0.1, #Fixed
                # # 
                # # # # ----- SQ: Position First ----- ----- ----- ----- #
                # # gamma_SQ_C_3                  = -0.1, #Fixed
                # 
                # # ----- Wealth indicators ----- ----- ----- ----- #
                # gamma_WI_FirstTwo_C_3         = 4.3142,
                gamma_WI_Q1_C_3               = 0.53573,
                # 
                # # ----- Household size ----- ----- ----- ----- #   
                gamma_HH_4_OrMore_Y_C_3       = 4.90396,
                # 
                # # ----- Marginalized caste ----- ----- -----  ----- #
                # gamma_Caste_Y_C_3             = 4.3121,
                # # gamma_Caste_Miss_C_3          = -0.1,
                # 
                # # ----- Elderly or differetnly abled  ----- ----- ----- #  
                # gamma_Elderly_Y_C_3           = -0.1,
                # 
                # # ----- Informal Settlement ----- ----- #
                gamma_Informal_C_3            = -5.92418,

                # # ----- Ownership ----- ----- #
                # gamma_Sharing_C_3             = -0.1,
                # gamma_Ownership_C_3           = -2.67388,
                # 
                # # ----- Benefiting from the subsidy ----- ----- ----- #
                gamma_Benefit_Subsidy_Y_C_3   = 4.20694,
                # # gamma_Benefit_Subsidy_DK_C_3  = -0.1,
                
                delta_C_3                     =  -3.77865
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
                 "gamma_Benefit_Subsidy_Y_C_1"#,
                 # # "gamma_Benefit_Subsidy_DK_C_1",
                 
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
                    gamma_Benefit_Subsidy_Y_C_1   *(Benefit_Subsidy_Y== 1) #+
                    # # gamma_Benefit_Subsidy_DK_C_1  *(Benefit_Subsidy_DK== 1) +

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
                    gamma_Benefit_Subsidy_Y_C_2   *(Benefit_Subsidy_Y== 1) #+
                    # # gamma_Benefit_Subsidy_DK_C_2  *(Benefit_Subsidy_DK== 1) +
  
  
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
                    gamma_Benefit_Subsidy_Y_C_3   *(Benefit_Subsidy_Y== 1) #+
                  # # gamma_Benefit_Subsidy_DK_C_3  *(Benefit_Subsidy_DK== 1) +

 
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
# model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings= list(maxIterations = 300,
#                                                                                                                 estimationRoutine = "BFGS"))
# apollo_modelOutput(model)
# apollo_saveOutput(model)
################################################################ #
## END: MODEL ESTIMATION                                            ####
################################################################ #


################################################################
####### LOADING MODEL OUTPUT FROM START SEARCH #####
################################################################
model = apollo_loadModel(file_name)
apollo_modelOutput(model)

################################################################
####### END: LOADING MODEL OUTPUT FROM START SEARCH #####
################################################################

# ##########################################################
# ## Start Search
# ###########################################################
# apollo_sink()
# ############ SET SEED (Dynamic) before search ##############################
# 
# ## To get a dynamic seed value, which will always change before each run
# ## This will get printed in the output, so it can be replicated
# ## It will be different and unique for each time this file is run
# 
# date_time_now <- Sys.time()
# date_time_now_string <- format(date_time_now,"%m%d%H%M%S")
# 
# dynamic_seed <- as.numeric(date_time_now_string)
# 
# cat("This is the dynamic seed:", dynamic_seed, " \nUse this for replication of randomization process\n")
# 
# set.seed(dynamic_seed)

# ### SEED TO USE FOR REPLICATION PROCESS
# ### SEED: 613192343
# set.seed(613192343)

# ############################################################################
# 
# ############# apollo_searchStart settings ##############
# apollo_inputs = apollo_validateInputs()
# 
# ### The search bounds for each covariate
# search_max <- apollo_beta + 0.5
# search_min <- apollo_beta - 0.5
# 
# ### Ensuring that the "worst scaling" is always positive
# search_min["mu_worst_C_1"] <- 0.01
# search_min["mu_worst_C_2"] <- 0.01
# search_min["mu_worst_C_3"] <- 0.01
# 
# start_values = apollo_searchStart(
#   apollo_beta = apollo_beta,
#   apollo_fixed = apollo_fixed,
#   apollo_probabilities = apollo_probabilities,
#   apollo_inputs = apollo_inputs,
#   searchStart_settings = list(
#     apolloBetaMax = search_max,
#     apolloBetaMin = search_min,
#     bgwIter = 60,
#     dTest = 0.5,
#     gTest = 0.001,
#     llTest = 3,
#     maxStages = 10,
#     nCandidates = 150,
#     smartStart = FALSE
#   )
# )
# 
# #############################################################
# ## Final Model with the best start values from start search
# #############################################################
# 
# # Input apollo_beta with the start search value
# apollo_beta = start_values
# apollo_inputs = apollo_validateInputs()
# # Model run with start search values
# model = apollo_estimate(apollo_beta,
#                         apollo_fixed,
#                         apollo_probabilities,
#                         apollo_inputs,
#                         estimate_settings = list(estimationRoutine = "BFGS"))
# 
# 
# apollo_modelOutput(model)
# apollo_saveOutput(model)
# 
# apollo_sink()
# #############################################################
# ## END: Final Model with the best start values from start search
# #############################################################


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
table_input <-  read.csv(paste0("../Start_Search_Files_and_Output/",file_name,"_estimates.csv"))




table_input <- table_input[,-c(3,4)]
names(table_input) <- c("Variable", "Estimate", "Robust_SE", "Robust_T_Ratio")


table_input$p_value <- 2*pnorm(abs(table_input$Robust_T_Ratio), lower.tail=FALSE)

for(n in 2:4){
  table_input[,n] <- round(table_input[,n] , 3)
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
  # "Received the subsidy (DK)",
  
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
write.xlsx(table_formatted, paste0("../Output_Tables_Fig_Manuscript/SS_SubsidyEligibility_Table_3.xlsx"))

######################################################################################
# END: Formatting Latent Class table with SE and significance. Have p-values separate
######################################################################################


######################################################################################
# Log Odds Forest Plot
######################################################################################

#### Ensure that the Previous Label and Variable are mapped together properly
#### Use this for making all LC related plots

membership_labels <- table_input[ , c("Variable", "Variable_2")]
membership_labels <- filter(membership_labels, str_detect(Variable_2, "^gamma_"))
membership_labels$Variable_2 <- substr(membership_labels$Variable_2, 1, nchar(membership_labels$Variable_2) - 4)
names(membership_labels) <- c("Clean_Label", "Base_Var")

########## Showing Class Membership ########

### Getting class probabilities from LC table
membership_coefs <- table_LC %>%
  # Filter only variables with names starting with gamma
  filter(str_detect(Variable, "^gamma_")) %>%
  mutate(
    Estimate = as.numeric(Estimate),
    Robust_SE = as.numeric(ifelse(is.na(Robust_SE), 0, Robust_SE)),
    # Extract class number from the variable name
    Class_Num = str_extract(Variable, "C_[1-3]"),
    Class = str_replace(Class_Num, "C_", "Class "),
    # Base variable for Y-axis
    Base_Var = str_remove(Variable, "_C_[1-3]$")
  ) %>%
  
  # Filter out Class 1 since it is fixed at 0 (the reference class)
  filter(Class != "Class 1")

membership_coefs <- membership_coefs %>%
  left_join(membership_labels, by = "Base_Var") %>%
  mutate(
    CI_lower = Estimate - (1.96 * Robust_SE),
    CI_upper = Estimate + (1.96 * Robust_SE)
  )

### Forest Plot
forest_plot <- ggplot(membership_coefs, 
                      aes(x = Estimate, 
                          y = reorder(Clean_Label, Estimate), 
                          # Reversing order so Class 2 lines are on top of class 3
                          color = forcats::fct_rev(Class))) +
  
  # Reference line at 0 (Class 1 Baseline)
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
  
  # Error bars for 95% CI
  geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), width = 0.2, linewidth = 0.8, position = position_dodge(width = 0.5)) +
  
  # Point estimates
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  
  
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(color = "black", size = 11),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_color_manual(values = c("Class 2" = "#009E73", 
                                "Class 3" = "#D55E00"),
                     breaks = c("Class 2", 
                                "Class 3"),
                     labels = c("Possibly Open to Reform", 
                                "Opposed to Reform")) +
  labs(
    x = NULL,
    y = NULL,
    title = file_name,
    subtitle = NULL
  )

print(forest_plot)
ggsave(paste0("../Output_Tables_Fig_Manuscript/SS_SubsidyEligibility_Extended_Figure_1.png"), 
       plot = forest_plot, width = 8, height = 10, dpi = 300)

######################################################################################
# END: Log Odds Forest Plot
######################################################################################

######################################################################################
# Predicted Probabilities
######################################################################################

temp <- database

# Percent of sample the profile represents
sample_props <- temp %>%
  group_by(Informal_Settlement,
           Benefit_Subsidy_Y,
           FirstQuin
           # HH_4_OrMore_Y
  ) %>%
  summarise(n_cases = n(), .groups = "drop") %>%
  mutate(sample_pct = n_cases / sum(n_cases))


# For predicted Probability,  preparing different profiles
synthetic_data <- expand.grid(
  Informal_Settlement  = c(1, 0),
  Benefit_Subsidy_Y    = c(1, 0),
  FirstQuin            = c(1, 0),
  HH_4_OrMore_Y        = 0.5
  
) %>%
  # Arranged by order of presentation
  arrange(
    desc(Informal_Settlement),
    desc(Benefit_Subsidy_Y),
    desc(FirstQuin)#,
    
  ) %>%
  # Join sample percentages of each profile
  left_join(sample_props, by = c("Informal_Settlement",
                                 "Benefit_Subsidy_Y", 
                                 "FirstQuin"#,
                                 # "HH_4_OrMore_Y"
  )) %>%
  # If any combination doesn't exist in the real data, percentage set to 0
  mutate(sample_pct = replace_na(sample_pct, 0)) %>%
  
  
  # Different Profile ID and label for each bar
  mutate(
    Profile_ID = row_number(),
    
    Profile_Label = paste0(
      "**", ifelse(sample_pct < 0.045, "\\*\\*", ""), "Profile ", Profile_ID, " (", scales::percent(sample_pct, accuracy = 0.1), "):**<br>",
      # ifelse(Informal_Settlement == 1,
      #        "Informal Settlement",
      #        "Planned Neighborhood"),
      # "<br>",
      # ifelse(Benefit_Subsidy_Y == 1,
      #        "Received Subsidy",
      #        "Did not Receive Subsidy"),
      # "<br>",
      ifelse(FirstQuin == 1,
             "Lowest Wealth (Q1)",
             "Higher Wealth (Q2-Q5)")
    )
  )

# Extract estimates from Apollo
est <- model$estimate

# Calculate Utilities for Predicted Probability
synthetic_data <- synthetic_data %>%
  mutate(
    V1 = 0,
    
    V2 = est["delta_C_2"] +
      # est["gamma_Ver_Fair_C_2"] * BWS_1_Version +
      est["gamma_WI_Q1_C_2"] * FirstQuin +
      est["gamma_HH_4_OrMore_Y_C_2"] * HH_4_OrMore_Y +
      est["gamma_Informal_C_2"] * Informal_Settlement +
      est["gamma_Benefit_Subsidy_Y_C_2"] * Benefit_Subsidy_Y,
    
    V3 = est["delta_C_3"] +
      # est["gamma_Ver_Fair_C_3"] * BWS_1_Version +
      est["gamma_WI_Q1_C_3"] * FirstQuin +
      est["gamma_HH_4_OrMore_Y_C_3"] * HH_4_OrMore_Y +
      est["gamma_Informal_C_3"] * Informal_Settlement +
      est["gamma_Benefit_Subsidy_Y_C_3"] * Benefit_Subsidy_Y
  )

# Calculate Probabilities
synthetic_data <- synthetic_data %>%
  mutate(
    sum_exp   = exp(V1) + exp(V2) + exp(V3),
    P_Class_1 = exp(V1) / sum_exp,
    P_Class_2 = exp(V2) / sum_exp,
    P_Class_3 = exp(V3) / sum_exp
  )


### For GGPlot, Formatting and Data Transformation
plot_data <- synthetic_data %>%
  pivot_longer(
    cols = c(P_Class_1, P_Class_2, P_Class_3), 
    names_to = "Raw_Class",
    values_to = "Probability"
  ) %>%
  mutate(
    
    Class = factor(Raw_Class, 
                   levels = c("P_Class_1", 
                              "P_Class_2", 
                              "P_Class_3"), 
                   
                   labels = c("Supporters of Reform",
                              "Possibly Open to Reform",
                              "Opposed to Reform" 
                   )
    ),
    
    
    Col_Facet = factor(
      ifelse(Informal_Settlement == 1, 
             "Informal Settlement", 
             "Planned Neighborhood"),
      
      levels = c("Informal Settlement", 
                 "Planned Neighborhood")
    ),
    
    Row_Facet = factor(
      ifelse(Benefit_Subsidy_Y == 1,
             "Received Subsidy",
             "Did not Receive Subsidy"),
      
      levels = c("Received Subsidy", "Did not Receive Subsidy")
    ),
    
    Profile_Label = factor(Profile_Label, levels = rev(unique(Profile_Label)))
  )

#################  Plotting ###################################################
prob_plot <- ggplot(plot_data, aes(x = Probability, y = Profile_Label, fill = Class)) +
  
  geom_bar(
    aes(alpha = ifelse(sample_pct < 0.039, 0.3, 0.75)), 
    stat = "identity", 
    position = position_stack(reverse = TRUE),
    width = 0.7
  ) +
  
  scale_alpha_identity() +
  
  geom_text(
    aes(label = ifelse(Probability >= 0.05, scales::percent(Probability, accuracy = 1), "")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.5, 
    color = "black", 
    fontface = 2
  ) +
  
  # facet_grid
  facet_grid2(
    Row_Facet ~ Col_Facet, 
    scales = "free_y", 
    independent = "y",
    switch = "y"
  ) +
  
  scale_fill_manual(
    values = c("Supporters of Reform"        = "#4169E1",
               "Possibly Open to Reform"     = "#009E73",
               "Opposed to Reform"           = "#D55E00" 
    ),
    breaks = c("Supporters of Reform",
               "Possibly Open to Reform",
               "Opposed to Reform" 
    )
  ) +
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  
  labs(
    caption = file_name,
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.text.y = ggtext::element_markdown(size = 10, color = "black", lineheight = 1.1),
    
    plot.caption = element_text(hjust = 0.5, face = "bold"),
    plot.caption.position = "plot",
    
    strip.background.x = element_rect(fill = "grey90", color = "white", linewidth = 0),
    
    strip.text.x = element_text(
      face = "bold", size = 12,
      margin = margin(t = 15, b = 15, r = 5, l = 5) 
    ),
    
    # Grey background for Y axix label
    strip.background.y = element_rect(fill = "grey90", color = "white", linewidth = 0),
    
    # Text Format and 90 rotating text on Y axis
    strip.text.y.left = element_text(
      face = "bold", size = 12, angle = 90,
      margin = margin(r = 10, l = 10, t = 5, b = 5)
    ),
    
    strip.placement = "outside",
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    
    panel.spacing.y = unit(1.5, "lines"), 
    panel.spacing.x = unit(1.5, "lines"),
    plot.margin = margin(t = 20, r = 15, b = 40, l = 10) 
  )

print(prob_plot)

# Save the plot
ggsave(paste0("../Output_Tables_Fig_Manuscript/SS_SubsidyEligibility_Figure_3.png"),
       plot = prob_plot, width = 12, height = 8, dpi = 300) 


