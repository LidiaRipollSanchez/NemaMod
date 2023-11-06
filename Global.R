#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries

source('dependencies.R')

#Make App public 
#rsconnect::deployApp('~/Desktop/Nextcloud/Lidia_new/Website App R/NemaMod')

# #Load the data
# foldername = "~/Desktop/Nextcloud/Lidia_new/Website App R/NemaMod/Data/"
# NeuronID <- read.csv(paste0(foldername, 'Neuron_ID_class.csv'))
# NPPpairs <- read.csv(paste0(foldername,'neuropeptide_pairs.csv'), header = FALSE)
# ReceptorsNPP <- read.csv(paste0(foldername,'20230602_Ligand_receptors_interactions_all_EC50_explicit_500nM.csv'))
# 
# #load networks in lists of dataframes to facilitate access
# df_networks <-
#   list.files(path = paste0(foldername,"Full_network"), pattern = "*.csv", full.names = T) %>%
#   map(~as.matrix(read.csv(., header = F, row.names = c(NeuronID %>% pull(nodeLabel)), col.names = c(NeuronID %>% pull(nodeLabel))))) %>%
#   setNames (c('GJ', 'Mon', 'NPPlr', 'NPPmr', 'NPPsr', 'Syn'))
# df_sr <-
#   list.files(path = paste0(foldername, "Individual_net_short_r"), pattern = "*.csv", full.names = T) %>%
#   map(~as.matrix(read.csv(., row.names = 1)))
# df_mr <-
#   list.files(path = paste0(foldername, "Individual_net_mid_r"), pattern = "*.csv", full.names = T) %>%
#   map(~as.matrix(read.csv(., row.names = 1)))
# df_lr <-
#   list.files(path = paste0(foldername, "Individual_net_long_r"), pattern = "*.csv", full.names = T) %>%
#   map(~as.matrix(read.csv(., row.names = 1)))
# df_NPP_sr <-
#   list.files(path = paste0(foldername, "NPP_grouped_SR"), pattern = "*.csv", full.names = T) %>%
#   map(~as.matrix(read.csv(., row.names = 1)))
# df_NPP_mr <-
#   list.files(path = paste0(foldername, "NPP_grouped_MR"), pattern = "*.csv", full.names = T) %>%
#   map(~as.matrix(read.csv(., row.names = 1)))
# df_NPP_lr <-
#   list.files(path = paste0(foldername, "NPP_grouped_LR"), pattern = "*.csv", full.names = T) %>%
#   map(~as.matrix(read.csv(., row.names = 1)))
# 
# 
# #Get a dataframe where for each neuron it indicates the NPP-GPCR pairs that use that neuron
# NPPpairsbyneuron_sr <- data.frame()
# NPPpairsbyneuron_mr <- data.frame()
# NPPpairsbyneuron_lr <- data.frame()
# 
# for (i in 1:302){
#   for (j in 1:nrow(NPPpairs)){
#     if(sum(df_sr[[j]][i,]) == 0 & sum(df_sr[[j]][,i]) == 0){NPPpairsbyneuron_sr[i,j] <- NA}
#     else if(sum(df_sr[[j]][i,]) >= 0 | sum(df_sr[[j]][,i]) >= 0){NPPpairsbyneuron_sr[i,j] <- NPPpairs[j,]}
#     if(sum(df_mr[[j]][i,]) == 0 & sum(df_mr[[j]][,i]) == 0){NPPpairsbyneuron_mr[i,j] <- NA}
#     else if(sum(df_mr[[j]][i,]) >= 0 | sum(df_mr[[j]][,i]) >= 0){NPPpairsbyneuron_mr[i,j] <- NPPpairs[j,]}
#     if(sum(df_lr[[j]][i,]) == 0 & sum(df_lr[[j]][,i]) == 0){NPPpairsbyneuron_lr[i,j] <- NA}
#     else if(sum(df_lr[[j]][i,]) >= 0 | sum(df_lr[[j]][,i]) >= 0){NPPpairsbyneuron_lr[i,j] <- NPPpairs[j,]}
#   }
# }
# rownames(NPPpairsbyneuron_sr) <- NeuronID$nodeLabel
# rownames(NPPpairsbyneuron_mr) <- NeuronID$nodeLabel
# rownames(NPPpairsbyneuron_lr) <- NeuronID$nodeLabel
# 
# #Get dataframe for each threshold where there are the NPP-GPCR pairs used for each edge
# NPPpairs_sr <- vector("list", length = 302 * 302)
# NPPpairs_mr <- vector("list", length = 302 * 302)
# NPPpairs_lr <- vector("list", length = 302 * 302)
# 
# NPPpairs_sr_n <- vector(mode='list', length=92)
# NPPpairs_mr_n <- vector(mode='list', length=92)
# NPPpairs_lr_n <- vector(mode='list', length=92)
# 
# for (i in 1:302){
#   for (j in 1:302){
#     NPPpairs_sr_n <- character(0)
#     NPPpairs_mr_n <- character(0)
#     NPPpairs_lr_n <- character(0)
#     for (k in 1:92){
#       if (df_sr[[k]][i,j] >= 1) {
#         NPPpairs_sr_n <- c(NPPpairs_sr_n, NPPpairs[k,])
#       }
#       else if (df_sr[[k]][i,j] < 1) {
#         NPPpairs_sr_n <- c(NPPpairs_sr_n, NA)
#       }
#       if (df_mr[[k]][i,j] >= 1) {
#         NPPpairs_mr_n <- c(NPPpairs_mr_n, NPPpairs[k,])
#       }
#       else if (df_mr[[k]][i,j] < 1) {
#         NPPpairs_mr_n <- c(NPPpairs_mr_n, NA)
#       }
#       if (df_lr[[k]][i,j] >= 1) {
#         NPPpairs_lr_n <- c(NPPpairs_lr_n, NPPpairs[k,])
#       }
#       else if (df_lr[[k]][i,j] < 1) {
#         NPPpairs_lr_n <- c(NPPpairs_lr_n, NA)
#       }
#     }
#     NPPpairs_sr[[(i - 1) * 302 + j]] <- NPPpairs_sr_n
#     NPPpairs_mr[[(i - 1) * 302 + j]] <- NPPpairs_mr_n
#     NPPpairs_lr[[(i - 1) * 302 + j]] <- NPPpairs_lr_n
#   }
# }
# 
# #Get neuropeptides names
# Neuropeptides<-unique(ReceptorsNPP$GeneLigand)
# Neuropeptides <- data.frame(Neuropeptides)
# 
# #Save all networks in Rdata to facilitate the import of the data everytime the app runs
# save(NeuronID, NPPpairs, Neuropeptides, ReceptorsNPP, df_networks, df_sr, df_mr, df_lr, NPPpairsbyneuron_sr,
# NPPpairsbyneuron_mr, NPPpairsbyneuron_lr, df_NPP_sr, df_NPP_mr, df_NPP_lr, NPPpairs_sr, NPPpairs_mr, NPPpairs_lr,
# file = paste0("~/Desktop/Nextcloud/Lidia_new/Website App R/NemaMod/", "Data.RData"))

#Load data
#load("~/Desktop/Nextcloud/Lidia_new/Website App R/NemaMod/Data.RData")
load('Data.RData')

# HELP & INTRO DATA ------------------------------------------------------------

steps <- read.csv("help.csv")
intro <- read.csv("intro.csv")
