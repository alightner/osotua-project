library(igraph)
library(tidyverse)
library(data.table)
library(ggsci)

# -------- standard osotua ------------

num_simulations <- 10000
duration <- 50
vtrate <- 0.1
minstock <- 64
maxstock <- 600
init_herdsize <- 70

# uncomment and source to run

#source('~/Desktop/Docs/PhD projects/Osotua ABM model/osotua_abm/pairwise_osotua.R')

df <- data.frame(df); colnames(df) <- c('stock1','stock2','dead1','dead2')
mdt <- data.frame(mdt); colnames(mdt) <- c('t', 'p1_stock','p2_stock','p1_req', 'p2_req')

df$dead1[is.na(df$dead1)] <- duration
df$dead2[is.na(df$dead2)] <- duration

#write.table(df, "osotua_df_standard.csv", sep=',', row.names = FALSE)
#write.table(mdt, "osotua_mdt_standard.csv", sep=',', row.names = FALSE)


# -------- single defection ------------

num_simulations <- 10000
duration <- 50
vtrate <- 0.1
minstock <- 64
maxstock <- 600
init_herdsize <- 70

# uncomment and source to run

dfpr <- 0.5  # probability of defection
defect_node <- 1

#source('~/Desktop/Docs/PhD projects/Osotua ABM model/osotua_abm/pairwise_defect.R')

df <- data.frame(df); colnames(df) <- c('stock1','stock2','dead1','dead2')
mdt <- data.frame(mdt); colnames(mdt) <- c('t', 'p1_stock','p2_stock','p1_req', 'p2_req')

df$dead1[is.na(df$dead1)] <- duration
df$dead2[is.na(df$dead2)] <- duration

#write.table(df, "defect_df.csv", sep=',', row.names = FALSE)
#write.table(mdt, "defect_mdt.csv", sep=',', row.names = FALSE)


# -------- mutual defection ------------

num_simulations <- 10000
duration <- 50
vtrate <- 0.1
minstock <- 64
maxstock <- 600
init_herdsize <- 70

# uncomment and source to run

dfpr <- 0.5  # probability of defection
defect_node <- c(1,2)

#source('~/Desktop/Docs/PhD projects/Osotua ABM model/osotua_abm/pairwise_mutual_defect.R')

df <- data.frame(df); colnames(df) <- c('stock1','stock2','dead1','dead2')
mdt <- data.frame(mdt); colnames(mdt) <- c('t', 'p1_stock','p2_stock','p1_req', 'p2_req')

df$dead1[is.na(df$dead1)] <- duration
df$dead2[is.na(df$dead2)] <- duration

#write.table(df, "mutual_defect_df.csv", sep=',', row.names = FALSE)
#write.table(mdt, "mutual_defect_mdt.csv", sep=',', row.names = FALSE)


# -------- no exchange (control) ------------

num_simulations <- 10000
duration <- 50
vtrate <- 0.1
minstock <- 64
maxstock <- 600
init_herdsize <- 70

# uncomment and source to run

#source('~/Desktop/Docs/PhD projects/Osotua ABM model/osotua_abm/pairwise_noexchange.R')

df <- data.frame(df); colnames(df) <- c('stock1','stock2','dead1','dead2')
mdt <- data.frame(mdt); colnames(mdt) <- c('t', 'p1_stock','p2_stock','p1_req', 'p2_req')

df$dead1[is.na(df$dead1)] <- duration
df$dead2[is.na(df$dead2)] <- duration

#write.table(df, "df_noexchange.csv", sep=',', row.names = FALSE)
#write.table(mdt, "mdt_noexchange.csv", sep=',', row.names = FALSE)

