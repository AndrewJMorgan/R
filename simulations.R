
#load packages
library(tidyverse)


#source LBA function
source("LBA.R")

#simulate LBA
output_tmp = rlba(n = 10000,  #number of decisions to simulate
     b = 2,      #threshold - higher values indicate greater caution
     A = 1,      #maximum starting point
     vs = c(1,1.2),    #rates of evidence accumulation (drift rates)  - one for each decision alternative 
     s = 0.2,      #standard deviation of drift rates
     t0 = 0.1    #non decision time (time for encoding visual stimuli and generating motor response)    
  )

#reformat output_tmp object to a more friendly data frame
output = tibble(
  rt = output_tmp[grep("rt",names(output_tmp))],
  resp = output_tmp[grep("resp",names(output_tmp))]
) %>%
  filter(rt < 5) #remove simulated responses with time greater than 5 secs


n_choices = length(unique(output$resp))
proportion = rep(NA,n_choices)
names(proportion) = as.character(1:n_choices)
for(r in 1:n_choices){
  proportion[r] = sum(output$resp == r) / length(output$resp)
}
proportion

#plot response time distributions
ggplot(output) +                      #specify relevant data frame
  geom_histogram(aes(x=rt),bins=100) +           #specify type of plot
  facet_grid(.~resp)

