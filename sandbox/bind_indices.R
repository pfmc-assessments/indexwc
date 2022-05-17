 
bind_indices <- function(dir, sdmtmb_index){

	vast_i <- read.csv(file.path(dir, "Index.csv")) %>%
	  mutate(index = "VAST", year = as.numeric(Time), est = Estimate, 
	    se = Std..Error.for.ln.Estimate.) %>% 
	  select(index, year, est, se) %>% 
	  mutate(lwr = exp(log(est) + qnorm(0.025) * se)) %>% 
	  mutate(upr = exp(log(est) + qnorm(0.975) * se))
	
	sdm_i <- sdmtmb_index %>% mutate(index = "sdmTMB")
	
	both_i <- bind_rows(sdm_i, vast_i) %>% filter(est > 0)
	save(both_i, file = file.path(dir, "both_indices.Rdata"))
	return(both_i)
}