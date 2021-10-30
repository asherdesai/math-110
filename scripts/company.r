# Company Class

# Construct "Company" object
Company.construct <- function(d, company_name) {
  dc <- d %>% filter(author_id == company_name)
  dr <- d %>% filter(grepl(paste("@", company_name, sep = ""), text))
  da <- rbind(dc, dr)
  return(da)
}

Company.VirginAtlantic <- Company.construct(d, "VirginAtlantic")

# Return number of threads for that company
# comp.get_num_threads <- function(comp) {
# 
# }

# Return data frame with author/text for one thread
# comp.get_thread <- function(comp) {
# 
# }