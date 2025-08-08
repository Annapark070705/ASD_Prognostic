library(flowchart)
library(rvg)
library(officer)
raw_fc <- raw %>% as_fc

tmp <- raw_fc %>% 
  fc_filter(N = nrow(naomit), label = "Complete Observation", show_exc = T) %>% 
  fc_split(N = c(nrow(naomit[idx,]), nrow(naomit[-idx,])), label = c("Train", "Test")) %>% 
  fc_split(N = c(nrow(naomit[idx,][ASD == "Y"]), nrow(naomit[idx,][ASD == "N"]), nrow(naomit[-idx,][ASD == "Y"]), nrow(naomit[-idx,][ASD == "N"])),
           label = c("ASD", "no ASD")) %>% 
  fc_draw %>% 
  dml
  
ppt2 <- read_pptx()
ppt2 %>% add_slide() %>% 
  ph_with(
    tmp,
    location = ph_location_fullsize()
  )

print(ppt2, "flowchart.pptx")

