library(tidyverse)
library(magrittr)
library(fuzzyjoin)
library(assertthat)

# ----- Configuration ---------------------------------------
merged_filename <- "merged_201701-03.csv"

# ----- Evaluate ---------------------------------
evaluate_mitl <- function(mms.target, predictions){
    
    assert_that("ObservationId" %in% colnames(predictions), msg = str(colnames(predictions)))
    
    # add one if the observations appear to be 1-based
    predictions <- mutate(predictions, ObservationId = ObservationId + 1 - min(ObservationId) )
    
    # sort them by prediction weight
    predictions <- predictions %>% arrange(desc(Selected))
    
    # use MITL to select data points
    true.predictions.count <- sum(mms.target$Selected == T)
    predictions$MITL_Selected <- F
    predictions$MITL_Selected[1:true.predictions.count] <- T
    
    # select the predictions (right join so if things are missing)
    pred.cmp <- right_join(predictions %>% select(-Selected), 
                                         mms.target, by=c("ObservationId" = "X1"))
    
    pred.cmp$MITL_Selected[is.na(pred.cmp$MITL_Selected)] <- F
    
    # computed metrics
    found <- pred.cmp %>% filter(Selected == T & MITL_Selected == T)
    missed <- pred.cmp %>% filter(Selected == T & MITL_Selected == F)
    
    list(total.sitl=true.predictions.count,
         found = nrow(found),
         missed = nrow(missed),
         class.error = with(pred.cmp,{mean(MITL_Selected != Selected)}))
         #F1 = with(pred.cmp,{MLmetrics::F1_Score(Selected, MITL_Selected)}))
}

# ----- Run actual evaluation -----

csv.files <- list.files(path = "results_prelim", pattern = "*.csv")
mms.target <- read_csv(merged_filename, col_types = readr::cols())

results <- list()
i <- 1
for(g in csv.files){
    cat("Processing: ", g, "\n")
    mitl <- read_csv(paste("results_prelim/",g, sep=""), col_types = readr::cols())
    l <- evaluate_mitl(mms.target, mitl)
    l$group <- stringr::str_split(csv.files[i], "_", 2)[[1]][1]
    results[[i]] <- l
    i<-i+1
}

results.frame <- results %>% transpose() %>% simplify_all() %>% as.data.frame() %>% 
    select(group,found,missed,class.error) %>% arrange(class.error)

str(results.frame %>% knitr::kable())



# ----- Report results -----------

library(xtable)

print(xtable(results.frame %>% arrange(class.error), digits = 5), type="html")


