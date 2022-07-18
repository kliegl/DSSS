
generated = head(d,0)
while (nrow(generated) < n_sample) {
  # For debug purposes
  cat(nrow(generated),"\n")
  flush.console()
  
  tmp = d
  
  # Calculate the histogram for each dimension
  # and select one value at a time, slicing the
  # original dataset according to its histogram
  for (i in 1:length(dimensions)) {
    
    colname = dimensions[i]
    if (class(d[[colname]]) %in% c("numeric") &&
        sum(d[[colname]] == as.integer(d[[colname]]),na.rm = TRUE) == 0
    ) {
      # Numerical variable. Histogram with Rice's Rule
      
      # If there are NA's, stratify on those
      
      na_count = sum(is.na(tmp[[colname]]))
      not_na_count = length(tmp[[colname]]) - na_count
      
      s = sample(c(0,1),prob = c(not_na_count,na_count),1)
      
      if (s == 0) {
        # Histogram stratification based on breaks calculated on the
        # population
        
        n_breaks = floor(2*sum(!is.na(d[[colname]]))**((1/3)))
        bar_size = (max(d[[colname]],na.rm = TRUE)-min(d[[colname]],na.rm = TRUE))/n_breaks
        
        breaks = sapply(0:n_breaks,function(i) {min(d[[colname]],na.rm = TRUE) + i*bar_size})
        
        h = hist(tmp[[colname]],breaks=breaks,plot = F)
        
        # Select one bar of the histogram according to the density
        bar_id  = sample(1:length(h$mids),prob = h$counts,1)
        
        bar_start = h$breaks[bar_id]
        bar_end = h$breaks[bar_id + 1]
        
        tmp = tmp[tmp[[colname]] >= bar_start & tmp[[colname]] < bar_end & !is.na(tmp[[colname]]),]
      } else {
        # NA
        tmp = tmp[is.na(tmp[[colname]]),]
      }
      
    } else {
      # Categorical variable
      
      # Histogram for the selected dimension
      aggr = as.data.frame(table(tmp[,colname],useNA="ifany"))
      names(aggr) = c("dim","count")
      
      # Generate a value according to the histogram
      generated_value = sample(aggr$dim,prob=aggr$count,1)
      
      # Slice the actual multivariate histogram in order to
      # take only records with the selected value on the
      # selected dimension
      if (!is.na(generated_value)) {
        tmp = tmp[tmp[[colname]] == generated_value & !is.na(tmp[[colname]]),]
      }
      else {
        tmp = tmp[is.na(tmp[[colname]]),]
      }
      
    }
    
  }
  
  # Once the procedure finishes, we get a bulk of records
  # with the same values of each dimension. Let's take
  # one of these records uniformly
  random_index = sample(1:nrow(tmp),1)
  new_record = tmp[random_index,]
  
  # Let's remove duplicates
  inserted_record = sqldf("select * from new_record except select * from generated")
  
  # Insert in the "generated" data frame and repeat until desired sample size is reached
  generated = rbind(generated,inserted_record)
}