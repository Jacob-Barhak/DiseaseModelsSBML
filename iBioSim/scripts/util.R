save_csv <- function(df, csv_name) {
  x <- data.frame("min" = apply(df, 2, function(x) {
                                                      y = x[x>0]
                                                      if(length(y) > 0) {min(y)}
                                                      else {0}
                                                   }),
                  "max" = apply(df, 2, function(x) {
                                                      y = x[x>0]
                                                      if(length(y) > 0) {max(y)}
                                                      else {0}
                  }),
                  "mean" = apply(df, 2, function(x) {
                                                      y = x[x>0]
                                                      if(length(y) > 0) {mean(y)}
                                                      else {0}
                  }),
                  "sd" = apply(df, 2, function(x)  {
                                                      y = x[x>0]
                                                      if(length(y) > 0) {sd(y)}
                                                      else {0}
                  }))
  
  write.csv(x, file = csv_name)
  
  x
}