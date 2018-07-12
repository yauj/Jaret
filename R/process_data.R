# Suggested Workflow:
#  1. combine_df
#  2. process_df
#  3. scale

# Combines multiple dataframes by rowname
# Has different options for cleansing data
combine_df = function(df_list, # Has to be List
                      rep_num = "add", # How to deal with repeated rows, given that column is numeric
                      rep_fac = "last", # How to deal with repeated rows, given that column is a factor
                      mis_num = NA, # How to deal with missing rows, given that column is a factor (currently just puts what you input)
                      mis_fac = NA) { # How to deal with missing rows, given that column is numeric (currently just puts what you input)
   if (!is.list(df_list)) {
      stop("df_list must be a list")
   }
   
   all_row_names = c()
   all_col_names = c()
   n_col = 0
   for (k in 1:length(df_list)) {
      all_row_names = c(all_row_names, rownames(df_list[[k]]))
      all_col_names = c(all_col_names, colnames(df_list[[k]]))
   }
   
   all_row_names = levels(as.factor(all_row_names))
   
   return_df = as.data.frame(matrix(NA, nrow = length(all_row_names), ncol = length(all_col_names),
                                    dimnames = list(all_row_names, all_col_names)))
   
   for (k in 1:length(df_list)) {
      missing_rows = setdiff(rownames(return_df), rownames(df_list[[k]]))
      
      for (j in colnames(df_list[[k]])) {
         for (i in rownames(df_list[[k]])) {
            if (!is.na(return_df[i, j])) { # Then we have a repeated row
               if (is.factor(df_list[[k]][i, j])) { # Factor column
                  if (rep_fac == "first") {
                     # Do Nothing
                  } else if (rep_fac == "last") {
                     return_df[i, j] = df_list[[k]][i, j]
                  } else {
                     stop("Invalid Repeated Factor instruction given.")
                  }
               } else { # Numeric Column
                  if (rep_num == "add") {
                     return_df[i, j] = return_df[i, j] + df_list[[k]][i, j]
                  } else if (rep_num == "first") {
                     # Do Nothing
                  } else if (rep_num == "last") {
                     return_df[i, j] = df_list[[k]][i, j]
                  } else {
                     stop("Invalid Repeated Numeric instruction given.")
                  }
               }
            } else {
               return_df[i, j] = df_list[[k]][i, j]
            }
         }
         
         if (is.factor(df_list[[k]][, j])) { # Is Factor
            return_df[missing_rows, j] = mis_fac
            
            return_df[, j] = as.factor(return_df[, j])
         } else { # Is Numeric
            return_df[missing_rows, j] = mis_num
            
            return_df[, j] = as.numeric(return_df[, j]) # Not quite necessary
         }
      }
   }
   
   return(combine_df)
}

# Produces a numeric data frame and has optionality to deal with NA data
# If data is quantitative, then NA will be it's own category
# If data is qualitative, then NA will be dealt with according to na_opt
process_df = function(raw_df, # Input Data Frame
                      na_opt = "min") { # How to deal with NA qualitative data
   this_df = NULL
   this_df_colnames = c()
   
   for (j in 1:ncol(raw_df)) {
      temp_name = colnames(raw_df)
      temp_name = temp_name[j]
      
      if (is.factor(raw_df[, j])) {
         temp_df = c()
         temp_levels = levels(raw_df[, j])
         
         containsNA = (1 * sum(is.na(temp_df)) > 0)
         
         temp_colnames = NULL
         for (k in 1:length(temp_levels)) {
            if (k != length(temp_levels) || containsNA) {
               temp_df = c(temp_df, list(1 * (raw_df[, j] == temp_levels[k])))
               temp_colnames = c(temp_colnames, unlist(paste(temp_name, gsub(" ", "_", temp_levels[k]), sep = ".")))
            }
         }
      } else {
         temp_df = raw_df[, j]
         if (na_opt == "min") {
            temp_df[is.na(temp_df)] = (2 * min(temp_df, na.rm = TRUE)) - max(temp_df, na.rm = TRUE)
         } else if (na_opt == "mean") {
            temp_df[is.na(temp_df)] = mean(temp_df, na.rm = TRUE)
         } else if (na_opt == "medium") {
            temp_df[is.na(temp_df)] = medium(temp_df, na.rm = TRUE)
         } else if (is.numeric(na_opt)) {
            temp_df[is.na(temp_df)] = na_opt
         } else {
            stop("Invalid NA option")
         }
         temp_df = as.numeric(temp_df)
         temp_df = data.frame(temp_df)
         
         temp_colnames = c(temp_name)
      }
      
      this_df = c(this_df, list(temp_df))
      this_df_colnames = c(this_df_colnames, temp_colnames)
   }
   
   this_df = as.data.frame(this_df)
   names(this_df) = this_df_colnames
   return(this_df)
}