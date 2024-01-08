#' #' This file loads the generated data of meiotic and mitotic rates and plots them
#' #' using different visualization methods.
#' library(ggplot2)
#' library(readr)
#'
#' # -------------Load and Process Data-------------------------
#' # Locate the folder to investigate
#' date <- "2024-01-04"
#'
#' # Set column names
#' my_data_cols <-
#'   read_table(paste0("data/", date, "/1.txt"),
#'              n_max  = 1,
#'              col_names = FALSE)
#' my_data_cols <- cbind('embryo', my_data_cols)
#'
#' # Read the first txt file
#' my_data <-
#'   read_table(paste0("data/", date, "/1.txt"),
#'              skip = 1,
#'              col_names = FALSE)
#' colnames(my_data) <- my_data_cols[1,]
#'
#' # Read all the rest of the data
#' for (i in 2:100) {
#'   temp <-
#'     read_table(paste0("data/", date, "/", i, ".txt"),
#'                skip = 1,
#'                col_names = FALSE)
#'   colnames(temp) <- my_data_cols[1,]
#'   my_data <- rbind(my_data, temp)
#' }
#'
#'
#' # Draw scatterplots
#' ggplot(data = my_data, aes(x = my_data$prob.meio, y = my_data$prob.mito)) + geom_point()
#' ggplot(data = my_data,
#'        aes(
#'          x = my_data$prob.meio,
#'          y = my_data$prob.mito,
#'          color = my_data$dispersal
#'        )) + geom_point()
#'
#' # 0.01 tolerance
#' # 95% Quantiles
#'
#' # Prob. meio
#' # 2.5%     97.5%
#' # 0.1583445 0.5501766
#'
#' # Prob. mito
#' # 2.5%       97.5%
#' # 0.009530512 0.126553831
