### Changes:
### - Add KNN
### - Add temp_sum and K_sum helper functions

# K-Fold ------------------------------------------------------------------
cv_k_fold = function(x, y, train_fun, K = 10,
                     knn_k = 1,
                     rf_ntree = c(500), rf_mtry = NULL,
                     svm_kernal = NULL, svm_scale = TRUE,
                        svm_nu = c(0.5), svm_gamma = NULL,
                        svm_coef0 = c(0), svm_degree = c(3),
                     plot = FALSE, dev = FALSE) {
   temp_sum = get_temp_sum(train_fun,
                           knn_k,
                           rf_ntree, rf_mtry,
                           svm_kernal, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree)

   K_sum = list(temp_sum, temp_sum)
   K_num = 0

   # Get K-Fold idxs
   fold_size = round(length(y) / K)
   sample_idx = sample(1:length(y))
   K_idx = c()
   for (k in 1:(K - 1)) {
      K_idx = c(K_idx, list(sample_idx[1:fold_size]))
      sample_idx = sample_idx[-c(1:fold_size)]
   }
   K_idx = c(K_idx, list(sample_idx))

   for (k in 1:K) {
      if (dev) {cat(paste("\n", k, "iteration \n"))}

      k_train_x = x[-c(K_idx[[k]]), ]
      k_train_y = y[-c(K_idx[[k]])]
      k_test_x = x[c(K_idx[[k]]), ]
      k_test_y = y[c(K_idx[[k]])]

      K_sum = get_K_sum(k_train_x, k_train_y, k_test_x, k_test_y,
                        K_sum, train_fun,
                        knn_k,
                        rf_ntree, rf_mtry,
                        svm_kernal, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree,
                        dev)

      K_num = K_num + length(k_test_y)

      if (dev && k %% 5 == 0) {
         print(K_sum[[1]] / K_num)
      }
   }

   K_mean = K_sum[[1]] / K_num
   K_sd = sqrt((K_sum[[2]] - (((K_sum[[1]]) ^ 2) / K_num)) / (K_num - 1))

   if (length(dim(K_mean)) == 1 && plot) {
      plot_names = as.numeric(names(K_mean))
      plot(x = plot_names, y = K_mean, type = "l")
      lines(x = plot_names, y = K_mean - (2 * K_sd), col = "gray")
      lines(x = plot_names, y = K_mean + (2 * K_sd), col = "gray")
   } else if (length(dim(K_mean)) == 2 && plot) {
      plot_names = dimnames(K_mean)
      plot_names[[1]] = as.numeric(plot_names[[1]])
      plot_names[[2]] = as.numeric(plot_names[[2]])
      K_plot = plot_ly() %>% add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean) %>%
                             add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean - (2 * K_sd), opacity = 0.7) %>%
                             add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean + (2 * K_sd), opacity = 0.7)
      return(list(err = K_mean, sd = K_sd, plot = K_plot))
   }

   return(list(err = K_mean, sd = K_sd))
}


# K-Fold (using fold for training) ----------------------------------------
cv_rev_k_fold = function(x, y, train_fun, K = 10,
                         knn_k = 1,
                         rf_ntree = c(500), rf_mtry = NULL,
                         svm_kernal = NULL, svm_scale = TRUE,
                            svm_nu = c(0.5), svm_gamma = NULL,
                            svm_coef0 = c(0), svm_degree = c(3),
                         plot = FALSE, dev = FALSE) {
   temp_sum = get_temp_sum(train_fun,
                           knn_k,
                           rf_ntree, rf_mtry,
                           svm_kernal, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree)

   K_sum = list(temp_sum, temp_sum)
   K_num = 0

   # Get K-Fold idxs
   fold_size = round(length(y) / K)
   sample_idx = sample(1:length(y))
   K_idx = c()
   for (k in 1:(K - 1)) {
      K_idx = c(K_idx, list(sample_idx[1:fold_size]))
      sample_idx = sample_idx[-c(1:fold_size)]
   }
   K_idx = c(K_idx, list(sample_idx))

   for (k in 1:K) {
      if (dev) {cat(paste("\n", k, "iteration \n"))}

      k_train_x = x[c(K_idx[[k]]), ]
      k_train_y = y[c(K_idx[[k]])]
      k_test_x = x[-c(K_idx[[k]]), ]
      k_test_y = y[-c(K_idx[[k]])]

      K_sum = get_K_sum(k_train_x, k_train_y, k_test_x, k_test_y,
                        K_sum, train_fun,
                        knn_k,
                        rf_ntree, rf_mtry,
                        svm_kernal, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree,
                        dev)

      K_num = K_num + length(k_test_y)

      if (dev && k %% 5 == 0) {
         print(K_sum[[1]] / K_num)
      }
   }

   K_mean = K_sum[[1]] / K_num
   K_sd = sqrt((K_sum[[2]] - (((K_sum[[1]]) ^ 2) / K_num)) / (K_num - 1))

   if (length(dim(K_mean)) == 1 && plot) {
      plot_names = as.numeric(names(K_mean))
      plot(x = plot_names, y = K_mean, type = "l")
      lines(x = plot_names, y = K_mean - (2 * K_sd), col = "gray")
      lines(x = plot_names, y = K_mean + (2 * K_sd), col = "gray")
   } else if (length(dim(K_mean)) == 2 && plot) {
      plot_names = dimnames(K_mean)
      plot_names[[1]] = as.numeric(plot_names[[1]])
      plot_names[[2]] = as.numeric(plot_names[[2]])
      K_plot = plot_ly() %>% add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean) %>%
         add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean - (2 * K_sd), opacity = 0.7) %>%
         add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean + (2 * K_sd), opacity = 0.7)
      return(list(err = K_mean, sd = K_sd, plot = K_plot))
   }

   return(list(err = K_mean, sd = K_sd))
}

# Monte Carlo -------------------------------------------------------------
cv_monte_carlo = function(x, y, train_fun, train_size = 1000, itr = 10, time_limit = -1,
                          knn_k = 1,
                          rf_ntree = c(500), rf_mtry = NULL,
                          svm_kernal = NULL, svm_scale = TRUE,
                             svm_nu = c(0.5), svm_gamma = NULL,
                             svm_coef0 = c(0), svm_degree = c(3),
                          plot = FALSE, dev = FALSE) {
   temp_sum = get_temp_sum(train_fun,
                           knn_k,
                           rf_ntree, rf_mtry,
                           svm_kernal, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree)

   K_sum = list(temp_sum, temp_sum)
   K_num = 0

   if (time_limit > 0) {
      method_start_time = Sys.time()
   }

   for (k in 1:itr) {
      if (dev) {cat(paste("\n", k, "iteration \n"))}

      sample_idx = sample(1:length(y))
      train_idx = sample_idx[c(1:train_size)]
      test_idx = sample_idx[-c(1:train_size)]

      k_train_x = x[train_idx, ]
      k_train_y = y[train_idx]
      k_test_x = x[test_idx, ]
      k_test_y = y[test_idx]

      K_sum = get_K_sum(k_train_x, k_train_y, k_test_x, k_test_y,
                        K_sum, train_fun,
                        knn_k,
                        rf_ntree, rf_mtry,
                        svm_kernal, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree,
                        dev)

      K_num = K_num + length(k_test_y)

      if (dev && k %% 5 == 0) {
         print(K_sum[[1]] / K_num)
      }

      if (time_limit > 0 &&
          as.numeric(Sys.time() - method_start_time, units = "hours") > time_limit) {
         break
      }
   }

   K_mean = K_sum[[1]] / K_num
   K_sd = sqrt((K_sum[[2]] - (((K_sum[[1]]) ^ 2) / K_num)) / (K_num - 1))

   if (length(dim(K_mean)) == 1 && plot) {
      plot_names = as.numeric(names(K_mean))
      plot(x = plot_names, y = K_mean, type = "l")
      lines(x = plot_names, y = K_mean - (2 * K_sd), col = "gray")
      lines(x = plot_names, y = K_mean + (2 * K_sd), col = "gray")
   } else if (length(dim(K_mean)) == 2 && plot) {
      plot_names = dimnames(K_mean)
      plot_names[[1]] = as.numeric(plot_names[[1]])
      plot_names[[2]] = as.numeric(plot_names[[2]])
      K_plot = plot_ly() %>% add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean) %>%
         add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean - (2 * K_sd), opacity = 0.7) %>%
         add_surface(x = plot_names[[2]], y = plot_names[[1]], z = K_mean + (2 * K_sd), opacity = 0.7)
      return(list(err = K_mean, sd = K_sd, plot = K_plot))
   }

   return(list(err = K_mean, sd = K_sd))
}


# Helper Functions --------------------------------------------------------
get_temp_sum = function(train_fun,
                        knn_k,
                        rf_ntree, rf_mtry,
                        svm_kernal, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree) {

   if (train_fun == "knn") {
      require(class)

      temp_sum = numeric(length(knn_k))
      names(temp_sum) = knn_k
   } else if (train_fun == "randomForest") {
      require(randomForest)
      if (plot) {require(plotly)}

      if (is.null(rf_mtry)) {
         if (is.factor(y)) {
            rf_mtry = max(floor(ncol(x) / 3), 1)
         } else {
            rf_mtry = floor(sqrt(ncol(x)))
         }
      }

      temp_sum = matrix(0, nrow = length(rf_ntree), ncol = length(rf_mtry),
                        dimnames = list(rf_ntree, rf_mtry))
   } else if (train_fun == "svm") {
      if (is.null(svm_gamma)) {
         svm_gamma = 1 / (ncol(x))
      }

      if (svm_kernal == "linear") {
         temp_sum = numeric(length(svm_nu))
         names(temp_sum) = svm_nu
      } else if (svm_kernal == "radial") {
         temp_sum = matrix(0, nrow = length(svm_nu), ncol = length(svm_gamma),
                           dimnames = list(svm_nu, svm_gamma))
      } else if (svm_kernal == "sigmoid") {
         temp_sum = array(0, dim = c(length(svm_nu), length(svm_gamma), length(svm_coef0)),
                          dimnames = list(svm_nu, svm_gamma, svm_coef0))
      } else if (svm_kernal == "polynomial") {
         temp_sum = array(0, dim = c(length(svm_nu), length(svm_gamma), length(svm_coef0), length(svm_degree)),
                          dimnames = list(svm_nu, svm_gamma, svm_coef0, svm_degree))
      } else {
         stop("No valid Kernal given for SVM")
      }
   } else {
      temp_sum = NULL

      stop("No Training Method Given")
   }

   return(temp_sum)
}

get_K_sum = function(k_train_x, k_train_y, k_test_x, k_test_y,
                     K_sum, train_fun,
                     knn_k,
                     rf_ntree, rf_mtry,
                     svm_kernal, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree,
                     dev) {

   if (train_fun == "knn") {
      K_sum = err_knn(k_train_x, k_train_y, k_test_x, k_test_y,
                      K_sum, knn_k, dev)
   } else if (train_fun == "randomForest") {
      K_sum = err_rf(k_train_x, k_train_y, k_test_x, k_test_y,
                     K_sum, rf_ntree, rf_mtry, dev)
   } else if (train_fun == "svm") {
      if (svm_kernal == "linear") {
         K_sum = err_svm_lin(k_train_x, k_train_y, k_test_x, k_test_y,
                             K_sum, svm_scale, svm_nu, dev)
      } else if (svm_kernal == "radial") {
         K_sum = err_svm_rad(k_train_x, k_train_y, k_test_x, k_test_y,
                             K_sum, svm_scale, svm_nu, svm_gamma, dev)
      } else if (svm_kernal == "sigmoid") {
         K_sum = err_svm_sig(k_train_x, k_train_y, k_test_x, k_test_y,
                             K_sum, svm_scale, svm_nu, svm_gamma, svm_coef0, dev)
      } else if (svm_kernal == "polynomial") {
         K_sum = err_svm_poly(k_train_x, k_train_y, k_test_x, k_test_y,
                              K_sum, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree, dev)
      }
   }

   return(K_sum)
}
