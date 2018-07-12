# K-Nearest Neighbors -----------------------------------------------------
err_knn = function(k_train_x, k_train_y, k_test_x, k_test_y,
                   K_sum, knn_k, dev) {
   require(class)
   
   for (i in 1:length(knn_k)) {
      if (dev) {start_time = Sys.time(); print(paste(i, "k")); print(start_time)}
      
      ijk_pred = knn(train = k_train_x, test = k_test_x, cl = k_train_y,
                     k = knn_k[i])
      
      if (is.factor(k_train_y)) {
         temp_err = 1 * (k_test_y != ijk_pred) # Classification Error
      } else {
         temp_err = (k_test_y - ijk_pred)^2 # Regression Error
      }
      
      K_sum[[1]][i] = K_sum[[1]][i] + sum(temp_err)
      K_sum[[2]][i] = K_sum[[2]][i] + sum(temp_err ^ 2)
         
      if (dev) {print(Sys.time() - start_time); cat("\n")}
   }
   
   return(K_sum)
}


# Random Forest -----------------------------------------------------------
err_rf = function(k_train_x, k_train_y, k_test_x, k_test_y,
                  K_sum, rf_ntree, rf_mtry, dev) {
   require(randomForest)
   
   for (i in 1:length(rf_ntree)) {
      for (j in 1:length(rf_mtry)) {
         if (dev) {start_time = Sys.time(); print(paste(i, "ntree,", j, "mtry")); print(start_time)}
         
         ijk_model = randomForest(x = k_train_x, y = k_train_y,
                                  ntree = rf_ntree[i], mtry = rf_mtry[j])
         ijk_pred = predict(ijk_model, k_test_x)
         
         if (is.factor(k_train_y)) {
            temp_err = 1 * (k_test_y != ijk_pred) # Classification Error
         } else {
            temp_err = (k_test_y - ijk_pred)^2 # Regression Error
         }
         
         K_sum[[1]][i, j] = K_sum[[1]][i, j] + sum(temp_err)
         K_sum[[2]][i, j] = K_sum[[2]][i, j] + sum(temp_err ^ 2)
         
         if (dev) {print(Sys.time() - start_time); cat("\n")}
      }
   }
   
   return(K_sum)
}

# Linear (nu) SVM ---------------------------------------------------------
err_svm_lin = function(k_train_x, k_train_y, k_test_x, k_test_y,
                       K_sum, svm_scale, svm_nu, dev) {
   require(e1071)
   
   for (i in 1:length(svm_nu)) {
      if (dev) {start_time = Sys.time(); print(paste(i, "nu")); print(start_time)}
      
      if (is.factor(k_train_y)) {
         ijk_model = svm(x = k_train_x, y = k_train_y, scale = svm_scale,
                         type = "nu-classification", kernal = "linear",
                         nu = svm_nu[i])
      } else {
         ijk_model = svm(x = k_train_x, y = k_train_y, scale = svm_scale,
                         type = "nu-regression", kernal = "linear",
                         nu = svm_nu[i])
      }
      
      ijk_pred = predict(ijk_model, k_test_x)
      
      if (is.factor(k_train_y)) {
         temp_err = 1 * (k_test_y != ijk_pred) # Classification Error
      } else {
         temp_err = (k_test_y - ijk_pred)^2 # Regression Error
      }
      
      K_sum[[1]][i] = K_sum[[1]][i] + sum(temp_err)
      K_sum[[2]][i] = K_sum[[2]][i] + sum(temp_err ^ 2)
      
      if (dev) {print(Sys.time() - start_time); cat("\n")}
   }
   
   return(K_sum)
}

# Radial (nu) SVM ---------------------------------------------------------
err_svm_rad = function(k_train_x, k_train_y, k_test_x, k_test_y,
                       K_sum, svm_scale, svm_nu, svm_gamma, dev) {
   require(e1071)
   
   for (i in 1:length(svm_nu)) {
      for (j in 1:length(svm_gamma)) {
         if (dev) {start_time = Sys.time(); print(paste(i, "nu,", j, "gamma,", p, "coef0")); print(start_time)}
         
         if (is.factor(k_train_y)) {
            ijk_model = svm(x = k_train_x, y = k_train_y, scale = svm_scale,
                            type = "nu-classification", kernal = "radial",
                            nu = svm_nu[i], gamma = svm_gamma[j])
         } else {
            ijk_model = svm(x = k_train_x, y = k_train_y, scale = svm_scale,
                            type = "nu-regression", kernal = "radial",
                            nu = svm_nu[i], gamma = svm_gamma[j])
         }
         
         ijk_pred = predict(ijk_model, k_test_x)
         
         if (is.factor(k_train_y)) {
            temp_err = 1 * (k_test_y != ijk_pred) # Classification Error
         } else {
            temp_err = (k_test_y - ijk_pred)^2 # Regression Error
         }
         
         K_sum[[1]][i, j] = K_sum[[1]][i, j] + sum(temp_err)
         K_sum[[2]][i, j] = K_sum[[2]][i, j] + sum(temp_err ^ 2)
         
         if (dev) {print(Sys.time() - start_time); cat("\n")}
      }
   }
   
   return(K_sum)
}

# Sigmoidal (nu) SVM ------------------------------------------------------
err_svm_sig = function(k_train_x, k_train_y, k_test_x, k_test_y,
                       K_sum, svm_scale, svm_nu, svm_gamma, svm_coef0, dev) {
   require(e1071)
   
   for (i in 1:length(svm_nu)) {
      for (j in 1:length(svm_gamma)) {
         for (p in 1:length(svm_coef0)) {
            if (dev) {start_time = Sys.time(); print(paste(i, "nu,", j, "gamma,", p, "coef0")); print(start_time)}
            
            if (is.factor(k_train_y)) {
               ijk_model = svm(x = k_train_x, y = k_train_y, scale = svm_scale,
                               type = "nu-classification", kernal = "sigmoid",
                               nu = svm_nu[i], gamma = svm_gamma[j],
                               coef0 = svm_coef0[p])
            } else {
               ijk_model = svm(x = k_train_x, y = k_train_y, scale = svm_scale,
                               type = "nu-regression", kernal = "sigmoid",
                               nu = svm_nu[i], gamma = svm_gamma[j],
                               coef0 = svm_coef0[p])
            }
            
            ijk_pred = predict(ijk_model, k_test_x)
            
            if (is.factor(k_train_y)) {
               temp_err = 1 * (k_test_y != ijk_pred) # Classification Error
            } else {
               temp_err = (k_test_y - ijk_pred)^2 # Regression Error
            }
            
            K_sum[[1]][i, j, p] = K_sum[[1]][i, j, p] + sum(temp_err)
            K_sum[[2]][i, j, p] = K_sum[[2]][i, j, p] + sum(temp_err ^ 2)
            
            if (dev) {print(Sys.time() - start_time); cat("\n")}
         }
      }
   }
   
   return(K_sum)
}

# Polynomial (nu) SVM -----------------------------------------------------
err_svm_poly = function(k_train_x, k_train_y, k_test_x, k_test_y,
                        K_sum, svm_scale, svm_nu, svm_gamma, svm_coef0, svm_degree, dev) {
   require(e1071)
   
   for (i in 1:length(svm_nu)) {
      for (j in 1:length(svm_gamma)) {
         for (p in 1:length(svm_coef0)) {
            for (q in 1:length(svm_degree)) {
               if (dev) {start_time = Sys.time(); print(paste(i, "nu,", j, "gamma,", p, "coef0,", q, "degree")); print(start_time)}
               
               if (is.factor(k_train_y)) {
                  ijk_model = svm(x = k_train_x, y = k_train_y, scale = svm_scale,
                                  type = "nu-classification", kernal = "polynomial",
                                  nu = svm_nu[i], gamma = svm_gamma[j],
                                  coef0 = svm_coef0[p], degree = svm_degree[q])
               } else {
                  ijk_model = svm(x = k_train_x, y = k_train_y, scale = svm_scale,
                                  type = "nu-regression", kernal = "polynomial",
                                  nu = svm_nu[i], gamma = svm_gamma[j],
                                  coef0 = svm_coef0[p], degree = svm_degree[q])
               }
               
               ijk_pred = predict(ijk_model, k_test_x)
               
               if (is.factor(k_train_y)) {
                  temp_err = 1 * (k_test_y != ijk_pred) # Classification Error
               } else {
                  temp_err = (k_test_y - ijk_pred)^2 # Regression Error
               }
               
               K_sum[[1]][i, j, p, q] = K_sum[[1]][i, j, p, q] + sum(temp_err)
               K_sum[[2]][i, j, p, q] = K_sum[[2]][i, j, p, q] + sum(temp_err ^ 2)
               
               if (dev) {print(Sys.time() - start_time); cat("\n")}
            }
         }
      }
   }
   
   return(K_sum)
}