#Imports needs to define 


setwd("D:/MS-Pinar-GAM")

require(data.table)
require(mgcv)
require(randomForest)
require(tidyverse)
require(mlr)  
require(knitr)
require(R.utils)
require(party)

source("import_all.r")

datasets= c("Adiac",
"CricketX",
"CricketY",
"CricketZ",
"DistalPhalanxTW",
"ECGFiveDays",
"ElectricDevices",
"FaceAll",
"FacesUCR",
"FiftyWords",
"Fish",
"FordA",
"FordB",
"HandOutlines",
"Haptics",
"InlineSkate",
"LargeKitchenAppliances",
"Lightning2",
"Lightning7",
"Mallat",
"Meat",
"MedicalImages",
"MiddlePhalanxTW",
"NonInvasiveFatalECGThorax1",
"NonInvasiveFatalECGThorax2",
"OSULeaf",
"Phoneme",
"Plane",
"ProximalPhalanxTW",
"RefrigerationDevices",
"ScreenType",
"SmallKitchenAppliances",
"Strawberry",
"SwedishLeaf",
"Symbols",
"SyntheticControl",
"Trace",
"TwoPatterns",
"UWaveGestureLibraryAll",
"UWaveGestureLibraryX",
"UWaveGestureLibraryY",
"UWaveGestureLibraryZ",
"Wafer",
"WordSynonyms",
"Worms"
)

for(dataset in datasets){
  
  data_dict = import_data(dataset)
  train_data = data_dict$train_data
  test_data = data_dict$test_data
  train_labels = data_dict$train_labels
  test_labels = data_dict$test_labels
  
  train_dict = prepare_data(train_data, train_labels)
  test_dict = prepare_data(test_data, test_labels)
  
  train_datam = train_dict$data
  
  train_classes = train_dict$class
  
  
  test_datam = test_dict$data
  
  test_classes = test_dict$class
  
  start_time <- Sys.time()
  
  shapelets <- generate_shapelets(train_datam, train_classes)
  
  shapelet_end_time <- Sys.time()
  
  shapelet_run_time = shapelet_end_time - start_time
  
  location = TRUE
  
  tr_start_time <- Sys.time()
  train_features <- generate_features(train_data,shapelets, location)
  tr_end_time <- Sys.time()
  
  
  tt_start_time <- Sys.time()
  test_features <- generate_features(test_data,shapelets, location)
  tt_end_time <- Sys.time()
  
  
  result <- classification(train_features, train_labels, test_features, test_labels)
    
  
  end_time <- Sys.time()
  
  result <- c(result, dataset)
  result <- c(result, nrow(train_data))
  result <- c(result, nrow(test_data))
  result <- c(result, length(unique(train_classes)))
  result <- c(result, length(shapelets))
  result <- c(result, difftime(shapelet_end_time, start_time, units = "secs"))
  result <- c(result, difftime(tr_end_time, tr_start_time, units = "secs"))
  result <- c(result, difftime(tt_end_time, tt_start_time, units = "secs"))
  result <- c(result, difftime(end_time, start_time, units = "secs"))
  
  
  create_output_file(result, "GAMresults.txt")
  
}
