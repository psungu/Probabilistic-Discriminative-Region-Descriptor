
create_output_file <- function(x, file = filepath, append = TRUE, quote = TRUE, sep=",", 
                      eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE, 
                      qmethod = c("escape", "double"), fileEncoding = ""){
  write.table(as.matrix(t(x)), file, append , quote, sep, 
              eol, na, dec, row.names, col.names, 
              qmethod, fileEncoding)
}


header <- c("Training Accuracy", "Test Accuracy", "OOB", "DataSet","LenTrain", "LenTest","ClassCount","ShapeletCount","Shapelet Running Time","Train Features Extraction Time","Test Features Extraction Time","Running Time")

create_output_file(header, "GAMresults.txt")
