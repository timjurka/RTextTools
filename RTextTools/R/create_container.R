create_container <- function(matrix,labels,trainSize=NULL,testSize=NULL,virgin) {
	if (is.null(trainSize) && is.null(testSize)) stop("You must specify either a trainSize or testSize parameter, or both.")
	if (is.null(trainSize)) trainSize <- testSize
	if (is.null(testSize)) testSize <- trainSize

	totalSize <- sort(unique(append(trainSize,testSize)))
	column_names <- colnames(matrix)
	data_matrix <- as.compressed.matrix(matrix[totalSize, ])
	
	matrix_train_predict <- data_matrix[trainSize,]
	matrix_test_predict <- data_matrix[testSize,]

    train_code <- as.factor(labels[trainSize])
	if (length(unique(is.na(train_code))) > 1) stop("All data in the training set must have corresponding codes.")
	
    test_code <- as.factor(labels[testSize])
	if (virgin == FALSE && length(unique(is.na(test_code))) > 1) stop("The data to be classified does not have corresponding codes. To treat this data set as virgin data, set virgin=TRUE.")
	
    container <- new("matrix_container", training_matrix=matrix_train_predict, classification_matrix=matrix_test_predict, training_codes=train_code, testing_codes=test_code, column_names=column_names, virgin=virgin)
    
    gc()
    return(container)
}
