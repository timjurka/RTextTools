create_matrix <- function(textColumns, language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=3, maxWordLength=Inf, ngramLength=1, originalMatrix=NULL, removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE,  stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
	
    stem_words <- function(x) {
        split <- strsplit(x," ")
        return(wordStem(unlist(split),language=language))
    }
    
    tokenize_ngrams <- function(x, n=ngramLength) return(rownames(as.data.frame(unclass(textcnt(x$content,method="string",n=n)))))
	
	control <- list(bounds=list(local=c(minDocFreq,maxDocFreq)),language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stopwords=removeStopwords,stripWhitespace=stripWhitespace,wordLengths=c(minWordLength,maxWordLength),weighting=weighting)
        
    if (ngramLength > 1) { 
    	control <- append(control,list(tokenize=tokenize_ngrams),after=7)
    } else {
    	control <- append(control,list(tokenize=scan_tokenizer),after=4)
    }
    
    if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=7)
    
    trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
    trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")

	corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
	matrix <- DocumentTermMatrix(corpus,control=control);
    if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
	
    if (!is.null(originalMatrix)) {
        terms <- colnames(originalMatrix[,which(!colnames(originalMatrix) %in% colnames(matrix))])

        weight <- 0
        if (attr(originalMatrix,"weighting")[2] =="tf-idf") weight <- 0.000000001
        amat <- matrix(weight,nrow=nrow(matrix),ncol=length(terms))
        colnames(amat) <- terms
        rownames(amat) <- rownames(matrix)

        fixed <- as.DocumentTermMatrix(cbind(matrix[,which(colnames(matrix) %in% colnames(originalMatrix))],amat),weighting=weighting)
        matrix <- fixed
    }

	matrix <- matrix[,sort(colnames(matrix))]

	gc()
	return(matrix)
}
