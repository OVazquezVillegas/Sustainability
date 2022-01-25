# Dictionary project - SDG 7

#Packages
library(bibliometrix)
library(textmineR)


#1 Selecting sample

#1.1 Importing bibtex file and converting it into dataframe
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/Data analysis/Bib files")
S = convert2df("Bibtex.bib", dbsource = "scopus", format = "bibtex")

#1.2 Generating random sample (100 observations)
sample = S[round(runif(100, min = 0, max = 7150),0),]

#1.3 Saving sample
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/Sustainability-repository")
write.csv(sample, 'sample.csv')


#2 Extracting terms for SDG 7

#2.1 Importing file with SDG official terminology (goals, targets and indicators)
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/Sustainability-repository")
SDG_terms = read.delim("SDG7_terminology.txt", header=FALSE)

dtm <- CreateDtm(doc_vec = SDG_terms$V1, # character vector of documents
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = stopwords::stopwords("en"), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # by default, this will be the max number of cpus available


dtm_2 <- CreateDtm(doc_vec = SDG_terms$V1, # character vector of documents
                ngram_window = 2, # minimum and maximum n-gram length
                stopword_vec = stopwords::stopwords("en"), # this is the default value
                lower = TRUE, # lowercase - this is the default value
                remove_punctuation = TRUE, # punctuation - this is the default
                remove_numbers = TRUE, # numbers - this is the default
                verbose = FALSE, # Turn off status bar for this demo
                cpus = 2) # by default, this will be the max number of cpus available


tf <- TermDocFreq(dtm = dtm)
vocabulary <- tf$term[ tf$term_freq > 5 ]
