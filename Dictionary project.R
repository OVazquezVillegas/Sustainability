# Dictionary project - SDG 7

#Packages
library(bibliometrix)
library(textmineR)
library(dplyr)

#1 Selecting sample

#1.1 Importing bibtex file and converting it into dataframe
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/Data analysis/Bib files")
S = convert2df("Bibtex.bib", dbsource = "scopus", format = "bibtex")

#1.2 Generating random sample (100 observations)
sample = S[round(runif(100, min = 0, max = 7150),0),]

#1.3 Saving sample
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/Sustainability-repository")
write.csv(sample, 'sample.csv')


#1.4 Opening sample
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/Sustainability-repository/Analysis")
data = read.csv("output - Copy.csv")
sample = data[round(runif(100, min = 0, max = 1000),0),]
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/Sustainability-repository/Analysis")
write.csv(sample, 'output.csv', row.names = F)
sample = read.csv("output.csv")


#2 Searching terms from the Ramirez thesaurus  ('Search function. R)
#2.1 Converting m into a df
search = as.data.frame(m)
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/Sustainability-repository/Analysis")
write.csv(search, 'search.csv', row.names = F)


#Opening Ramirez thesaurus
setwd("C:/Users/6674828/OneDrive - Universiteit Utrecht/SDG-UU/keywords")
thesaurus <- read.csv("keywords_ramirez.csv")

# Filtering required SDGs
sdg_7 = filter(thesaurus, Goal == "7")

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


dtm_2 <- CreateDtm(doc_vec = SDG_terms$V1, 
                ngram_window = c(2, 2),
                stopword_vec = stopwords::stopwords("en"),
                lower = TRUE, 
                remove_punctuation = TRUE, 
                remove_numbers = TRUE, 
                verbose = FALSE, 
                cpus = 2) 

dtm_3 <- CreateDtm(doc_vec = SDG_terms$V1, 
                   ngram_window = c(2, 3),
                   stopword_vec = stopwords::stopwords("en"),
                   lower = TRUE, 
                   remove_punctuation = TRUE, 
                   remove_numbers = TRUE, 
                   verbose = FALSE, 
                   cpus = 2) 


tf <- TermDocFreq(dtm = dtm)
tf_2 <- TermDocFreq(dtm = dtm_2)
tf_3 <- TermDocFreq(dtm = dtm_3)
