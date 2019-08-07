## parrot

Parrot scales short text around common words.

Let me know if you run into any problems.

Paper: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3044864

The scale_text function suggests a truncation for the SVD. It produces approximately the same number of pivot words.

## Example



```{r example}

## ## packages installed in install_github(, dependencies=TRUE)
## install.packages(c("stm","ggplot2","gridExtra","Matrix","reshape2","ForeCA","devtools","magrittr","RSpectra"))
## ## word embeddings
## install.packages(c("dplyr","readr","tidyr","CCA"))
## ## optional (not installed)
## install.packages(c("knitr"))

library(devtools)
install_github("wilryh/parrot", dependencies=TRUE)

library(stm)
library(parrot)

processed <- textProcessor(
    input_data$text,
    data.frame(input_data),
    removestopwords=T, lowercase=T, stem=F
    )
out <- prepDocuments(
    processed$documents, processed$vocab, processed$meta
    )

tdm <- doc_to_tdm(out)

embeddings <- read_word_embeddings(
    in_vocab=out$vocab,
    ovefile = "O2M_overlap.txt" # must add location on your computer "path/to/O2M_overlap.txt"
    ## ovefile2 = "path/to/O2M_oov.txt", # very rare words and misspellings
    ## available here http://www.cis.uni-muenchen.de/~wenpeng/renamed-meta-emb.tar.gz
    ## must unpack and replace "path/to/" with location on your computer
    )

scores <- scale_text(
    meta=out$meta,
    tdm=tdm
##    embeddings=embeddings[["meta"]], ## embeddings have little effect
##    on output -- if used, consider setting pivot lower (e.g. pivot = 1/2)
    )

document_scores <- score_documents(
    scores=scores, n_dimensions=10
    )

get_keywords(scores, n_dimensions=3, n_words=15)

with(document_scores, cor(sqrt(n_words), X0, use="complete"))

plot_keywords(
    scores, x_dimension=1, y_dimension=2, q_cutoff=0.9
    )
```
