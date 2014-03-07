The project layout is as follows:

- bin/ contains scripts meant to be executed
- R/ contains all the R stuff
- doc/ contains documentation
- models/ contains serialized models

To be more specific:

- bin/export-arff.R: exports the preprocessed data frame to arff
- bin/install-deps.R: installs the required R dependencies
- bin/run.R: trains a model; give model(s) as argument(s) to train specific ones

- R/data.R: lazy evaluation for all data frames (dt, dt.c50, dt.cart, ...)
- R/fs.R: feature selection (per model)
- R/pipeline.R: the the model fitting stuff
- R/pp.R: preprocessing and outlier removal
- R/utils.R: utility functions

As far as documentation is concerned:

- doc/doc.Rnw: the final documentation in R/LaTeX
- doc/notes.Rmd: notes taken during data analysis in R/Markdown
