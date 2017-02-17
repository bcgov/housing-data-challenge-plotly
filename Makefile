all: install launch

install:
	Rscript -e 'update.packages(ask=F)'
	Rscript -e 'if(!require(devtools)) install.packages("devtools", repos="http://cran.rstudio.com")'
	Rscript -e 'devtools::install()'
  
launch:
	Rscript -e 'bcviz::launch()'
