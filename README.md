# BC Housing Data Challenge



## Running the app

If you have a working [docker environment](https://docs.docker.com/engine/):

```shell
git clone https://github.com/cpsievert/simple-R-shiny.git
cd simple-R-shiny
./dev.sh
```

Otherwise, make sure you have [R](https://cran.r-project.org/) installed, then from your terminal:

```shell
git clone https://github.com/cpsievert/housing-data-challenge-plotly.git
cd housing-data-challenge-plotly
make
```

(**Note:** The `make` command assumes you already have a number of non-standard system libraries. Most, if not all of them, are listed in the `before_install` step [here](https://github.com/edzer/sfr/blob/master/.travis.yml))

