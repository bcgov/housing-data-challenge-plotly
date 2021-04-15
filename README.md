[![img](https://img.shields.io/badge/Lifecycle-Retired-d45500)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

---

# BC Housing Data Challenge



## Running the app

If you have a working [docker environment](https://docs.docker.com/engine/):

```shell
git clone https://github.com/cpsievert/simple-R-shiny.git
cd simple-R-shiny
./dev.sh
```

The application is then available at <http://0.0.0.0:3838/>. You can also run the app from dockerhub with: `docker run -p 3838:3838 cpsievert/simple-r-shiny`

Otherwise, make sure you have [R](https://cran.r-project.org/) installed, then from your terminal:

```shell
git clone https://github.com/cpsievert/housing-data-challenge-plotly.git
cd housing-data-challenge-plotly
make
```

(**Note:** The `make` command assumes you already have a number of non-standard system libraries. Most, if not all of them, are listed in the `before_install` step [here](https://github.com/edzer/sfr/blob/master/.travis.yml))

## Using the app

Click on the image below to a full demo of the application:

<a href="https://vimeo.com/207379729">
  <img src="http://i.imgur.com/HSW3ruY.gif" />
</a>
