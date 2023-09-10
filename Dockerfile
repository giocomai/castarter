FROM rocker/shiny:4.3.1
RUN apt-get update && apt-get install -y  git-core libcairo2-dev libcurl4-openssl-dev libgit2-dev libicu-dev libpng-dev libssl-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("cli",upgrade="never", version = "3.6.1")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("ellipsis",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.6")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.8.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.24")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.43")'
RUN Rscript -e 'remotes::install_version("xml2",upgrade="never", version = "1.3.5")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.5")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("roxygen2",upgrade="never", version = "7.2.3")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.10")'
RUN Rscript -e 'remotes::install_version("PrettyCols",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.3.3")'
RUN Rscript -e 'remotes::install_version("rvest",upgrade="never", version = "1.0.3")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.3.1")'
RUN Rscript -e 'remotes::install_version("usethis",upgrade="never", version = "2.2.2")'
RUN Rscript -e 'remotes::install_version("cicerone",upgrade="never", version = "1.0.4")'
RUN Rscript -e 'remotes::install_version("tbl2xts",upgrade="never", version = "1.0.4")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.2")'
RUN Rscript -e 'remotes::install_version("ggiraph",upgrade="never", version = "0.8.7")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")'
RUN Rscript -e 'remotes::install_version("dygraphs",upgrade="never", version = "1.1.1.6")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.29")'
RUN Rscript -e 'remotes::install_version("tidytext",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("slider",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'source("https://raw.githubusercontent.com/apache/arrow/main/r/R/install-arrow.R");install_arrow()'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838