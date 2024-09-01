FROM castarter_base
COPY renv.lock renv.lock
RUN R -e 'options(renv.config.pak.enabled = FALSE);renv::restore()'
COPY castarter_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 3838