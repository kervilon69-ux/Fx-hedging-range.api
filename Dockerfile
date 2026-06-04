FROM rstudio/plumber

RUN R -e "install.packages('plumber', repos='https://cloud.r-project.org')"

COPY . /app
WORKDIR /app

EXPOSE 8001

ENTRYPOINT ["Rscript", "entrypoint.R"]