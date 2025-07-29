FROM rstudio/plumber

WORKDIR /app

RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev pandoc

RUN R -e "install.packages(c('tidyverse', 'tidymodels', 'ranger', 'plumber'))"

COPY myAPI.R /app/

COPY diabetes_binary_health_indicators_BRFSS2015.csv /app/

EXPOSE 8000

ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('myAPI.R'); pr$run(host='0.0.0.0', port=8000)"]