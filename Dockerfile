# Includes R 3.5.0, src build tools, rstudio, tidyverse & devtools and tex and publishing-related pacakages. R 3.5.0 was released on April 23
FROM rocker/tidyverse:3.4.4
# Installing texlive though apt-get cause I was having trouble using TinyTex
RUN apt-get update \
  && apt-get -y --no-install-recommends install texlive-full
# R dependences are installed from MRAN repo snapshotted on 2018-06-01, one day before 3.5.1 and a month before they did something weird to MuMIN
RUN R -e "install.packages('drake', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('bipartite', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('exp', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('cowplot', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('MuMIn', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('rlist', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('AICcmodavg', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('corpcor', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('foreach', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('doMC', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('latex2exp', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('bookdown', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"
RUN R -e "install.packages('kableExtra', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-30'))"