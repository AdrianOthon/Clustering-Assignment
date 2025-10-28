FROM --platform=linux/amd64 rocker/verse

# Install R packages
RUN install2.r --error \
    tidyverse \
    ggplot2 \
    cluster \
    plotly \
    htmlwidgets \
    tidytext \
    stopwords \
    textdata \
    rmarkdown 

# Install LaTeX (for knitting PDFs)
RUN R -e "tinytex::install_tinytex(force = TRUE)"

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /home/rstudio/project
