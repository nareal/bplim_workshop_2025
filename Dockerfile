FROM ghcr.io/rocker-org/cuda:latest
LABEL org.opencontainers.image.authors="Nelson Areal <nareal@eeg.uminho.pt>"

USER root

RUN apt-get update -y \
    && apt-get install -y curl libjpeg-dev libpoppler-cpp-dev opensp

## Install Julia
RUN curl -fsSL https://install.julialang.org | sh -s -- -y

## Fix Julia permissions for jovyan user
RUN mkdir -p /home/jovyan/.julia /home/jovyan/.juliaup \
    && chown -R jovyan:users /home/jovyan/.julia /home/jovyan/.juliaup

## Install ALL CUDA 11.8 runtime libraries via conda (for R torch compatibility)
RUN conda install -y -c nvidia cuda-cudart=11.8 cuda-nvrtc=11.8 libcublas=11.11 libcusparse=11.7 libcusolver=11.4

## Set environment for R torch CUDA support
ENV LD_LIBRARY_PATH=/opt/conda/lib:/opt/conda/lib/python3.12/site-packages/nvidia/nvtx/lib:/opt/conda/lib/python3.12/site-packages/nvidia/cuda_runtime/lib:${LD_LIBRARY_PATH}
ENV TORCH_HOME=/home/jovyan/.local/share/r-torch
ENV CUDA=11.8

## Find R home and set environment variables
RUN R_HOME=$(R RHOME) && \
    echo "LD_LIBRARY_PATH=/opt/conda/lib:/opt/conda/lib/python3.12/site-packages/nvidia/nvtx/lib:/opt/conda/lib/python3.12/site-packages/nvidia/cuda_runtime/lib:\${LD_LIBRARY_PATH}" >> ${R_HOME}/etc/Renviron.site && \
    echo "TORCH_HOME=/home/jovyan/.local/share/r-torch" >> ${R_HOME}/etc/Renviron.site && \
    echo "CUDA=11.8" >> ${R_HOME}/etc/Renviron.site

## Install R torch v0.13.0
RUN R -e 'remotes::install_version("torch", version = "0.13.0")'

## Install other packages
RUN install2.r --error \
    JuliaCall \
    remotes \
    here \
    tidyverse \
    duckdb \
    furrr \
    gt \
    bench \
    Rcpp &> /dev/null

## Install torch CUDA backend as jovyan user
USER jovyan
RUN R -e 'torch::install_torch(type = "cuda")'
USER root
