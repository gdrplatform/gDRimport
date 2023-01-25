ARG BASE_IMAGE=arkadiuszgladki/gdr_shiny:0.09
FROM ${BASE_IMAGE}

# temporary fix
# GitHub token for downloading private dependencies
ARG GITHUB_TOKEN

#================= Install dependencies
RUN mkdir -p /mnt/vol
COPY rplatform/dependencies.yaml rplatform/.github_access_token.txt* /mnt/vol
RUN Rscript -e "gDRstyle::installAllDeps()"

#================= Check & build package
COPY ./ /tmp/gDRimport/
RUN Rscript -e "gDRstyle::installLocalPackage('/tmp/gDRimport')" 

#================= Clean up
RUN sudo rm -rf /mnt/vol/* /tmp/gDRimport/
