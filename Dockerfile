FROM arkadiuszgladki/gdr_shiny:0.08

# temporary fix
# GitHub token for downloading private dependencies
ARG GITHUB_TOKEN

#================= Install dependencies
RUN mkdir -p /mnt/vol
COPY rplatform/dependencies.yaml rplatform/.github_access_token.txt* /mnt/vol
RUN Rscript -e "gDRgenesis::installAllDeps()"

#================= Check & build package
COPY ./ /tmp/gDRimport/
RUN Rscript -e "gDRgenesis::installLocalPackage('/tmp/gDRimport')" 

#================= Clean up
RUN sudo rm -rf /mnt/vol/* /tmp/gDRimport/
