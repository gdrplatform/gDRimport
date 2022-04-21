FROM arkadiuszgladki/gdr_shiny:0.08

# temporary fix
# GitHub token for downloading private dependencies
ARG GITHUB_TOKEN

#================= Install dependencies
RUN mkdir -p /mnt/vol
COPY rplatform/dependencies.yaml rplatform/.github_access_token.txt* /mnt/vol
COPY rplatform/install_all_deps.R /mnt/vol/install_all_deps.R
RUN R -f /mnt/vol/install_all_deps.R

#================= Check & build package
COPY ./ /tmp/gDRimport/
COPY rplatform/install_repo.R /mnt/vol
RUN R -f /mnt/vol/install_repo.R 

#================= Clean up
RUN sudo rm -rf /mnt/vol/* /tmp/gDRimport/
