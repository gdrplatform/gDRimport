#/bin/sh

echo "Executing $0"
echo "Environment: ${rp_env}"
echo "Working directory: `pwd`"
echo "Working directory contains: `ls | tr '\n' ' '`"

# exit when any command fails
set -e

CORE_DIR=/mnt/vol
echo ">>>>> OWNER OF THE REPOSITORY"
# Fix for the access permissions issues with 'R CMD' when running Docker image with 'USER 1000'
sudo chown -R `id -u` $CORE_DIR

echo ">>>>>>>> RUNNING CHECK"
Rscript -e "gDRstyle::checkPackage('gDRutils' , '/mnt/vol', FALSE)"
