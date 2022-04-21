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

echo ">>>>>>>> RUNNING LINT"
Rscript -e "gDRstyle::lintPkgDirs('/mnt/vol')"

echo ">>>>> RUNNING UNIT TESTS"
Rscript -e "testthat::test_local(path = '/mnt/vol', stop_on_failure = TRUE) "

echo ">>>>> RUNNING CHECK"
R CMD build /mnt/vol &&
    R CMD check gDRimport_*.tar.gz --no-vignettes --no-examples --no-manual

echo ">>>>>>>> RUNNING CHECK DEPENDENCIES"
Rscript -e "gDRstyle::checkDependencies(desc_path='/mnt/vol/DESCRIPTION', dep_path='/mnt/vol/rplatform/dependencies.yaml')"
