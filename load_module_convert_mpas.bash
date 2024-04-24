#! /bin/bash -x
# install convert_mpas
echo ""
echo -e  "${GREEN}==>${NC} Moduling environment for convert_mpas...\n"
module purge
module load gnu9/9.4.0
module load ohpc
module load phdf5
module load netcdf
module load netcdf-fortran
module list
