#! /bin/bash -x

# for install convert_mpas
echo ""
echo -e  "${GREEN}==>${NC} Moduling environment for convert_mpas...\n"
module purge
module load gnu9/9.4.0
module load ohpc
module load phdf5
module load netcdf
module load netcdf-fortran

# for cdo, if you use in test/functional
# module load phdf5
#module load cdo-2.0.4-gcc-9.4.0-bjulvnd
module list
