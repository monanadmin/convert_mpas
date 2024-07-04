

cd ../..
. ./load_module_convert_mpas.bash
make clean
make 
cd test/functional

# command to extract only necessary variabels for init file
# cdo select,name=zgrid,lonEdge,latEdge,lonVertex,latVertex,lonCell,latCell,cellsOnEdge,edgesOnCell,cellsOnVertex,
# verticesOnCell,cellsOnCell,nEdgesOnCellx1.1024002.init_ORIGINAL.nc ./x1.1024002.init.nc



# TEST 1 - fix for invalid LAT and LON values in output file (Issue #)
# target domain X 0 - 360 , Y -90, 90
# should set correct domain in output file

# init file contains only necessary data for the test
# zgrid,lonEdge,latEdge,lonVertex,latVertex,lonCell,latCell,cellsOnEdge,edgesOnCell,cellsOnVertex,verticesOnCell,cellsOnCell,nEdgesOnCell
input_init_file='./x1.1024002.init.nc'
# contains only t2m
input_file=./MONAN_DIAG_G_MOD_GFS_2024042700_2024042701.00.00.x1024002L55.nc

rm -f ./latlon.nc
time ../../src/convert_mpas $input_init_file $input_file




# to check in grads manually
# module load opengrads-2.2.1
# grads -lc "sdfopen latlon.nc"

# must display 
# LON set to 0 360 
# LAT set to -90 90 