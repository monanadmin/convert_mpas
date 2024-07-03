
 
cd ../..
. ./load_module_convert_mpas.bash
# make clean
# make 

cd test/unitary

make clean
make

./test_target_mesh


