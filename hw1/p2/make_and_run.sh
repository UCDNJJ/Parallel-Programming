mkdir -p build
cd build
cmake -DCMAKE_BUILD_TYPE=Debug .. && make 
#../random_gen.sh > test_ran.txt
env OMP_NUM_THREADS=4 ./Problem2 < test.txt

