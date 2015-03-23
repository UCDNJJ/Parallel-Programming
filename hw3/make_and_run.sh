#/usr/local/cuda-5.5/bin/nvcc SmoothC.cu main.cu -g -G -o SmoothC.out && ./SmoothC.out
/usr/local/cuda-5.5/bin/nvcc SmoothC.cu -g -G -o SmoothC.out && ./SmoothC.out
#nvcc SmoothT.cu -g -G -o SmoothC.out && ./SmoothC.out
cp -f SmoothT.cu SmoothT.cpp
/usr/local/cuda-5.5/bin/nvcc SmoothT.cu -o SmoothC.out && ./SmoothC.out
#g++ -g -O2 -o SmoothT.out SmoothT.cpp main.cpp -fopenmp -lgomp \
g++ -g -O2 -o SmoothT.out SmoothT.cpp -fopenmp -lgomp \
    -DTHRUST_DEVICE_SYSTEM=THRUST_DEVICE_SYSTEM_OMP \
    -I /usr/local/cuda-5.5/targets/x86_64-linux/include && ./SmoothT.out
