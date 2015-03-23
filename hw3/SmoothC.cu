#include <cuda.h>
#include <cstdio>
#include <cmath>
#include <algorithm>

/*#define STANDALONE*/
/*#define TEST*/
/*#define PRINT*/

#ifdef STANDALONE
#include <sys/time.h>

#define MICRO_IN_SEC 1000000

typedef unsigned long long timestamp_t;

timestamp_t get_timestamp()
{
    struct timeval now;
    gettimeofday(&now, NULL);
    return now.tv_usec + (timestamp_t)(now.tv_sec) * MICRO_IN_SEC;
}

void checkpoint(timestamp_t* marker, const char* message)
{
    timestamp_t now = get_timestamp();
    printf("# %20f - %s\n", (now-*marker) / (double)MICRO_IN_SEC, message);
    *marker = now;
}
#endif

__global__ void sum_and_count(float *i_x, int i_n, float *j_x, float *j_y, int j_n,
        float h, float *i_sum, int *i_count) {
    int me_i = blockIdx.x * blockDim.x + threadIdx.x;

    float my_sum = 0;
    int my_count = 0;
    
    if(me_i < i_n) {
        for(int j = 0; j < j_n; j++) {
            if(abs(j_x[j] - i_x[me_i]) < h) {
#ifdef PRINT
                /*printf("%d, %d, value: %f\n", me_i, j, j_y[j]);*/
#endif
                my_sum += j_y[j];
                my_count += 1;
            }
        }
    }

    i_sum[me_i] = my_sum;
    i_count[me_i] = my_count;
}

__global__ void div(float *m, int *count, int n) {
    int me = blockIdx.x * blockDim.x + threadIdx.x;
    if(me < n) {
#ifdef PRINT
        /*printf("mean: %f, %d\n", m[me], count[me]);*/
#endif
        m[me] /= count[me];
    }
}

#define CUDA_CHECK(EXP) if(EXP != cudaSuccess) goto cudaError;

void smoothc(float *x, float *y, float *m, int n, float h) {
    float *device_i_x = NULL,
        *device_i_y = NULL,
        *device_j_x = NULL,
        *device_j_y = NULL,
        *device_i_m = NULL;

    int *host_count,
        *device_i_count;

    int int_byte_count = n * sizeof(int);

    host_count = (int*)malloc(int_byte_count);

    for(int i = 0; i < n; i++) {
        m[i] = 0;
        host_count[i] = 0;
    }

    int chunk_size = 100000;
    int chunk_float_byte_count = chunk_size * sizeof(float);
    int chunk_int_byte_count = chunk_size * sizeof(int);
    int chunk_count = ceil(n / (double)chunk_size);
#ifdef PRINT
    printf("%d\n", chunk_count);
#endif

    int dim_block = 192;
    int dim_grid = ceil(chunk_size / (double)dim_block);
#ifdef PRINT
    printf("Conf: %d %d\n", dim_block, dim_grid);
#endif

    CUDA_CHECK(cudaMalloc((void **)&device_i_x,     chunk_float_byte_count));
    CUDA_CHECK(cudaMalloc((void **)&device_j_x,     chunk_float_byte_count));
    CUDA_CHECK(cudaMalloc((void **)&device_j_y,     chunk_float_byte_count));
    CUDA_CHECK(cudaMalloc((void **)&device_i_count, chunk_int_byte_count));
    CUDA_CHECK(cudaMalloc((void **)&device_i_m,     chunk_float_byte_count));

    for(int i = 0; i < chunk_count; i++) {
        for(int j = 0; j < chunk_count; j++) {
            int i_remaining_count = std::min(chunk_size, n - i*chunk_size);
            int j_remaining_count = std::min(chunk_size, n - j*chunk_size);

            CUDA_CHECK(cudaMemcpy(device_i_x, x + i*chunk_size,
                        i_remaining_count*sizeof(float), cudaMemcpyHostToDevice));

            CUDA_CHECK(cudaMemcpy(device_j_x, x + j*chunk_size,
                        j_remaining_count*sizeof(float), cudaMemcpyHostToDevice));
            CUDA_CHECK(cudaMemcpy(device_j_y, y + j*chunk_size,
                        j_remaining_count*sizeof(float), cudaMemcpyHostToDevice));

            CUDA_CHECK(cudaMemcpy(device_i_m, m + i*chunk_size,
                        i_remaining_count*sizeof(float), cudaMemcpyHostToDevice));
            CUDA_CHECK(cudaMemcpy(device_i_count, host_count + i*chunk_size,
                        i_remaining_count*sizeof(int), cudaMemcpyHostToDevice));

            sum_and_count<<<dim_grid, dim_block>>>(device_i_x, 
                    i_remaining_count,
                    device_j_x, device_j_y,
                    j_remaining_count,
                    h, device_i_m, device_i_count);

            CUDA_CHECK(cudaMemcpy(m + i*chunk_size, device_i_m,
                        i_remaining_count*sizeof(float), cudaMemcpyDeviceToHost));
            CUDA_CHECK(cudaMemcpy(host_count + i*chunk_size, device_i_count,
                        i_remaining_count*sizeof(float), cudaMemcpyDeviceToHost));
        }
    }
    for(int i = 0; i < chunk_count; i++) {
        int i_remaining_count = std::min(chunk_size, n - i*chunk_size);
        CUDA_CHECK(cudaMemcpy(device_i_m, m + i*chunk_size,
                    i_remaining_count*sizeof(float), cudaMemcpyHostToDevice));
        CUDA_CHECK(cudaMemcpy(device_i_count, host_count + i*chunk_size,
                    i_remaining_count*sizeof(int), cudaMemcpyHostToDevice));

        div<<<dim_grid, dim_block>>>(device_i_m, device_i_count,
                i_remaining_count);
        
        CUDA_CHECK(cudaMemcpy(m + i*chunk_size, device_i_m,
                    i_remaining_count*sizeof(float), cudaMemcpyDeviceToHost));
    }

cudaError:
    cudaError_t error = cudaGetLastError();
    if(error != cudaSuccess) {
        printf("CUDA Error: %s\n", cudaGetErrorString(error));
    }
    free(host_count);
    cudaFree(device_i_x);
    cudaFree(device_i_y);
    cudaFree(device_i_count);
    cudaFree(device_i_m);
}

#ifdef STANDALONE
int main(int argc, char **argv) {
    int element_count;
    int h;
#ifdef TEST
    h = 2;
    element_count = 10;
#else
    h = 100000;
    FILE* fp = fopen("test.txt", "r");
    fscanf(fp, "%d\n", &element_count);
    element_count = 100000;
#endif
    int float_byte_count = element_count * sizeof(float);
    float *host_x = (float*)malloc(float_byte_count);
    float *host_y = (float*)malloc(float_byte_count);
    float *host_m = (float*)malloc(float_byte_count);
#ifdef TEST
    for(int i = 0; i < element_count; i++) {
        host_x[i] = i;
        host_y[i] = i;
    }
#else
    for(int i = 0; i < element_count; i++) {
        fscanf(fp, "%f\n", host_x+i);
        host_y[i] = host_x[i] + host_x[0];
    }
#endif
    timestamp_t marker = get_timestamp();
    smoothc(host_x, host_y, host_m, element_count, h);
    checkpoint(&marker, "TOTAL");
#ifdef PRINT
    for(int i = 0; i < element_count; i++) {
        printf("(%f, %f, %f)\n", host_x[i], host_y[i], host_m[i]);
    }
#endif
    return 0;
}
#endif
