#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <algorithm>

#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/generate.h>
#include <thrust/sort.h>
#include <thrust/copy.h>
#include <thrust/count.h>
#include <thrust/tuple.h>
#include <thrust/sequence.h>

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

struct SumAndCount {
    float *j_x,
          *j_y;
    int j_n;
    float h;

    SumAndCount(thrust::device_vector<float>::iterator it_x,
            thrust::device_vector<float>::iterator it_y,
            int j_n, float h) : j_n(j_n), h(h) {
        j_x = thrust::raw_pointer_cast(&it_x[0]);
        j_y = thrust::raw_pointer_cast(&it_y[0]);
    }

    __device__ thrust::tuple<float, int> operator()(
            thrust::tuple<float, float, int> ix_sum_count) {
        float ix = thrust::get<0>(ix_sum_count);
        float sum = thrust::get<1>(ix_sum_count);
        int count = thrust::get<2>(ix_sum_count);

        for(int j = 0; j < j_n; j++) {
            if(abs(j_x[j] - ix) < h) {
                sum += j_y[j];
                count += 1;
            }
        }

        return thrust::make_tuple(sum, count);
    }
};

struct Mean {
    __device__ float operator()(const float sum, const float count) {
        return sum / count;
    }
};

void smootht(float *x, float *y, float *m, int n, float h) {
    thrust::host_vector<float> host_x(x, x + n);
    thrust::host_vector<float> host_y(y, y + n);
    thrust::host_vector<float> host_m(m, m + n);
    thrust::host_vector<int> host_count(n);
    thrust::fill(host_count.begin(), host_count.end(), 0);
    thrust::fill(host_m.begin(), host_m.end(), 0);

    int chunk_size = 100000;
    int chunk_count = ceil(n / (double)chunk_size);

    thrust::device_vector<float> device_i_x(chunk_size);
    thrust::device_vector<float> device_j_x(chunk_size);
    thrust::device_vector<float> device_j_y(chunk_size);
    thrust::device_vector<int> device_i_count(chunk_size);
    thrust::device_vector<float> device_i_m(chunk_size);
    thrust::device_vector<int> device_out_count(chunk_size);
    thrust::device_vector<float> device_out_m(chunk_size);
    thrust::device_vector<int> seq(chunk_size);
    thrust::sequence(seq.begin(), seq.end());

    for(int i = 0; i < chunk_count; i++) {
        for(int j = 0; j < chunk_count; j++) {
            int i_remaining_count = std::min(chunk_size, n - i*chunk_size);
            int j_remaining_count = std::min(chunk_size, n - j*chunk_size);

            thrust::copy(host_x.begin() + i*chunk_size,
                    host_x.begin() + i*chunk_size + i_remaining_count,
                    device_i_x.begin());

            thrust::copy(host_x.begin() + j*chunk_size,
                    host_x.begin() + j*chunk_size + j_remaining_count,
                    device_j_x.begin());
            thrust::copy(host_y.begin() + j*chunk_size,
                    host_y.begin() + j*chunk_size + j_remaining_count,
                    device_j_y.begin());

            thrust::copy(host_m.begin() + i*chunk_size,
                    host_m.begin() + i*chunk_size + i_remaining_count,
                    device_i_m.begin());
            thrust::copy(host_count.begin() + i*chunk_size,
                    host_count.begin() + i*chunk_size + i_remaining_count,
                    device_i_count.begin());

            thrust::transform(
                    thrust::make_zip_iterator(thrust::make_tuple(
                            device_i_x.begin(),
                            device_i_m.begin(),
                            device_i_count.begin())),
                    thrust::make_zip_iterator(thrust::make_tuple(
                            device_i_x.begin() + i_remaining_count,
                            device_i_m.begin() + i_remaining_count,
                            device_i_count.begin() + i_remaining_count)),
                    thrust::make_zip_iterator(thrust::make_tuple(
                            device_out_m.begin(), device_out_count.begin())),
                    SumAndCount(device_j_x.begin(), device_j_y.begin(),
                        j_remaining_count, h));

            thrust::copy(device_out_m.begin(),
                    device_out_m.begin() + i_remaining_count,
                    host_m.begin() + i*chunk_size);
            thrust::copy(device_out_count.begin(),
                    device_out_count.begin() + i_remaining_count,
                    host_count.begin() + i*chunk_size);
        }
    }

    for(int i = 0; i < chunk_count; i++) {
        int i_remaining_count = std::min(chunk_size, n - i*chunk_size);

        thrust::copy(host_m.begin() + i*chunk_size,
                host_m.begin() + i*chunk_size + i_remaining_count,
                device_i_m.begin());
        thrust::copy(host_count.begin() + i*chunk_size,
                host_count.begin() + i*chunk_size + i_remaining_count,
                device_i_count.begin());

        thrust::transform(device_i_m.begin(),
                device_i_m.begin() + i_remaining_count,
                device_i_count.begin(),
                device_out_m.begin(),
                Mean());

        thrust::copy(device_out_m.begin(),
                device_out_m.begin() + i_remaining_count,
                host_m.begin() + i*chunk_size);
    }

    thrust::copy(host_m.begin(), host_m.end(), m);
#ifdef PRINT
    for(int i = 0; i < 10; i++) {
        printf("%f\n", m[i]);
    }
#endif
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
    smootht(host_x, host_y, host_m, element_count, h);
    checkpoint(&marker, "TOTAL");
#ifdef PRINT
    for(int i = 0; i < element_count; i++) {
        printf("(%f, %f, %f)\n", host_x[i], host_y[i], host_m[i]);
    }
#endif
    return 0;
}
#endif
