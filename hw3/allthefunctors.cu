#include <cstdio>
#include <cmath>

#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/generate.h>
#include <thrust/sort.h>
#include <thrust/copy.h>
#include <thrust/count.h>

#define STANDALONE
#define TEST
#define PRINT

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

struct twice {
    __device__ float operator()(const float n) {
        return n+2;
    }
};

void smootht(float *x, float *y, float *m, int n, float h) {
    /*thrust::host_vector<float> host_x(x, x + n);*/
    //thrust::host_vector<float> host_y(y, y + n);
    //thrust::host_vector<float> host_m(m, m + n);
    thrust::host_vector<float> host_x(10);

    thrust::device_vector<float> device_x = host_x;
    thrust::device_vector<float> device_m(host_x.size());

    thrust::transform(device_x.begin(), device_x.end(), device_m.begin(), twice());
    /*thrust::sort(device_x.begin(), device_x.end());*/
    thrust::copy(device_m.begin(), device_m.end(), host_x.begin());

    for(int i = 0; i < 10; i++) {
        printf("%f\n", host_x[i]);
    }

}

#ifdef STANDALONE
int main(int argc, char **argv) {
    printf("hello\n");
    int element_count;
    int h;
#ifdef TEST
    h = 5;
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
        host_x[i] = element_count - 1- i;
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
