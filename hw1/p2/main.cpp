#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include <string>

#include "Problem2.h"

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

int main(int argc, char **argv)
{
    int *x, n;
    scanf("%d\n", &n);
    x = (int*)malloc(n * sizeof(int));
    for(int i = 0; i < n; i++) {
        scanf("%d\n", x+i);
    }
    int test_count = 3;
    int m = 3;
    timestamp_t start = get_timestamp();
    for(int i = 0; i < test_count; i++) {
        int* o = numcount(x, n, m);
        if(i == test_count-1) {
            printf("%d, ", o[0]);
            for(int i = 0; i < o[0] && i < 100; i++) {
//                if(o[1+i*(m+1)+m] > 1) {
                    for(int j = 0; j < m+1; j++) {
                        printf("%d,", o[1+i*(m+1)+j]);
                    }
                    printf(" ");
//                }
            }
            printf(".....\n");
        }
        free(o);
    }
    timestamp_t end = get_timestamp();
    printf("\nAverage Time: %f\n", (end-start)/(double)(test_count * MICRO_IN_SEC));
    free(x);
}
