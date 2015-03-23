#include <omp.h>
#include <math.h>
#include <map>
#include <stdlib.h>

//#define DEBUG

#ifdef DEBUG
typedef unsigned long long timestamp_t;

timestamp_t get_timestamp();

void checkpoint(timestamp_t* marker, const char* message);
#endif

int global_m;

// Used to keep maps sorted ascending
struct pattern_cmp
{
    bool operator()(const int* left, const int* right)
    {
        for(int i = 0; i < global_m; i++) {
            if(left[i] != right[i]) {
                return left[i] < right[i];
            }
        }
        return false;
    }
};

int* numcount(int *x, int n, int m)
{
    global_m = m;
#ifdef DEBUG
    timestamp_t start = get_timestamp();
    timestamp_t marker = get_timestamp();
#endif
    // Stores counts per pattern per thread
    std::map<int*, int, pattern_cmp>* counts_by_patterns;
    // Output array
    int* output;
    // Used to calculate the number of items each thread should process
    int each_count;
    // Stores the number of threads started
    int thread_count;
    // Stores a global number of items left to process
    int number_left_to_process;
    // Stores a global index to half the number of items left to process
    int mid_to_process;

    if(m == 2){
        if(n <= 10000) thread_count = 3;
        else thread_count = omp_get_max_threads();
    } else if(m == 3) {
        if(n <= 10000) thread_count = 3;
        else thread_count = omp_get_max_threads();
    } else if(m == 4) {
        if(n <= 10000) thread_count = 3;
        else thread_count = omp_get_max_threads();
    } else thread_count = omp_get_max_threads();

    #pragma omp parallel num_threads(thread_count)
    {
        int me = omp_get_thread_num();

        #pragma omp single
        {
            // Set up global numbers used during combine phase
            number_left_to_process = thread_count;
            mid_to_process = ceil(number_left_to_process / 2.0);

            // Set up count per thread for separated stage
            each_count = ceil((n - (m - 1))/(double)thread_count);
            // Init data structures
            counts_by_patterns = new std::map<int*, int, pattern_cmp>[thread_count];
#ifdef DEBUG
            checkpoint(&marker, "Setup");
#endif
        }

        // Calculate starting point and how many to count (less for the end)
        int my_count = std::min(each_count, n - (m -1) - each_count*me);
        int *current_start_x = x + each_count*me;

        // Count section
        for(int i = 0; i < my_count; i++, current_start_x++) {
             counts_by_patterns[me][current_start_x] += 1;
        }

        #pragma omp barrier

#ifdef DEBUG
        #pragma omp single
        {
            checkpoint(&marker, "Separate complete");
        }
#endif

        // While there are more than one sections left, insert the second half
        // of the sections into the first half of the section, to fold the number
        // of items
        while(number_left_to_process > 1)
        {
            int my_src = mid_to_process + me;
            // If the thread is part of the first half
            if(me < mid_to_process && my_src < number_left_to_process) {
                // Fold src into me
                std::map<int*, int, pattern_cmp>::iterator pit,
                        begin = counts_by_patterns[my_src].begin(),
                        end = counts_by_patterns[my_src].end();
                for(pit = begin; pit != end; ++pit) {
                    counts_by_patterns[me][pit->first] += pit->second;
                }

                // Free memory
                counts_by_patterns[my_src].clear();
            } // otherwise skip

            #pragma omp barrier

            #pragma omp single
            {
                // Now there are half as many sections
                number_left_to_process = ceil(number_left_to_process / 2.0);
                mid_to_process = ceil(number_left_to_process / 2.0);
            }
        }
    }

#ifdef DEBUG
    checkpoint(&marker, "Combine complete");
#endif

    // Remaining section is the final counts
    int final_pattern_count = counts_by_patterns[0].size();

    // Init output
    output = (int*)malloc((1 + final_pattern_count*(m + 1))*sizeof(int));
    output[0] = final_pattern_count;

    // Copy into output, doing this is in parallel didn't seem to help.
    int i = 1;
    std::map<int*, int, pattern_cmp>::iterator final_it,
            begin = counts_by_patterns[0].begin(),
            end = counts_by_patterns[0].end();
    for(final_it = begin; final_it != end; ++i, ++final_it) {
        int* pattern = final_it->first;
        for(int j = 0; j < m; ++j, ++i) {
            output[i] = *pattern++;
        }
        output[i] = final_it->second;
    }

#ifdef DEBUG
    checkpoint(&marker, "Output complete");
    checkpoint(&start, "TOTAL --------------\n");
#endif

    // Free memory
    delete[] counts_by_patterns;

    return output;
}
