/*
 Have Node 0 play the role of the manager, with the others being workers. 
 * Node 0 will divvy up the work, assigning it to the workers, etc.
Note, however, that in MPI, all the nodes execute the full program. 
 * So they will ALL execute your main(), which contains your test code. 
 * So they all have their own copy of x, whether they generate it at random, 
 * read it from a file, or whatever you want.
Place your main() and numcount() in separate files, so that the TA can link in his own main() for grading. 
 * Make main() argumentless.
Source file names are Problem 1numcount.cpp. and Problem 1main.cpp. 
 * The former file can of course contain support routines for numcount().
The TA test will have at most 8 workers, as opposed to 8 threads as before.
The 3 groups having the fastest code will receive Extra Credit.
 */

#include <math.h>
#include <map>
#include <stdlib.h>
#include <mpi.h>
#include <vector>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <climits>

#define DATA_MSG 0 // type of message containing a chunk to be checked
#define NEWDATA_MSG 1 // type of message indicating new data from worker is being sent
#define NEWDATACOUNT_MSG 2// num elements in array to be sent

//#define DEBUG

#ifdef DEBUG
typedef unsigned long long timestamp_t;
void checkpoint(timestamp_t* marker, const char* message);
timestamp_t get_timestamp();
#endif

//global variables but not shared across nodes
int node_count; //number of nodes in computation
int me; //current node id
int* output; // output array
int global_m;

// Used to keep maps sorted ascending
struct pattern_cmp
{
    bool operator()(const int* left, const int* right)
    {
        // For each element in the pattern
        for(int i = 0; i < global_m; i++) {
            // Compare value in left and right
            // and if not a tie
            if(left[i] != right[i]) {
                // Return comparison for map to sort correctly
                return left[i] < right[i];
            }
        }
        // If same pattern, return same
        return false;
    }
};

void tally(int *x, int n, int m, int start, int end, std::map<int*, int, pattern_cmp> &counts_by_pattern){
    //printf("%d %d\n", start, end);
    // Calculate starting point and how many to count (less for the end)
    int* current_start = x;

    // Tally within range
    for(int i = 0; i < n-(m-1); i++, current_start++) {
        if(start <= x[i] && x[i] < end) {
            counts_by_pattern[current_start] += 1;
        }
    }
}

void minmax(int* start, int* end, int* max, int* min){
    while(start != end) {
        if(*start > *max) *max = *start;
        if(*start < *min) *min = *start;
        start++;
    }
}

//node0
void managernode(int *x, int n, int m){
    MPI_Status status;
#ifdef DEBUG
    timestamp_t marker = get_timestamp();
#endif            
    // Setup variables for splitting up range of values in x
    int max = INT_MIN;
    int min = INT_MAX;
    int each_element_count = ceil(n/(double)node_count);
    int* starting_element_position = x;
    minmax(starting_element_position, starting_element_position
            + each_element_count, &max, &min);
    for(int i = 0; i < node_count-1; i++) {
        int current;
        MPI_Recv(&current, 1, MPI_INT, i+1, DATA_MSG, MPI_COMM_WORLD, &status);
        if(current > max) max = current;
        MPI_Recv(&current, 1, MPI_INT, i+1, DATA_MSG, MPI_COMM_WORLD, &status);
        if(current < min) min = current;
    }
    
    int each_range = ceil((max-min)/(double)node_count);

#ifdef DEBUG
    checkpoint(&marker, "Range Calculation");
#endif

    //send range splits to workers and keep the last one for myself
    int start, end;
    for(int i = 0; i < node_count; i++){
        start = min + each_range*i;
        end = start + each_range;
        if(i < node_count - 1) {
            MPI_Send(&start, 1, MPI_INT, i+1, DATA_MSG, MPI_COMM_WORLD);
            MPI_Send(&end, 1, MPI_INT, i+1, DATA_MSG, MPI_COMM_WORLD);
        }
    }

#ifdef DEBUG
    checkpoint(&marker, "Range sent");
#endif
    
    // I'm the manager node, also count the ending start number
    end++;

    // Manager does its share
    std::map<int*, int, pattern_cmp> counts_by_pattern;
    tally(x, n, m, start, end, counts_by_pattern);

#ifdef DEBUG
    checkpoint(&marker, "Tallied");
#endif
    
    int* worker_counts = new int[node_count];
    int max_worker_count = 0;
    int total_pattern_count = 0;
    // Get counts back from workers
    for(int i = 0; i < node_count; i++){
        int worker_count;
        if(i == node_count-1) {
            worker_count = counts_by_pattern.size();
        } else {
            //get vector size from worker node
            MPI_Recv(&worker_count, 1, MPI_INT, i+1,
                    NEWDATACOUNT_MSG, MPI_COMM_WORLD, &status);
        }
        worker_counts[i] = worker_count;
        max_worker_count = std::max(max_worker_count, worker_count);
        total_pattern_count += worker_count;
    }

#ifdef DEBUG
    checkpoint(&marker, "Worker counts received");
#endif

    // Allocate storage for whole output given total pattern count
    output = (int*)malloc(sizeof(int)*(total_pattern_count*(m+1) + 1));
    // Start with the total pattern count
    output[0] = total_pattern_count;
    // And continue with rest
    int* current_output_location = output + 1;
    int* worker_pattern_indexes = new int[max_worker_count*2];

    //fill up output vector with patterns and counts from worker nodes
    //printf("Node Count %d\n", node_count);
    for(int i = 0; i < node_count - 1; i++){
        int worker_count = worker_counts[i];
        //printf("%d %d Count\n", i, worker_count);
        MPI_Recv(worker_pattern_indexes, worker_count*2,
                MPI_INT, i+1, NEWDATA_MSG, MPI_COMM_WORLD, &status);
        int* current_worker_pattern_indexes = worker_pattern_indexes;
        for(int j = 0; j < worker_count; j++) {
            int* pattern_index = x + *current_worker_pattern_indexes++;
            for(int k = 0; k < m; k++) {
                *current_output_location++ = *pattern_index++;
            }
            *current_output_location++ = *current_worker_pattern_indexes++;
        }
    }
    
    // Fill rest with my patterns and counts
    std::map<int*, int, pattern_cmp>::iterator pit = counts_by_pattern.begin(),
        end_it = counts_by_pattern.end();
    for(; pit != end_it; pit++) {
        int *pattern = pit->first;
        for(int k = 0; k < m; k++) {
            *current_output_location++ = *pattern++;
        }
        *current_output_location++ = pit->second;
    }

#ifdef DEBUG
    checkpoint(&marker, "Output completed");
#endif

    delete[] worker_counts;
    delete[] worker_pattern_indexes;
}

//all nodes but node 0
void workernode(int *x, int n, int m){
    MPI_Status status;
#ifdef DEBUG
    timestamp_t marker = get_timestamp();
    char message[80];
    std::string str_format = "[%*d] %s";
    const char* format = str_format.c_str();
#endif
    int max = INT_MIN;
    int min = INT_MAX;
    int each_element_count = ceil(n/(double)node_count);
    int* starting_element_position = x + me*each_element_count;
    // Find min max or chunk
    minmax(starting_element_position, starting_element_position
            + each_element_count, &max, &min);
    MPI_Send(&max, 1, MPI_INT, 0, DATA_MSG, MPI_COMM_WORLD);
    MPI_Send(&min, 1, MPI_INT, 0, DATA_MSG, MPI_COMM_WORLD);

#ifdef DEBUG
    sprintf(message, format, me, me, "Chunk range sent");
    checkpoint(&marker, message);
#endif
    
    //start and stop points in the range of values in x
    int startIt, endIt;
    std::map<int*, int, pattern_cmp> counts_by_pattern;

    //get the start and end for current worker based on range of x
    MPI_Recv(&startIt, 1, MPI_INT, 0, DATA_MSG, MPI_COMM_WORLD, &status);
    MPI_Recv(&endIt, 1, MPI_INT, 0, DATA_MSG, MPI_COMM_WORLD, &status);

#ifdef DEBUG
    sprintf(message, format, me, me, "Received range");
    checkpoint(&marker, message);
#endif

    tally(x, n, m, startIt, endIt, counts_by_pattern);
    
#ifdef DEBUG
    sprintf(message, format, me, me, "Tallied");
    checkpoint(&marker, message);
#endif

    int count_to_send = counts_by_pattern.size();
    
    //send number of paterns to manager
    MPI_Send(&count_to_send, 1, MPI_INT, 0, 
            NEWDATACOUNT_MSG, MPI_COMM_WORLD);
    
    //numPatterns*m to allow for the pattern and the count
    int outPattern[count_to_send*2];
    
    // Put into my output
    std::map<int*, int, pattern_cmp>::iterator pit = counts_by_pattern.begin(),
        end = counts_by_pattern.end();
    for(int i = 0; pit != end; pit++) {
        // Store index of pattern
        outPattern[i++] = pit->first - x;
        // Store count of pattern
        outPattern[i++] = pit->second;
    }

#ifdef DEBUG
    sprintf(message, format, me, me, "Generated output");
    checkpoint(&marker, message);
#endif
    
    //send data to manager
    MPI_Send(outPattern, count_to_send*2, MPI_INT, 0,
            NEWDATA_MSG, MPI_COMM_WORLD);

#ifdef DEBUG
    sprintf(message, format, me, me, "Sent");
    checkpoint(&marker, message);
#endif
}

int* numcount(int *x, int n, int m){
    // Init pattern size parameter for pattern_cmp
    global_m = m;

    // Init parameters for node count and which node is running using MPI APIs
    MPI_Init(NULL, NULL);
    MPI_Comm_size(MPI_COMM_WORLD, &node_count);
    MPI_Comm_rank(MPI_COMM_WORLD, &me);   

    // Based on node ID go to specific function
    if(me == 0){
        managernode(x, n, m);
    }else{
        workernode(x, n, m);
    }

    MPI_Finalize();
    if(me == 0){
        return output;
    } else {
        // Return nothing if this was not a manager node
        return NULL;
    }
}
