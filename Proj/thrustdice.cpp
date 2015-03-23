#include <Rcpp.h>
#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/sequence.h>
#include <cstdio>

using namespace Rcpp;

__device__ std::vector<std::vector<int > > table(std::vector<std::vector<int > > vec_eventList, std::vector<std::vector<int > > combMatrix){

    int prev = vec_eventList[0][0];
    int numFound = 1;

    for(unsigned int i = 1; i < vec_eventList.size(); i++){
        if(prev == vec_eventList[i][0]){
            numFound++;
        }else{
            combMatrix[0].push_back(numFound);
            numFound = 1;
            prev = vec_eventList[i][0];
        }
    }
    combMatrix[0].push_back(numFound);

    return combMatrix;

}

//factorial function
int factorial(int n)
{
  int result = 1;
  while(n > 1) {
      result *= n--;
  }
  return result;
}

// This helper function returns the probabilities of each element of eventList
__device__ std::vector<float> getEventListProbs(int ndicePerRoll, int nsidesPerDie, std::vector<std::vector<int> > eventList, float* probs){

    // On the assumption that eventList has length nrolls (which is safe since this is a
    // private helper function), we calculate the probability of getting an acceptable
    // outcome (a "success") on each of the rolls by iterating through the vector of
    // successes for that roll and adding the corresponding probability to our tally

    std::vector<float> eventListProbs;

#ifdef DEBUG_LIST_PROBS
    for(unsigned int i = 0; i < eventList.size(); i++){
        for(unsigned int j = 0; j < eventList[i].size(); j++){
            std::cout << eventList[i][j] << " ";
        }
    }
    std::cout << std::endl;

#endif

    for (unsigned int i = 0; i < eventList.size(); i++)
    {
      std::sort(eventList[i].begin(), eventList[i].end());
      float successProbForThisRoll = 0.0;
      for (unsigned int j = 0; j < eventList[i].size(); j++)
      {
          successProbForThisRoll = successProbForThisRoll + probs[(eventList[i][j]-1 - (ndicePerRoll - 1))];
      }
      eventListProbs.push_back(successProbForThisRoll);
    }

    return eventListProbs;
}

struct RowGetEventProb
{
    int *combMat;
    float *probs;
    int ncol;
    int c_ndicePerRoll;
    int c_nsidesPerDie;
    int c_nrolls;

    RowGetEventProb(int ncol, int c_nrolls, int c_ndicePerRoll, int c_nsidesPerDie, thrust::device_vector<int>::iterator it_combMat, thrust::device_vector<float>::iterator it_probs)
        : ncol(ncol), c_nrolls(c_nrolls), c_ndicePerRoll(c_ndicePerRoll), 
            c_nsidesPerDie(c_nsidesPerDie)
    {
        combMat = thrust::raw_pointer_cast(&it_combMat[0]);
        probs = thrust::raw_pointer_cast(&it_probs[0]);
    }

    __device__ float operator()(const int& matrix_i)
    {
        std::vector<std::vector<int> > vec_eventList;
        for(int* p = combMat+matrix_i*ncol; p < combMat+matrix_i*ncol+ncol; p++){
            vec_eventList.push_back(std::vector<int>(p, p+1));
        }
            
        //If each element of eventList is a length-1 vector, we can convert eventList
        //itself to a vector; then we calculate the probability of getting the specific
        //set of outcomes specified by eventList in any order (reflecting the fact that
        //orderMatters was passed in as FALSE)

        //Replaces line sapply(eventList, max) which converts to a vector
        std::vector<std::vector<int> >combMatrix(1);

        combMatrix = table(vec_eventList, combMatrix);

        int combMatProduct = 1;

        for(unsigned int i = 0; i < combMatrix[0].size(); i++){
            combMatProduct *= factorial(combMatrix[0][i]);
        }

        std::vector<float> eventListProb = getEventListProbs(c_ndicePerRoll, c_nsidesPerDie, vec_eventList, probs);

        float productEventListProb = 1.0;

        for(unsigned int i = 0; i < eventListProb.size(); i++){
            productEventListProb *= eventListProb[i];
        }
        
        float result = (productEventListProb * factorial(c_nrolls))/combMatProduct;

        return result;
    }
};

RcppExport SEXP thrustApplyGetEventProb(SEXP combMatrix, SEXP nrolls, SEXP probs, SEXP ndicePerRoll, SEXP nsidesPerDie){
    NumericMatrix m = combMatrix;
    NumericVector probsM = probs;

    // Import into C++ from SEXP
    int ncol = m.ncol(),
        nrow = m.nrow(),
        size = ncol*nrow;
    int c_nrolls = as<int>(nrolls);
    int c_ndicePerRoll = as<int>(ndicePerRoll);
    int c_nsidesPerDie = as<int>(nsidesPerDie);
    thrust::host_vector<int> hv(size);
    for(int i = 0; i < nrow; i++){
        for(int j = 0; j < ncol; j++){
            hv[i*ncol + j] = m(i, j);
        }
    }

    // Copy into device vectors
    thrust::device_vector<int> dm(size);
    thrust::copy(hv.begin(), hv.end(), dm.begin());

    thrust::host_vector<float> hprobs(probsM.size());
    thrust::copy(probsM.begin(), probsM.end(), hprobs.begin());

    thrust::device_vector<float> dprobs(size);
    thrust::copy(hprobs.begin(), hprobs.end(), dprobs.begin());

    thrust::device_vector<int> seq(nrow);
    // Set up indexing
    thrust::sequence(seq.begin(), seq.end());
    thrust::device_vector<float> dv(nrow);

    // Run functor
    thrust::transform(seq.begin(), seq.end(),
            dv.begin(),
            RowGetEventProb(ncol, c_nrolls, 
                c_ndicePerRoll, c_nsidesPerDie, 
                dm.begin(), dprobs.begin()));
    thrust::host_vector<float> host_prob(nrow);
    // Copy results back
    thrust::copy(dv.begin(), dv.end(), dprobs.begin());
    
    // Return sum (default reduce behavior)
    return wrap(thrust::reduce(dv.begin(), dv.end()));
}
