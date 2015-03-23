#include <Rcpp.h>
#include <cstdio>
#include <omp.h>

using namespace Rcpp;

std::vector<std::vector<int > > table(std::vector<std::vector<int > > vec_eventList, std::vector<std::vector<int > > combMatrix){

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
std::vector<float> getEventListProbs(int ndicePerRoll, int nsidesPerDie, std::vector<std::vector<int> > eventList, float* probs){

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

float RowGetEventProb(int ncol, int c_nrolls, int c_ndicePerRoll, int c_nsidesPerDie, int *combMat, float *probs, int matrix_i)
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


RcppExport SEXP ompApplyGetEventProb(SEXP combMatrix, SEXP nrolls, SEXP probs, SEXP ndicePerRoll, SEXP nsidesPerDie){
    NumericMatrix m = combMatrix;
    NumericVector probsM = probs;

    int ncol = m.ncol(),
        nrow = m.nrow(),
        size = ncol*nrow;
    int c_nrolls = as<int>(nrolls);
    int c_ndicePerRoll = as<int>(ndicePerRoll);
    int c_nsidesPerDie = as<int>(nsidesPerDie);
    int* hv = new int[size];

//#pragma omp parallel for schedule(static,8) collapse(2)
    for(int i = 0; i < nrow; i++){
        for(int j = 0; j < ncol; j++){
            hv[i*ncol + j] = m(i, j);
        }
    }

    std::vector<float> hprobs = as<std::vector<float> >(probs);
    float * passprob = &hprobs[0];


    int nProcessors=omp_get_max_threads();
    omp_set_num_threads(nProcessors);


    float sum[nProcessors];
    float totalSum = 0;

    for(int i = 0; i < nProcessors; i++)
    {
        sum[i] = 0;
    }


 #pragma omp parallel for schedule(static,1)
    for(int i = 0; i < nrow; i++)
    {
         //int me = omp_get_thread_num();
        sum[omp_get_thread_num()] += RowGetEventProb(ncol, c_nrolls, c_ndicePerRoll, c_nsidesPerDie, hv, passprob, i);
        //std::cout << " " << me << " ";
    }

    
     for(int i = 0; i < nProcessors; i++)
     {
         totalSum += sum[i];
     }

    return wrap(totalSum);
}


