#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rcpp.h>
#include <omp.h>
#include <string>
#include <iostream>
#include <stdexcept>
#include <ctype.h>

//#define DEBUG_LIST_PROBS
//#define DEBUG_GET_SUM
//#define DEBUG_STM
//#define DEBUG_GET_EVENT

/*
Full c++ implementation of dice.R used sections for actual parallel code.
*/

using namespace Rcpp;

//functor for filling vector using additon
struct c_unique {
  int current;
  c_unique(int start) : current(start) {}
  int operator()() {return ++current;}
};

//functor for filling vector using subtraction
struct c_unique_minus {
  int current;
  c_unique_minus(int start) : current(start) {}
  int operator()() {return --current;}
};

struct target_less
{
    template<class It>
    bool operator()(It const &a, It const &b) const { return *a < *b; }
};

struct target_equal
{
    template<class It>
    bool operator()(It const &a, It const &b) const { return *a == *b; }
};

//My implementation of std::unique which can handle vec<vec T>
template<class It> It best_unique_ever(It begin, It const end)
{
    std::vector<It> v;

    v.reserve(static_cast<size_t>(std::distance(begin, end)));

    for (It i = begin; i != end; ++i){ v.push_back(i); }

    std::sort(v.begin(), v.end(), target_less());

    v.erase(std::unique(v.begin(), v.end(), target_equal()), v.end());

    std::sort(v.begin(), v.end());

    size_t j = 0;

    for (It i = begin; i != end && j != v.size(); ++i){
        if (i == v[j]){
            using std::iter_swap; iter_swap(i, begin);
            ++j;
            ++begin;
        }
    }
    return begin;
}

//factorial function using recursion
int factorial(int n)
{
  return (n == 1 || n == 0) ? 1 : factorial(n - 1) * n;
}

//choose function using recursion
int nchoosek(int n, int k)
{
    if(k==0) return 1;
    if(n==0) return 0;
    return nchoosek(n-1, k-1)+nchoosek(n-1,k);
}

//overloaded << to print vectors easily.
std::ostream& operator<< (std::ostream& os, std::vector<int> v)
{
    for (std::vector<int>::iterator it = v.begin(); it != v.end(); ++it)
    {
        os << *it << ", ";
    }
    return os;
}

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

std::vector<std::vector<float> > getSumProbs(int ndicePerRoll, int nsidesPerDie) {

    //Variable initialization similiar to line 203
    int nkept = ndicePerRoll, sumModifier = 0, perDieModifier = 0;
    bool dropLowest = TRUE, perDieMinOfOne = TRUE;

    if(nkept > ndicePerRoll){
        std::cout << "nkept must not be greater than ndicePerRoll \n Returning with: ";
        exit(EXIT_FAILURE);
    }

    //231:233
    int numOutcomes = (int)std::pow(nsidesPerDie, ndicePerRoll);
    int numDiceToDrop = ndicePerRoll - nkept;
    int currNumArrangements = 0;

    //235:237
    sumModifier += (perDieModifier * nkept);
    int currentSum = 1;

    //replaces line 239 which initalizes a vector of known size
    std::vector<float> vectorOfSums((((nsidesPerDie * nkept) + sumModifier) - (nkept + sumModifier)) + 1);
    c_unique n((nkept + sumModifier)-1);

    //using our functor to fill the vector with nkept + sumModifier++ until reached end of vectorOfSums
    std::generate(vectorOfSums.begin(), vectorOfSums.end(), n);

    int numPossibleSums = vectorOfSums.size() - 1;

    //sumTallyMatrix is used to track the number of times we see every possible outcome sum,
    //which we will use to produce the probabilities of every sum (e.g., for the 3d6 case we
    //see 10 as a sum 27 times, so the probability of a sum of 10 is 27/216 = .125, while for
    //the 5d6 drop 2 case we see 13 as a sum 1055 times, so the probability of a sum of 13
    //is 1055/7776 = .1356739).

    //fill rows with values for STM
    std::vector<std::vector<float> > sumTallyMatrix(numPossibleSums + 1);

    for(int it = 0; it < (numPossibleSums+1); it++){
        sumTallyMatrix[it].push_back(vectorOfSums[it]);
        sumTallyMatrix[it].push_back(0.0);
        sumTallyMatrix[it].push_back(0.0);
    }

    //boundaryVal is the most extreme die-roll value that will be kept (i.e., the die-roll "boundary"
    //value: e.g., for 5d6 drop lowest two, if our sorted die rolls are {3 4 4 5 6}, boundaryVal is 4).
    //We'll call all dice with this value the "b" dice (because they're on the [b]oundary).

    for(int boundaryVal = 0; boundaryVal < nsidesPerDie; boundaryVal++){

        //numOs is the number of dice whose values are outside of boundaryVal (e.g., for 5d6 drop lowest
        //two, if we roll {3 4 4 5 6}, boundaryVal is 4, so numOs is 1).  We'll call these dice the "o"
        //dice (because they're [o]utside our boundary).
        //NOTE: We have an embedded if clause in our for-loop declaration because if we're dropping lowest
        //and boundaryVal is 1 or we're dropping highest and boundaryVal is nsidesPerDie, there cannot be
        //any dice whose values are outside of boundaryVal, and hence numOs can only be 0.  The following
        //loop syntax might look suspicious, but we *do* want to iterate once in these two cases, as well
        //as in the case where numDiceToDrop is 0 (and in all three such cases, numOs will be 0).

        //Removed logic from line 277 to figure out numDiceToDrop value
        int endPoint;

        if((dropLowest && (boundaryVal+1) == 1) ||
            (!dropLowest && (boundaryVal+1) == nsidesPerDie))
            endPoint = 1;
        else {
            if(numDiceToDrop == 0) endPoint = 1;
            else endPoint = numDiceToDrop;
        }

        for(int num0s = 0; num0s < endPoint; num0s++){

            //numBsKept is the number of b's that will be kept (e.g., for 5d6 drop lowest two, if we roll
            //{3 4 4 5 6}, numBsKept is 1, because one of the two 4's will be kept).
            //Now, since we're discarding numDiceToDrop dice (including the numOs o's), we'll discard
            //(numDiceToDrop - numOs) of the b's and keep numBsKept of them, and thus the total number of
            //b's is (numBsKept + numDiceToDrop - numOs).  NOTE: Hence, the number of dice whose values
            //exceed boundaryVal is (nkept - numBsKept).  We will call these higher dice the "i" dice (since
            //they're "inside" our boundary).

            for(int numBsKept = 0; numBsKept < nkept; numBsKept++){

                //By this part of the function, we've specified a class of outcomes identified by their
                //(boundaryVal, numOs, numBsKept) values--i.e., every outcome in this class has the
                //following properties:
                //1). the die-roll boundary value boundaryVal;
                //2). numOs "o" dice, whose values are outside boundaryVal and will be dropped; and
                //3). numBsKept "b" dice that will be kept.  Furthermore, each such outcome has
                //4). (numBsKept + numDiceToDrop - numOs) "b" dice in total, and
                //5). (nkept - numBsKept) "i" dice, whose values are inside boundaryVal.

                int numBs = (numBsKept+1) + numDiceToDrop - num0s;
                int numIs = nkept - (numBsKept+1);

                //Now, we're interested in sums for the various outcomes in this class, and these sums
                //don't depend upon the order in which the various values appear in our sequence of
                //rolls; i.e., multiple outcomes in this class will have the same values but have the o's,
                //the b's, and the i's appear at different places in the sequence of rolls.  To account
                //for this, we need to multiply each distinct outcome by the number of other outcomes
                //that are identical to it except for the order in which the o's, b's, and i's occur
                //(NOTE: the orders *within* these groups are accounted for below: order within the o's
                //is accounted for immediately below, and we account for the order within the i's in the
                //section of the code in which we enumerate the i's).  For now, we will define a term by
                //which to multiply each sum we find; this term is a result of the multinomial theorem's
                //combinatoric interpretation as the number of ways to put n distinct objects (in this
                //case, our die rolls) into 3 bins of size numOs, (numBsKept + numDiceToDrop - numOs),
                //and (nkept - numBsKept), corresponding to the number of o's, b's, and i's in the class:

                int numArrangementsOfDice = factorial(ndicePerRoll)/((factorial(num0s+1)
                                                * factorial(numBs) * factorial(numIs)));

                //[NOTE: The formula above could overflow if ndicePerRoll gets large, in which case we may
                //consider using lfactorial()--but I think the function would keel over before then anyway]

                //Because we support dropping lowest or highest values, we define convenient variables
                //to allow us to operate over appropriate ranges for the rest of this function

                int innerBoundary, outerBoundary, rangeOfOVals, rangeOfIVals;
                if (dropLowest) innerBoundary = nsidesPerDie; else innerBoundary = 1;
                if (dropLowest) outerBoundary = 1; else outerBoundary = nsidesPerDie;
                rangeOfOVals = std::abs((boundaryVal+1) - outerBoundary);
                rangeOfIVals = std::abs((boundaryVal+1) - innerBoundary);

                std::vector<int> possibleIValsVec;

                if(dropLowest){
                    int length;
                    if(nsidesPerDie - (boundaryVal+2) + 1 <= 0){
                        length = ((boundaryVal+2) - nsidesPerDie) + 1;
                    }else{
                        length = nsidesPerDie - (boundaryVal+2) + 1;
                    }

                    possibleIValsVec.assign(length, 0);

                                        //using functor to fill vector with values from boundaryVal + 1 to boundaryVal + possibleIValsVec.size()
                    if((boundaryVal + 1) < nsidesPerDie){

                        c_unique n(boundaryVal + 1);
                        std::generate(possibleIValsVec.begin(), possibleIValsVec.end(), n);

                    }else{

                        c_unique_minus n(boundaryVal + 3);
                        std::generate(possibleIValsVec.begin(), possibleIValsVec.end(), n);

                    }
                } else {
                    possibleIValsVec.assign((boundaryVal+1), 0);
                    c_unique n(1);
                    //using functor to fill vector with values from 1 to boundaryVal + possibleIValsVec.size()
                    std::generate(possibleIValsVec.begin(), possibleIValsVec.end(), n);
                }

               //Next: The value of boundaryVal is fixed for this loop, but there are many "o" dice values
               //that outcomes in this class might have; because we don't care about these values, we need
               //to increase our multiplicity term to account for the outcomes that will be identical
               //to this iteration's distinct outcome but for the values (and order) of the o's:

               numArrangementsOfDice = numArrangementsOfDice * (int)std::pow(rangeOfOVals,num0s);

               //Now that we've accounted for sorting the values into three bins and for all the possible
               //"o" values that are immaterial to our calculations, we can treat our outcome class as
               //sorted into groups and can focus our attention on the numBsKept b's we keep and the
               //i's.  The numBsKept b's will contribute a known amount to our sum for all outcomes in
               //this class (viz., numBsKept * boundaryVal); but the i's will contribute various amounts,
               //depending on their values.  So now we turn to determining the possible distinct outcomes
               //for this class by enumerating the possible values for the i's.  We will work as follows:
               //rangeOfIVals is the distance between the smallest and largest possible "i" values for this
               //class of outcomes, and we use it to determine the number of distinct outcomes for this
               //class, which is given by rangeOfIVals^numIs.  We create an outcomeMatrix with as many
               //rows as there are distinct outcomes for this class and nkept columns; each element
               //in a row corresponds to a die-roll value, and the sum of the row elements is the
               //sum for that distinct outcome.  We populate outcomeMatrix with a row for every possible
               //value for the i's in this class (and hence all distinct outcomes in the class).  We then
               //calculate the number of permutations of each distinct outcome (e.g., in the 3d6 case,
               //the outcome {1, 1, 2} has three permutations) and use this information to calculate the
               //probability of every possible outcome in this class.

               if((numBsKept+1) == nkept){

                   currentSum = ((numBsKept+1) * (boundaryVal+1)) + sumModifier;

                   sumTallyMatrix[currentSum - sumModifier - (nkept - 1)-1][1]
                           = sumTallyMatrix[currentSum - sumModifier - (nkept - 1)-1][1]
                                + numArrangementsOfDice;

               } else {

                   std::vector<std::vector<int> > outcomeMatrix(nchoosek((rangeOfIVals + numIs - 1), numIs));

                   for(unsigned int i = 0; i < outcomeMatrix.size(); i++){
                       for(int j = 0; j < (numBsKept+1); j++){
                           outcomeMatrix[i].push_back(boundaryVal+1);
                       }
                   }

                   std::vector<std::vector<int> > hCombs;

                   int lengthIValVec = possibleIValsVec.size();

                   if(outcomeMatrix.size() > 0){
                         //setup hCombs to have numIs col and vecsize^numI rows
                         hCombs.resize((int)std::pow(lengthIValVec ,numIs));

                         //setup variables for finding all permutations which will be sort/uniqued.
                         int power = numIs-1;
                         int stopPoint = (int)std::pow(lengthIValVec, numIs);

                         for(int i = 0; i < numIs; i++){

                             int timesRepeatValue = (int)std::pow(lengthIValVec, power);

                             int repeat = 0;
                             while(repeat < stopPoint){
                                 for(int j = 0; j < lengthIValVec ; j++){
                                     for(int k = 0; k < timesRepeatValue; k++){
                                         hCombs[repeat++].push_back(possibleIValsVec[j]);
                                     }
                                 }
                             }
                             power--;
                         }
                     }




#ifdef DEBUG_GET_SUM
                       std::cout << "Printing hCombs " << std::endl;
                       for(std::vector<std::vector<int> >::iterator vv_i = hCombs.begin(); vv_i != hCombs.end(); vv_i++){
                           for(std::vector<int>::iterator v_i = (*vv_i).begin(); v_i != (*vv_i).end(); v_i++){
                               std::cout << *v_i << " ";
                           }

                           std::cout << std::endl;
                       }
#endif

                       for(unsigned int i = 0; i < hCombs.size(); i++){
                           std::sort(hCombs[i].begin(), hCombs[i].end());
                       }

                       hCombs.erase(best_unique_ever(hCombs.begin(), hCombs.end()), hCombs.end());

                       //print hCombs
#ifdef DEBUG_GET_SUM
                       std::cout << "Printing hCombs " << std::endl;
                       for(std::vector<std::vector<int> >::iterator vv_i = hCombs.begin(); vv_i != hCombs.end(); vv_i++){
                           for(std::vector<int>::iterator v_i = (*vv_i).begin(); v_i != (*vv_i).end(); v_i++){
                               std::cout << *v_i << " ";
                           }

                           std::cout << std::endl;
                       }
#endif

                       std::vector<std::vector<int> > tabledHCombs(hCombs.size());
                       tabledHCombs = table(hCombs, tabledHCombs);

                       std::vector<int> hPermCounts;
                       for(std::vector<std::vector<int> >::iterator vv_i = tabledHCombs.begin(); vv_i != tabledHCombs.end(); vv_i++){
                           int prodFact = 1;
                           for(std::vector<int>::iterator v_i = (*vv_i).begin(); v_i != (*vv_i).end(); v_i++){
                               prodFact *= factorial(*v_i);
                           }
                           hPermCounts.push_back(prodFact);
                       }

                       for(unsigned int i = 0; i < outcomeMatrix.size(); i++){
                           for(unsigned int j = 0; j < hCombs[i].size(); j++){
                               outcomeMatrix[i].push_back(hCombs[i][j]);
                           }
                       }

                       int i = 0;
                       for(std::vector<std::vector<int> >::iterator vv_i = outcomeMatrix.begin(); vv_i != outcomeMatrix.end(); vv_i++){
                           currentSum = 0;
                           for(std::vector<int>::iterator v_i = (*vv_i).begin(); v_i != (*vv_i).end(); v_i++){
                               currentSum += *v_i;
                           }

                           currentSum += sumModifier - 1;


                           currNumArrangements = numArrangementsOfDice * hPermCounts[0];

                           sumTallyMatrix[currentSum - sumModifier - (nkept - 1)][1] = sumTallyMatrix[currentSum - sumModifier - (nkept - 1)][1] + currNumArrangements;

                           i++;

                       }
                   }

               }

            }
        }

    if(perDieMinOfOne){

        if (sumTallyMatrix[numPossibleSums][0] <= nkept){

            sumTallyMatrix[numPossibleSums].push_back(nkept);
            sumTallyMatrix[numPossibleSums].push_back(numOutcomes);
            sumTallyMatrix[numPossibleSums].push_back(numOutcomes);


        } else{

        }
    } else {

        int extraWaysToRollMin = 0;
        std::vector<std::vector<float> > newSumTallyMatrix;

        for(unsigned int i = 0; i < sumTallyMatrix.size(); i++){

            if(sumTallyMatrix[i][0] < nkept){

                extraWaysToRollMin += sumTallyMatrix[i][1];

            }

            if(sumTallyMatrix[i][0] >= nkept){

                sumTallyMatrix.push_back(sumTallyMatrix[i]);

            }
        }

        newSumTallyMatrix[0][2] += extraWaysToRollMin;
        sumTallyMatrix = newSumTallyMatrix;

    }

    std::vector<float> overallAverageSum;
    overallAverageSum.assign(1,0);

    for(unsigned int i = 0; i < sumTallyMatrix.size(); i++){
        sumTallyMatrix[i][2] = sumTallyMatrix[i][1];
        sumTallyMatrix[i][1] = sumTallyMatrix[i][1]/numOutcomes;
        overallAverageSum[0] += sumTallyMatrix[i][0] * sumTallyMatrix[i][2] / numOutcomes;
    }

    //add average to sumTallyMatrix in stead of making a whole new data structure.
    sumTallyMatrix.push_back(overallAverageSum);

#ifdef DEBUG_STM
        std::cout << "Printing sumTallyMatrixFinal" << std::endl;
        for(unsigned int i = 0; i < sumTallyMatrix.size(); i++){
            for(unsigned int j = 0; j < sumTallyMatrix[i].size(); j++){
                std::cout << sumTallyMatrix[i][j] << " ";
            }
            std::cout << std::endl;
        }

#endif

    return sumTallyMatrix;
}

// This helper function returns the probabilities of each element of eventList
std::vector<float> getEventListProbs(int ndicePerRoll, int nsidesPerDie, std::vector<std::vector<int> > eventList){

    std::vector<std::vector<float> > probs = getSumProbs(ndicePerRoll, nsidesPerDie);

#ifdef DEBUG_LIST_PROBS
    std::cout << "printing probs from getSumProbs" << std::endl;
    for(unsigned int i = 0; i < probs.size(); i++){
        for(unsigned int j = 0; j < probs[i].size(); j++){
            std::cout << probs[i][j] << " ";
        }
        std::cout << std::endl;
    }
#endif

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

          successProbForThisRoll = successProbForThisRoll + probs[((eventList[i][j]-1) - (ndicePerRoll-1))][1];

          eventListProbs.push_back(successProbForThisRoll);
      }

    }

    return eventListProbs;
}

RcppExport SEXP getEventProb(SEXP nrolls,
                             SEXP ndicePerRoll,
                             SEXP nsidesPerDie,
                             SEXP eventList,
                             SEXP orderMatters){

    //convert to cland variables
    int c_nrolls = as<int>(nrolls),
              c_ndicePerRoll = as<int>(ndicePerRoll),
              c_nsidesPerDie = as<int>(nsidesPerDie);

    List c_eventList = eventList;
    int c_eventSize = length(c_eventList);

    const bool c_orderMatters = as<bool>(orderMatters);

    if(c_eventSize > c_nrolls){
        std::cout << "The length of eventList must not be greater than nrolls. \nReturning with: ";
        return R_NilValue;
    }

    if(c_orderMatters && c_eventSize != c_nrolls){
        std::cout << "If orderMatters is passed as TRUE, the length of eventList must equal\nnrolls (i.e., there must be an element of eventList for each roll) \nReturning with: ";
        return R_NilValue;
    }

    /* //not exactly sure how to do with the List object, can't do with vector since converting to <int>.
    if (!all(sapply(eventList, is.numeric))){
        errorVector = append(errorVector, "\n* All elements of eventList must be numeric vectors")
    }
    */

    //instead of a list we will use a vector of vectors
    std::vector<std::vector<int> > vec_eventList;

    for(int i = 0; i < c_eventSize; i++){
        vec_eventList.push_back(as<std::vector<int> >(c_eventList[i]));
    }

    //variable for finding min and max elements in vec_eventList
    int max = 0;
    int min = INT_MAX;

    for(int i = 0; i < c_eventSize; i++){
        //make sure vectors in vec_eventList have no duplicate values
        std::sort(vec_eventList[i].begin(), vec_eventList[i].end());
        vec_eventList[i].erase(std::unique(vec_eventList[i].begin(), vec_eventList[i].end()),vec_eventList[i].end());

        //will need this later may as well calc it while already looping
        for(std::vector<int>::iterator j = vec_eventList[i].begin(); j != vec_eventList[i].end(); ++j){
            if(*j > max) max = *j;
            if(*j < min) min = *j;
            if(*j < 0){
                std::cout << "All numbers in each element of eventList must be positive integers \nReturning with: ";
                return R_NilValue;
            }
        }
    }

    if(min < c_ndicePerRoll || max > (c_ndicePerRoll * c_nsidesPerDie)){
        std::cout << "All numbers in each element of eventList must be between ndicePerRoll\nand (ndicePerRoll * nsidesPerDie) \nReturning with: ";
        return R_NilValue;
    }

    //If eventList doesn't have an element for each roll, we add elements until it does;
    //after this point, each element of eventList will constrain one roll (but some of
    //those constraints may be simply {min:max} for that roll--i.e., trivial constraints)

    //Line 117:121

    if(c_eventSize < c_nrolls){
        vec_eventList.resize(c_nrolls);
        for(int i = (c_nrolls - c_eventSize); i < c_nrolls; i++){
            if(max == 0){
#ifdef DEBUG_GET_EVENT
                std::cout << "if" << std::endl;
#endif
                //setup functor
                c_unique n(c_ndicePerRoll);
                vec_eventList[i].assign(c_nrolls, 0);
                //fill vector with ndicePerRoll:(ndicePerRoll*nsidesPerDie)
                std::generate(vec_eventList[i].begin(),
                        vec_eventList[i].end(), n);
            }else{
#ifdef DEBUG_GET_EVENT
                std::cout << "else" << std::endl;
#endif
                //need to fill next row with all elements in each row of vec_eventList... ex
                //vec_eventList = c(4,3,c(1,2))... vec_eventList[i+(c_nrolls - c_eventSize)] = c(1,2,3,4)
                //order doesn't matter.;
                vec_eventList[i] = std::vector<int>(0,0);
#ifdef DEBUG_GET_EVENT
                std::cout << "done intializing" << std::endl;
#endif
                for(int itt = 0; itt < c_eventSize; itt++){
                    for(unsigned int it = 0; it < vec_eventList[itt].size(); it++){
                        vec_eventList[i].push_back(vec_eventList[itt][it]);
                    }
                }

            }
        }
    }

#ifdef DEBUG_GET_EVENT
    std::cout << "Printing VecList" << std::endl;
    for(unsigned int i = 0; i < vec_eventList.size(); i++){
        for(std::vector<int>::iterator j = vec_eventList[i].begin();
            j != vec_eventList[i].end(); ++j){
            std::cout << *j << " ";
        }
        std::cout << std::endl;
    }
#endif

    float outcomeProb = 0;

    if(c_orderMatters){
        std::vector<float> outcomeProbVec = getEventListProbs(c_ndicePerRoll, c_nsidesPerDie, vec_eventList);
        //calculate product of vector
        outcomeProb = 1;
        for(std::vector<float>::iterator it = outcomeProbVec.begin(); it != outcomeProbVec.end(); it++){
            outcomeProb *= *it;
        }

    } else{
        // i.e., if (!orderMatters)
        // We only calculate probabilities if each element of eventList is a length-1 vector
        // (i.e., a single number), e.g., {2, 3, 2}; if any element is longer than that, e.g.,
        // {2, {3, 4}, 2}, we call ourselves recursively on each list we can construct of only
        // length-1 vectors (e.g., in the example above we'd call ourselves on {2, 3, 2} and
        // {2, 4, 2}); then we sum the resulting probabilities (which, since orderMatters is
        // FALSE, account for all permutations of each of {2, 3, 2} and {2, 4, 2}) to arrive
        // at our probability for the original list of {2, {3, 4}, 2}

        //line 137-138
        std::vector<int> listElemLengths;
        int listElemProduct = 1;
        unsigned int maxListElemLength = 0;

        //Another method for produce of vector, seems pointless
        //to call this way when already looping below.
        //int p = std::accumulate(vec.begin(),
        //vec.end(), 1, std::multiplies<int>());

        for(unsigned int i = 0; i < vec_eventList.size(); i++){
            //find the max length in the event vector
            if(vec_eventList[i].size() > maxListElemLength){
                maxListElemLength = vec_eventList[i].size();
            }
            //get the product of lengths
            listElemProduct *= vec_eventList[i].size();

            //store lengths in vector
            listElemLengths.push_back(vec_eventList[i].size());

        }

        if(maxListElemLength > 1){

            std::vector<std::vector<int> > combMatrix(listElemProduct);

            if(c_nrolls > 1){
                for(int eventRow = 0; eventRow < c_nrolls; eventRow++){
                    //ex. of how this works. Need to fill first col with c(1,2,3) and we have a row size of 6.
                    //Thus we need to do [1,2,3,1,2,3] which can be achieved by indexing into vec_eventList
                    //with current row % 3 which outputs 0,1,2 indices for row 0:2 then 0,1,2 indices again
                    //for row 3:5.
                    int product = 1;

                    for(int length = eventRow + 1; length < c_nrolls; length++){
                        product *= listElemLengths[length];
                    }

                    int repeat = 0;
                    while(repeat < listElemProduct){
                        for(unsigned int eventCol = 0; eventCol < vec_eventList[eventRow].size(); eventCol++){
                            for(int j = 0; j < product; j++){
                                combMatrix[repeat].push_back(vec_eventList[eventRow][eventCol]);
                                repeat++;
                            }
                        }
                    }

                }
            } else {
                std::vector<std::vector<int> >::iterator vv_i;
                std::vector<int>::iterator v_i;
                int i = 0;
                for(vv_i = vec_eventList.begin(); vv_i != vec_eventList.end(); vv_i++){
                    for(v_i = (*vv_i).begin(); v_i != (*vv_i).end(); v_i++){
                        combMatrix[i++].push_back(*v_i);
                    }
                }
            }

            //Next we eliminate all rows that are permutations of other rows
            //(otherwise we would over-count in the calculations that follow)

            //Have to handle duplicate removal here or else the case where all length == 1 breaks.
            if(c_nrolls > 1){
                for(unsigned int i = 0; i < combMatrix.size(); i++){
                    std::sort(combMatrix[i].begin(), combMatrix[i].end());
                }

                combMatrix.erase(best_unique_ever(combMatrix.begin(), combMatrix.end()), combMatrix.end());

            }else {
                std::sort(combMatrix[0].begin(), combMatrix[0].end());
                combMatrix[0].erase(std::unique(combMatrix[0].begin(), combMatrix[0].end()), combMatrix[0].end());
            }

#ifdef DEBUG_GET_EVENT
            std::cout << std::endl << "Printing unique combMatrix by row" << std::endl;
            for(unsigned int i = 0; i < combMatrix.size(); i++){
                for(unsigned int j = 0; j < combMatrix[i].size(); j++){
                    std::cout << combMatrix[i][j] << " ";
                }
                std::cout << std::endl;
            }
#endif

            //Now we make a recursive call for each row of combMatrix and sum the resulting
            //probabilities to arrive at our probability for the original eventList

            for(unsigned int i = 0; i < combMatrix.size(); i++){
                outcomeProb += as<float>(getEventProb(nrolls,
                                            ndicePerRoll,
                                            nsidesPerDie,
                                            //reference first row and wrap it as an SEXP
                                            wrap(combMatrix[i]),
                                            orderMatters));
            }

        } else {

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

            std::vector<float> eventListProb = getEventListProbs(c_ndicePerRoll, c_nsidesPerDie, vec_eventList);

            float productEventListProb = 1.0;

            for(unsigned int i = 0; i < eventListProb.size(); i++){
                productEventListProb *= eventListProb[i];
            }

            outcomeProb += (productEventListProb * factorial(c_nrolls))/combMatProduct;

        }
    }

    //wrap float as SEXP
    return wrap(outcomeProb);

}
