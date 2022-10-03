#include <Rcpp.h>
#include <RcppParallel.h>
#include <math.h>
#include <iostream>
#include <vector>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/variate_generator.hpp>
#include <time.h>
#include <numeric>

using namespace Rcpp;
using namespace RcppParallel;

typedef boost::mt19937                     ENG;    // Mersenne Twister
typedef boost::normal_distribution<double> DIST;   // Normal Distribution
typedef boost::variate_generator<ENG,DIST> GEN;    // Variate generator



// [[Rcpp::depends(RcppParallel)]]
struct ddm2w : public Worker { 

  // input vectors/matrices to read from
  const double drift;
  const double thres;
  const double nDT;
  const double thetaGaze_A;
  const double thetaGaze_B;
  const double bias;
  const double val_A1;
  const double val_B1;
  const double val_A2;
  const double val_B2;
  std::vector<double> fixL;//const double fixL; https://www.codeguru.com/cplusplus/c-tutorial-a-beginners-guide-to-stdvector-part-1/
  std::vector<double> fixDur; //const double fixDur;
  const double sd_n; 
  GEN gen; 

  // output vector to write to
  RVector<double> vecOut;

  // initialize from Rcpp input and output matrices/vectors (the RMatrix/RVector class
  // can be automatically converted to from the Rcpp matrix/vector type)
  ddm2w(const double drift, const double thres, const double nDT, const double thetaGaze_A, const double thetaGaze_B, const double bias, const double val_A1, const double val_B1, const double val_A2, const double val_B2, std::vector<double> fixL, std::vector<double> fixDur, const double sd_n, NumericVector vecOut , GEN gen)
    : drift(drift), thres(thres), nDT(nDT), thetaGaze_A(thetaGaze_A), thetaGaze_B(thetaGaze_B), bias(bias), val_A1(val_A1), val_B1(val_B1), val_A2(val_A2), val_B2(val_B2), fixL(fixL), fixDur(fixDur), sd_n(sd_n), gen(gen), vecOut(vecOut) {}
  
  // function call operator that work for the specified range (begin/end)
  void operator()(std::size_t begin, std::size_t end) {

    double T = 50, dt = 0.001, lt; // T = 5.2 
    lt = (int)(T/dt); // time step

    std::vector<double> vec_L(lt,1);
    std::vector<double> vec_R(lt,1);
    
    int countFix = 0;
    double sum_of_fix = fixDur[countFix];
    
    for (int t=0; t<lt; t++) {
        if (sum_of_fix < (t*dt*1000)) { // ms
            countFix++;
            std::vector<double>::iterator endInd = fixDur.begin(); //https://www.techiedelight.com/get-iterator-specific-position-vector-cpp/
            std::advance(endInd, countFix);
            sum_of_fix = std::accumulate(fixDur.begin(), endInd, 0.0); //https://stackoverflow.com/questions/3221812/how-to-sum-up-elements-of-a-c-vector
        } 
        vec_L[t] = fixL[countFix] + (1-fixL[countFix])*thetaGaze_A;
        vec_R[t] = fixL[countFix]*thetaGaze_B + (1-fixL[countFix]);
        // Rcout << fixL[countFix] << ',';  
    }

    
    for (std::size_t i = begin; i < end; i++) {
      vecOut[i] = T;
      double X = bias*thres;
      int flag = 0;
      double cont = 0;
      double noise = 0;

      while (flag==0 && cont<lt) {

        noise=gen()*sqrt(dt);
        
        X = X + ( drift*( (val_A1+val_A2)*0.5 )*vec_L[cont] - drift*( (val_B1+val_B2)*0.5 )*vec_R[cont] )*dt + noise;

        if (X > thres) {
          flag=1;
          vecOut[i] = nDT + cont*dt;
        }
        else if (X < 0) {
          flag=1;
          vecOut[i] = -nDT -cont*dt;
        }
        cont++;

      }
    }
  }
};


// [[Rcpp::export]]
NumericVector ddm2_parallel(double drift, double thres, double nDT, double thetaGaze_A, double thetaGaze_B, double bias, double val_A1, double val_B1, double val_A2, double val_B2, std::vector<double> fixL, std::vector<double> fixDur, double sd_n, unsigned int N) {

  //const double sd_n = 1.4;
  struct timespec time;
  clock_gettime(CLOCK_REALTIME, &time);
  ENG  eng;
  eng.seed(time.tv_nsec);
  DIST dist(0,sd_n);
  GEN  gen(eng,dist);

  //output vector
  NumericVector vecOut(N);
  
  // create the worker
  ddm2w ddm2w(drift, thres, nDT, thetaGaze_A, thetaGaze_B, bias, val_A1, val_B1, val_A2, val_B2, fixL, fixDur, sd_n, vecOut, gen);

  // call the worker
  parallelFor(0, N, ddm2w);

  return vecOut;
}
