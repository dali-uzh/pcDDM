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
  const double d_v;
  const double d_h;
  const double thres;
  const double nDT;
  const double theta;
  const double bias;
  const double v_tL;
  const double v_tR;
  const double v_hL;
  const double v_hR;
  std::vector<double> fixL;//const double fixL; https://www.codeguru.com/cplusplus/c-tutorial-a-beginners-guide-to-stdvector-part-1/
  std::vector<double> fixDur; //const double fixDur;
  const double sd_n;
  GEN gen;

  // output vector to write to
  RVector<double> vecOut;

  // initialize from Rcpp input and output matrices/vectors (the RMatrix/RVector class
  // can be automatically converted to from the Rcpp matrix/vector type)
  ddm2w(const double d_v, const double d_h, const double thres, const double nDT, const double theta, const double bias, const double v_tL, const double v_tR, const double v_hL, const double v_hR, std::vector<double> fixL, std::vector<double> fixDur, const double sd_n, NumericVector vecOut , GEN gen)
    : d_v(d_v), d_h(d_h), thres(thres), nDT(nDT), theta(theta), bias(bias), v_tL(v_tL), v_tR(v_tR), v_hL(v_tL), v_hR(v_tR), fixL(fixL), fixDur(fixDur), sd_n(sd_n), gen(gen), vecOut(vecOut) {}

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
        vec_L[t] = fixL[countFix] + (1-fixL[countFix])*theta;
        vec_R[t] = fixL[countFix]*theta + (1-fixL[countFix]);
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
        X = X + ((d_v*v_tL+ d_h*v_hL)*vec_L[cont] - (d_v*v_tR+ d_h*v_hR)*vec_R[cont])*dt + noise;

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
NumericVector ddm2_parallel(double d_v, double d_h, double thres, double nDT, double theta, double bias, double v_tL, double v_tR, double v_hL, double v_hR, std::vector<double> fixL, std::vector<double> fixDur, double sd_n, unsigned int N) {

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
  ddm2w ddm2w(d_v, d_h, thres, nDT, theta, bias, v_tL, v_tR, v_hL, v_hR, fixL, fixDur, sd_n, vecOut, gen);

  // call the worker
  parallelFor(0, N, ddm2w);

  return vecOut;
}
