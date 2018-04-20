#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double foo(DataFrame x, DataFrame y) {
    IntegerVector x_id = x["id"];
    IntegerVector y_id = y["id"];
    IntegerVector idx;
    for ( int i = 0; i < y.nrow(); ++i ) {
        int ID = y_id[i];
        for ( int j = 0; j < x.nrow(); ++j ) {
            int compare_id = x_id[j];
            if ( compare_id == ID ) {
                idx.push_back(j);
            }
        }
    }
    NumericVector val = x["val"];
    val = val[idx];
    return sum(val);
}