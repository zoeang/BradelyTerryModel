#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame ex_fun(DataFrame x, IntegerVector ids) {
    IntegerVector x_ids = x["id"]; // get the IDs in the dataframe
    NumericVector val = x["val"]; // get the other vector(s) in x
    IntegerVector idx; // An integer vector to store the rows to keep
    int n = x_ids.size(), m = ids.size(); // Just object size variables
    for ( int i = 0; i < n; ++i ) { // for every row in the dataframe
        for ( int j = 0; j < m; ++j ) { // for every id in ids
            if ( x_ids[i] == ids[j] ) { // for every x_id in x_ids
                idx.push_back(i); // if x_id is in ids, add it to idx
            }
        }
    }
    // create a new dataframe out of each column of x,
    // subsetting each vector by idx
    return DataFrame::create(_["val"] = val[idx], _["id"] = x_ids[idx]);
}

/*** R
x = data.frame(val = 1:10, id = 1:10)
ids = 5:7
ex_fun(x, ids)
*/

