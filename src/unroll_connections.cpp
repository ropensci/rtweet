#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List unroll_connections(const CharacterVector& from,
                        const List& to) {
  // set size parameters (exclude NAs from the 'to'-based output count)
  const int n = from.size();
  R_xlen_t len = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const CharacterVector to_i = to[i];
    
    for (R_xlen_t j = 0; j < to_i.size(); ++j) {
      if (to_i[j].get() != R_NaString) {
        len++;
      }
    }
    
  }
  
  // use calculated lengths to initialize output character vectors
  CharacterVector from2(len); 
  CharacterVector to2(len); 
  
  // for each value of the 'from' vector, create appropriately re-sized from2
  // and to2 vectors
  R_xlen_t k = 0; // `k` is current index of `from2`/`to2`
  for (R_xlen_t i = 0; i < n; ++i) { 
    // `i` is index of current `to`
    const CharacterVector to_i = to[i];
    // `j` is index of current `to[i]`
    for (R_xlen_t j = 0; j < to_i.size(); ++j) {
      
      if (to_i[j].get() != R_NaString) {
        from2[k] = from[i];
        to2[k] = to_i[j];
        k++;
      }
      
    }
    
  }
  // combine the new [flat] vectors into a data frame (requires row names)
  List df = List::create(_["from"] = from2, _["to"] = to2);
  df.attr("row.names") = seq_len(len);
  df.attr("class") = "data.frame";
  return df;
}




// [[Rcpp::export]]
CharacterVector unroll_users(const List& x) {
  // use second column in each element to determine output size
  R_xlen_t n = x.size();
  R_xlen_t len = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const CharacterVector x_i = x[i];
    if (x_i[0].get() != R_NaString) {
      len += x_i.size();
    }
  }
  // use calculated length to initialize IDs vector
  CharacterVector ids(len);
  
  // for each value of the 'from' vector, create appropriately re-sized from2
  // and to2 vectors
  R_xlen_t ctr = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const CharacterVector x_i = x[i];
    R_xlen_t nn = x_i.size();
    for (R_xlen_t j = 0; j < nn; ++j) {
      if (j == 0) {
        if (x_i[j].get() != R_NaString) {
          ids[ctr] = x_i[j];
          ctr++;
        }
      } else {
        ids[ctr] = x_i[j];
        ctr++;
      }
    }
  }
  //Function f("unique");
  return ids;
}
