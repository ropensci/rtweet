#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List unroll_connections(const CharacterVector& from,
                        const List& to) {
  // set size paramaeters (exclude NAs from the 'to'-based output count)
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
  Rcpp::CharacterVector from2(len); 
  Rcpp::CharacterVector to2(len); 
  
  // for each value of the 'from' vector, create appropriately re-sized from2
  // and to2 vectors
  R_xlen_t k = 0; // `k` is current index of `from2`/`to2`
  for (R_xlen_t i = 0; i < n; ++i) { 
    // `i` is index of current `to`
    const CharacterVector to_i = to[i];
    // `j` is index of current `to[i]`
    for (int j = 0; j < to_i.size(); ++j) {
      
      if (to_i[j].get() != R_NaString) {
        from2[k] = from[i];
        to2[k] = to_i[j];
        k++;
      }
      
    }
    
  }
  // combine the new [flat] vectors into a data frame (requires row names)
  List df = List::create(_["from"] = from2,_["to"] = to2);
  df.attr("row.names") = seq_len(len);
  df.attr("class") = "data.frame";
  return df;
}




// [[Rcpp::export]]
std::vector<std::string> unroll_users(std::vector<std::vector<std::string> > x) {
  // use second column in each element to determine output size
  int n = x.size();
  int len = 0;
  for (int i = 0; i < n; i++) {
    if (x[i][0] != "NA") {
      len += x[i].size();
    }
  }
  // use calculated length to initialize IDs vector
  std::vector<std::string> ids(len);

  // for each value of the 'from' vector, create appropriately re-sized from2
  // and to2 vectors
  int ctr = 0;
  for (int i = 0; i < n; i++) {
    int nn = x[i].size();
    for (int j = 0; j < nn; j++) {
      if (j == 0) {
        if (x[i][j] != "NA") {
          ids[ctr] = x[i][j];
          ctr += 1;
        }
      } else {
        ids[ctr] = x[i][j];
        ctr += 1;
      }
    }
  }
  //Function f("unique");
  return ids;
}
