#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List unroll_connections(CharacterVector from, std::vector<std::vector<std::string> > to) {
  // set size paramaeters (exclude NAs from the 'to'-based output count)
  const int n = from.size();
  int len = 0;
  for (int i = 0; i < n; i++) {
    if (to[i][0] != "NA") {
      len += to[i].size();
    }
  }
  // use calculated lengths to initialize output character vectors
  CharacterVector from2(len);
  CharacterVector to2(len);

  // for each value of the 'from' vector, create appropriately re-sized from2
  // and to2 vectors
  int ctr = 0;
  for (int i = 0; i < n; i++) {
    int nn = to[i].size();
    for (int j = 0; j < nn; j++) {
      if (j == 0) {
        if (to[i][j] != "NA") {
          from2[ctr] = from[i];
          to2[ctr] = to[i][j];
          ctr += 1;
        }
      } else {
        from2[ctr] = from[i];
        to2[ctr] = to[i][j];
        ctr += 1;
      }
    }
  }
  // combine the new [flat] vectors into a data frame (requires row names)
  List df = List::create(_["from"] = from2, _["to"] = to2);
  df.attr("class") = "data.frame";
  df.attr("row.names") = seq(1, ctr);
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
