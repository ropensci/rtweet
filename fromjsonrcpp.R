

library(Rcpp)
cppFunction('List CheapDataFrameBuilder(List a) {
    List returned_frame = clone(a);
  GenericVector sample_row = returned_frame(0);

  StringVector row_names(sample_row.length());
  for (int i = 0; i < sample_row.length(); ++i) {
  char name[5];
  sprintf(&(name[0]), "%d", i);
  row_names(i) = name;
  }
  returned_frame.attr("row.names") = row_names;

  StringVector col_names(returned_frame.length());
  for (int j = 0; j < returned_frame.length(); ++j) {
  char name[6];
  sprintf(&(name[0]), "X.%d", j);
  col_names(j) = name;
  }
  returned_frame.attr("names") = col_names;
  returned_frame.attr("class") = "data.frame";

  return returned_frame;
  }')


CheapDataFrameBuilder()

rtweet::stream_tweets(timeout = 3, parse = FALSE, file_name = "rcpp.json")
x <- readLines("rcpp.json")
xn <- grep("created_at", x)
xn <- xn[length(xn)]
x <- substr(x, 1, (xn - 3))

