# tidytags

<details>

* Version: 1.1.0
* GitHub: https://github.com/ropensci/tidytags
* Source code: https://github.com/cran/tidytags
* Date/Publication: 2022-11-18 23:10:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::revdep_details(, "tidytags")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11.       └─rtweet:::TWIT_get(...)
       12.         └─rtweet:::TWIT_method(...)
       13.           └─httr::GET(url, query = params, token, ...)
       14.             └─httr:::request_perform(req, hu$handle$handle)
       15.               └─httr:::perform_callback("request", req = req)
       16.                 └─webmockr (local) callback(...)
       17.                   └─webmockr::HttrAdapter$new()$handle_request(req)
       18.                     └─private$request_handler(req)$handle()
       19.                       └─eval(parse(text = req_type_fun))(self$request)
       20.                         └─err$run()
       21.                           └─self$construct_message()
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 266 ]
      Error: Test failures
      Execution halted
    ```

