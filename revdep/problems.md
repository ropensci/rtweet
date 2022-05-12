# bdpar

<details>

* Version: 3.0.1
* GitHub: https://github.com/miferreiro/bdpar
* Source code: https://github.com/cran/bdpar
* Date/Publication: 2021-06-24 11:10:02 UTC
* Number of recursive dependencies: 74

Run `revdep_details(, "bdpar")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘test_all.R’
     ERROR
    Running the tests in ‘tests/test_all.R’ failed.
    Last 13 lines of output:
      Error: 'emojis' is not an exported object from 'namespace:rtweet'
      Backtrace:
          ▆
       1. └─pipe$pipe(instance) at test_FindEmojiPipe.R:152:2
       2.   └─base::as.list(rtweet::emojis[2][[1]])
      ── Error (test_FindEmojiPipe.R:187:3): pipe replaceEmojis <- FALSE ─────────────
      Error: 'emojis' is not an exported object from 'namespace:rtweet'
      Backtrace:
          ▆
       1. └─pipe$pipe(instance) at test_FindEmojiPipe.R:187:2
       2.   └─base::as.list(rtweet::emojis[2][[1]])
      
      [ FAIL 2 | WARN 0 | SKIP 5 | PASS 545 ]
      Error: Test failures
      Execution halted
    ```

