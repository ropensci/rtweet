Tests and Coverage
================
17 January, 2019 12:18:36

This output is created by
[covrpage](https://github.com/metrumresearchgroup/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                               | Coverage (%) |
| :--------------------------------------------------- | :----------: |
| carbonate                                            |    48.58     |
| [R/carbonate.R](../R/carbonate.R)                    |     0.00     |
| [R/selenium\_functions.R](../R/selenium_functions.R) |     0.00     |
| [R/uri\_functions.R](../R/uri_functions.R)           |    52.63     |
| [R/carbon.R](../R/carbon.R)                          |    64.29     |
| [R/helpers.R](../R/helpers.R)                        |    70.09     |
| [R/set\_get\_functions.R](../R/set_get_functions.R)  |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

| file                              | n |  time | error | failed | skipped | warning | icon |
| :-------------------------------- | -: | ----: | ----: | -----: | ------: | ------: | :--- |
| [test-set.R](testthat/test-set.R) | 3 | 0.013 |     0 |      0 |       0 |       0 |      |
| [test-uri.R](testthat/test-uri.R) | 8 | 1.352 |     0 |      0 |       1 |       0 | 🔶    |
| [test-yml.R](testthat/test-yml.R) | 6 | 0.017 |     0 |      0 |       0 |       0 |      |

<details open>

<summary> Show Detailed Test Results
</summary>

| file                                  | context | test                                        | status  | n |  time | icon |
| :------------------------------------ | :------ | :------------------------------------------ | :------ | -: | ----: | :--- |
| [test-set.R](testthat/test-set.R#L8)  | set\_   | set functions: set\_template                | PASS    | 1 | 0.011 |      |
| [test-set.R](testthat/test-set.R#L13) | set\_   | set functions: set\_font\_family            | PASS    | 1 | 0.001 |      |
| [test-set.R](testthat/test-set.R#L18) | set\_   | set functions: set\_windows\_control\_theme | PASS    | 1 | 0.001 |      |
| [test-uri.R](testthat/test-uri.R#L9)  | uri     | options: benchmark                          | PASS    | 1 | 0.002 |      |
| [test-uri.R](testthat/test-uri.R#L17) | uri     | uri: benchmark                              | PASS    | 1 | 0.002 |      |
| [test-uri.R](testthat/test-uri.R#L21) | uri     | uri: 200                                    | PASS    | 1 | 0.721 |      |
| [test-uri.R](testthat/test-uri.R#L27) | uri     | encode: encode character                    | PASS    | 1 | 0.002 |      |
| [test-uri.R](testthat/test-uri.R#L31) | uri     | encode: no encode character                 | PASS    | 1 | 0.001 |      |
| [test-uri.R](testthat/test-uri.R#L37) | uri     | tiny: valid tiny                            | PASS    | 1 | 0.620 |      |
| [test-uri.R](testthat/test-uri.R#L42) | uri     | tiny: clipboard                             | SKIPPED | 1 | 0.001 | 🔶    |
| [test-uri.R](testthat/test-uri.R#)    | uri     | bad template: error uri                     | PASS    | 1 | 0.003 |      |
| [test-yml.R](testthat/test-yml.R#L24) | yml     | yaml fields: rgba                           | PASS    | 1 | 0.006 |      |
| [test-yml.R](testthat/test-yml.R#L29) | yml     | yaml fields: template                       | PASS    | 1 | 0.003 |      |
| [test-yml.R](testthat/test-yml.R#L34) | yml     | yaml fields: bad font family                | PASS    | 1 | 0.002 |      |
| [test-yml.R](testthat/test-yml.R#L39) | yml     | yaml fields: pv                             | PASS    | 1 | 0.002 |      |
| [test-yml.R](testthat/test-yml.R#L44) | yml     | yaml fields: ph                             | PASS    | 1 | 0.002 |      |
| [test-yml.R](testthat/test-yml.R#L59) | yml     | namesless palette: fill in palette          | PASS    | 1 | 0.002 |      |

| Failed | Warning | Skipped |
| :----- | :------ | :------ |
| 🛑      | ⚠️      | 🔶       |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                               |
| :------- | :---------------------------------- |
| Version  | R version 3.5.1 (2018-07-02)        |
| Platform | x86\_64-apple-darwin15.6.0 (64-bit) |
| Running  | macOS 10.14.2                       |
| Language | en\_US                              |
| Timezone | America/Chicago                     |

| Package  | Version    |
| :------- | :--------- |
| testthat | 2.0.0.9000 |
| covr     | 3.2.0      |
| covrpage | 0.0.69     |

</details>

<!--- Final Status : skipped/warning --->
