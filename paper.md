---
title: "rtweet: Collecting and analyzing Twitter data"
authors:
  - name: Michael W. Kearney
    orcid: 0000-0002-0730-4694
    affiliation: '1'
affiliations:
  - name: School of Journalism, Informatics Institute, University of Missouri
    index: '1'
date: 13 May 2019
bibliography: paper.bib
tags:
  - R
  - twitter
  - social media
  - API
---

# Statement of need

Following the [announced (2016) deprecation of the ``twitteR``
package](https://github.com/ropensci/rtweet/issues/1#issuecomment-492753003)
[@twitteR], R users seeking to interact with Twitter APIs have been encouraged
to use the ``rtweet`` package. Use of the up-to-date and actively-maintained
``rtweet`` package is especially important in light of changes to Twitter's APIs
since 2016. Most notably is the increased character limit (from 140 to 280) for
Twitter statuses [@tweetmodeextended]. In addition to providing an updated
interface with similar functionality to ``twitteR``, allowing R users to
communicate with various endpoints from Twitter's REST API, the ``rtweet``
package also provides support for communicating with Twitter's stream API.

# Summary

Interest in Twitter data continues to grow, but for many the task of actually
collecting and analyzing data via Twitter APIs remains daunting. For example, in
order to interact with Twitter's APIs users must, in addition to identifying and
digesting the relevant information from [Twitter's developer
documentation](https://developer.twitter.com), build/send/receive requests,
manage rate limits, and wrangle nested and real-time response objects into
analysis-friendly data structures. Fortunately, the ``rtweet`` R package [@rtweet]
is designed to simplify these processes, making interacting with Twitter's APIs
more accessible to a wider range of users.

The main goals of the ``rtweet`` package are two-fold. The first goal is to make
interacting with Twitter's APIs more approachable and streamlined for less
computationally-experienced users. The second goal is to assist in the analysis
of Twitter data via converting information returned by Twitter's APIs into
tabular data structures and providing several convenience functions for common
analytical techniques such as examining Twitter networks or the frequency of
tweets over time. In short, although it is certainly possible for users to write
their own Twitter API wrapper functions, the heavy-lifting done by ``rtweet`` to
(a) streamline the building, authorizing, and sending of API requests, (b)
wrangle deeply nested JSON data into tabular structures, and (c) provide
convenience functions for relevant and popular analytical techniques, make
it a valuable contribution in the area of collecting and analyzing Twitter data.

Although ``rtweet`` provides some coverage to user context-behaviors (e.g.,
posting statuses, liking tweets, following users, etc.), the primary audience
for the package to date has been researchers. Accordingly, ``rtweet`` has been
featured in numerous popular press [e.g.,
@bajak2019democrats; @machlis2019r; @riley2019twitter] and academic publications
[e.g.,
@bossetta2018simulated; @bradley2019major; @buscema2018media;
@erlandsen2018twitter; @gitto2019brand; @kearney2019analyzing;
@kearney2018analyzing; @li2019sentiment; @lutkenhaus2019tailoring;
@lutkenhaus2019mapping; @molyneux2018media; @tsoi2018can; @unsihuay2018topic;
@valls2017urban; @wu2018finding].

# References


