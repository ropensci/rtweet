
test_that("hashtags works", {
  hashtags <- jsonlite::fromJSON('{
  "hashtags": [
    {
      "indices": [
        32,
        38
      ],
      "text": "nodejs"
    }
  ]
}')
  out <- hashtags(hashtags$hashtags)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("text", "indices"))
  expect_length(out$indices[[1]], 2)
})




test_that("media works", {
  extended_media <- jsonlite::fromJSON('{
"extended_entities": {
    "media": [
      {
        "id": 861627472244162561,
        "id_str": "861627472244162561",
        "indices": [
          68,
          91
        ],
        "media_url": "http://pbs.twimg.com/media/C_UdnvPUwAE3Dnn.jpg",
        "media_url_https": "https://pbs.twimg.com/media/C_UdnvPUwAE3Dnn.jpg",
        "url": "https://t.co/9r69akA484",
        "display_url": "pic.twitter.com/9r69akA484",
        "expanded_url": "https://twitter.com/FloodSocial/status/861627479294746624/photo/1",
        "type": "photo",
        "sizes": {
          "medium": {
            "w": 1200,
            "h": 900,
            "resize": "fit"
          },
          "small": {
            "w": 680,
            "h": 510,
            "resize": "fit"
          },
          "thumb": {
            "w": 150,
            "h": 150,
            "resize": "crop"
          },
          "large": {
            "w": 2048,
            "h": 1536,
            "resize": "fit"
          }
        }
      },
      {
        "id": 861627472244203520,
        "id_str": "861627472244203520",
        "indices": [
          68,
          91
        ],
        "media_url": "http://pbs.twimg.com/media/C_UdnvPVYAAZbEs.jpg",
        "media_url_https": "https://pbs.twimg.com/media/C_UdnvPVYAAZbEs.jpg",
        "url": "https://t.co/9r69akA484",
        "display_url": "pic.twitter.com/9r69akA484",
        "expanded_url": "https://twitter.com/FloodSocial/status/861627479294746624/photo/1",
        "type": "photo",
        "sizes": {
          "small": {
            "w": 680,
            "h": 680,
            "resize": "fit"
          },
          "thumb": {
            "w": 150,
            "h": 150,
            "resize": "crop"
          },
          "medium": {
            "w": 1200,
            "h": 1200,
            "resize": "fit"
          },
          "large": {
            "w": 2048,
            "h": 2048,
            "resize": "fit"
          }
        }
      },
      {
        "id": 861627474144149504,
        "id_str": "861627474144149504",
        "indices": [
          68,
          91
        ],
        "media_url": "http://pbs.twimg.com/media/C_Udn2UUQAADZIS.jpg",
        "media_url_https": "https://pbs.twimg.com/media/C_Udn2UUQAADZIS.jpg",
        "url": "https://t.co/9r69akA484",
        "display_url": "pic.twitter.com/9r69akA484",
        "expanded_url": "https://twitter.com/FloodSocial/status/861627479294746624/photo/1",
        "type": "photo",
        "sizes": {
          "medium": {
            "w": 1200,
            "h": 900,
            "resize": "fit"
          },
          "small": {
            "w": 680,
            "h": 510,
            "resize": "fit"
          },
          "thumb": {
            "w": 150,
            "h": 150,
            "resize": "crop"
          },
          "large": {
            "w": 2048,
            "h": 1536,
            "resize": "fit"
          }
        }
      },
      {
        "id": 861627474760708096,
        "id_str": "861627474760708096",
        "indices": [
          68,
          91
        ],
        "media_url": "http://pbs.twimg.com/media/C_Udn4nUMAAgcIa.jpg",
        "media_url_https": "https://pbs.twimg.com/media/C_Udn4nUMAAgcIa.jpg",
        "url": "https://t.co/9r69akA484",
        "display_url": "pic.twitter.com/9r69akA484",
        "expanded_url": "https://twitter.com/FloodSocial/status/861627479294746624/photo/1",
        "type": "photo",
        "sizes": {
          "small": {
            "w": 680,
            "h": 680,
            "resize": "fit"
          },
          "thumb": {
            "w": 150,
            "h": 150,
            "resize": "crop"
          },
          "medium": {
            "w": 1200,
            "h": 1200,
            "resize": "fit"
          },
          "large": {
            "w": 2048,
            "h": 2048,
            "resize": "fit"
          }
        }
      }
    ]
  }
}')
  out <- media(extended_media$extended_entities$media)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("id", "id_str", "indices", "media_url", "media_url_https", 
                      "url", "display_url", "expanded_url", "type", "sizes", 
                      "ext_alt_text"))
})

  


test_that("polls works", {
  polls_media <- jsonlite::fromJSON('{"polls": [
      {
        "options": [
          {
            "position": 1,
            "text": "I read documentation once."
          },
          {
            "position": 2,
            "text": "I read documentation twice."
          },
          {
            "position": 3,
            "text": "I read documentation over and over again."
          }
        ],
        "end_datetime": "Thu May 25 22:20:27 +0000 2017",
        "duration_minutes": 60
      }
    ]
  }')
  out <- polls(polls_media$polls)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("options", "end_datetime", "duration_minutes"))
})
