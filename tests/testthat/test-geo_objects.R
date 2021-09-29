test_that("bounding_box works", {
  bb <- jsonlite::fromJSON('{
  "bounding_box": {
    "coordinates": [
      [
        [
          -74.026675,
          40.683935
        ],
        [
          -74.026675,
          40.877483
        ],
        [
          -73.910408,
          40.877483
        ],
        [
          -73.910408,
          40.3935
        ]
      ]
    ],
    "type": "Polygon"
  }
}')
  out <- bounding_box(bb$bounding_box)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("long", "lat", "type"))
  expect_equal(ncol(out), 3)
  expect_equal(nrow(out), 4)
})


test_that("place works", {
  exact_location <- jsonlite::fromJSON('{
  "geo": {
    "type": "Point",
    "coordinates": [
      40.74118764,
      -73.9998279
    ]
  },
  "coordinates": {
    "type": "Point",
    "coordinates": [
      -73.9998279,
      40.74118764
    ]
  },
  "place": {
    "id": "01a9a39529b27f36",
    "url": "https://api.twitter.com/1.1/geo/id/01a9a39529b27f36.json",
    "place_type": "city",
    "name": "Manhattan",
    "full_name": "Manhattan, NY",
    "country_code": "US",
    "country": "United States",
    "bounding_box": {
      "type": "Polygon",
      "coordinates": [
        [
          [
            -74.026675,
            40.683935
          ],
          [
            -74.026675,
            40.877483
          ],
          [
            -73.910408,
            40.877483
          ],
          [
            -73.910408,
            40.683935
          ]
        ]
      ]
    },
    "attributes": {
      
    }
  }
}')
  out <- place(exact_location)
  expect_s3_class(out, "data.frame")
  
})
