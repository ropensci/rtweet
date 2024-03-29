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


vcr::use_cassette("geo_objects1", {
  test_that("coordinates work", {

    minimal_coord <- structure(
      list(type = "Point",
           coordinates = list(c(-85.6445217, 42.9360473))),
      row.names = 45L, class = "data.frame")

    expect_error(out <- coordinates(minimal_coord), NA)
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 3)
    expect_equal(out$type[1], "Point")
  })
})
