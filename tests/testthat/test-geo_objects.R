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
    
    # ids of these "faulty" ids: 
    ids <- c("1462911176719757313", "1462903173656428545", 
             "1462902964150935558", "1462899130808762371")
    # other_ids adjacent (w-1):
    other_ids <- c("1462911347801444365", "1462903930090840071", 
                   "1462903173656428545", "1462900536848490499")
    all_ids <- unique(c(ids, other_ids))
    expect_error(lu <- lookup_tweets(all_ids), NA)
    expect_equal(length(lu$coordinates), nrow(lu))
    expect_equal(ncol(lu$coordinates[[1]]), 3)
    expect_named(lu$coordinates[[1]], c("long", "lat", "type"))
    # The tweet might have been deleted 
    if ("1462903173656428545" %in% lu$id_str) {
      expect_equal(lu$coordinates[[which(lu$id_str == "1462903173656428545")]]$type, "Point")
    }  
  })
})
