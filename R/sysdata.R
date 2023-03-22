# Helper to explain the sydata.rda file
# This is a file with two R objects:
# citycoords: A tibble used by lookup_coords (and tested in test-coords.)
# sysdat: A list used by trends and auth clients
#  - woeid
#  - DYK, MRsn: used to set up default client with oauth 1.0:
#     - This account was created by Michael W. Kearney and it is now lost
#  - d55, e914: used to set up default client with oauth 2.0:
#     - This account was created by me:
#        - id: 1355953759982120964
#        - account username: app_rtweet
#        - name: rtweet_app
# Each client is hidden using openssl::encrypt(secret, rsa-key)
# Just in case I always use the same rsa-key as in DYK... and MRsn...
# The key is the first element of DYK, MRsn, d55, e914
# The names are a section for md5sum from something.
