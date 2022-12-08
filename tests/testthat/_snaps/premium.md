# search_fullarchive works

    Code
      df <- search_fullarchive(q = "#covid place:UK OR place:GB OR place:\"United Kindom\"",
        n = 20, env_name = "fullArchive", fromDate = "201810010000")
    Condition
      Warning:
      Unidentified value: edit_history, edit_controls, editable.
      	Please open an issue to notify the maintainer. Thanks!
      Warning:
      Unidentified value: edit_history, edit_controls, editable.
      	Please open an issue to notify the maintainer. Thanks!

# search_fullarchive queries bigger than page size work

    Code
      df <- search_fullarchive(q = "#covid place:UK OR place:GB OR place:\"United Kindom\"",
        n = 20, env_name = "fullArchive", fromDate = "201810010000")
    Condition
      Warning:
      Unidentified value: edit_history, edit_controls, editable.
      	Please open an issue to notify the maintainer. Thanks!
      Warning:
      Unidentified value: edit_history, edit_controls, editable.
      	Please open an issue to notify the maintainer. Thanks!

