Feature: .emanoteignore (issue #228)
  Each notebook layer's top-level `.emanoteignore` excludes matching
  files from the model. These scenarios run in both live (`emanote
  run`) and static (`emanote gen`) modes — both code paths consult
  the same loader, so any divergence would surface here.

  We assert on the absence of a unique fixture marker rather than on
  HTTP status: live mode renders a "! Missing link" page with status
  200 for routes not in the model, while static mode returns 404 from
  serve-handler. Both agree that the excluded fixture's marker is
  absent.

  Scenario: A file pattern in .emanoteignore excludes the matching note
    When I fetch "/secret-ignored.html"
    Then the response body does not contain "EMANOTEIGNORE_REGRESSION_FILE"

  Scenario: A directory pattern in .emanoteignore excludes everything beneath it
    When I fetch "/private-dir/notes.html"
    Then the response body does not contain "EMANOTEIGNORE_REGRESSION_DIR"

  Scenario: The .emanoteignore file itself is not served as a static asset
    When I fetch "/.emanoteignore"
    Then the response body does not contain "EMANOTEIGNORE_REGRESSION_SELF"
