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

  # Hot-reload (issue #739): each layer's .emanoteignore is read once
  # at process start in master; these scenarios prove that mid-session
  # edits to the file take effect without restarting emanote.

  @live @hot-reload
  Scenario: Removing a pattern from .emanoteignore re-includes the matching note (#739)
    # Baseline: secret-ignored.md is excluded by .emanoteignore.
    When I fetch "/secret-ignored.html"
    Then the response body does not contain "EMANOTEIGNORE_REGRESSION_FILE"
    # Drop the pattern; the note should re-appear without a restart.
    When I write the file ".emanoteignore" with:
    """
    # secret-ignored.md is now unfiltered — hot-reload should re-include it.
    private-dir/**
    """
    Then the URL "/secret-ignored.html" contains "EMANOTEIGNORE_REGRESSION_FILE" within 10 seconds

  @live @hot-reload
  Scenario: Adding a pattern to .emanoteignore hides the matching note (#739)
    # Start from a clean ignore file so secret-ignored.md is visible.
    When I write the file ".emanoteignore" with:
    """
    # no patterns
    """
    Then the URL "/secret-ignored.html" contains "EMANOTEIGNORE_REGRESSION_FILE" within 10 seconds
    # Add a pattern that excludes it; hot-reload should evict the note.
    When I write the file ".emanoteignore" with:
    """
    secret-ignored.md
    """
    Then the URL "/secret-ignored.html" stops containing "EMANOTEIGNORE_REGRESSION_FILE" within 10 seconds
