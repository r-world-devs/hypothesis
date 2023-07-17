test_that("Check get_hs_host", {
  withr::with_envvar(
    c("HYPOTHESIS_API_PATH" = "test"),
    expect_equal(get_hs_host(), "test")
  )
})

test_that("Check hs_api_handler", {

  # Test other type of handler
  expect_error(hs_api_handler(type = "ABC"), "object 'ABC' not found")

  # Test handle api errors
  mockery::stub(hs_api_handler, "get", function(...) {function(...) {404L}})
  mockery::stub(hs_api_handler, "httr::content", function(...) {"error 404L"})

  expect_error(hs_api_handler(type = "GET"), "error 404L")

  # Test json output
  mockery::stub(hs_api_handler, "get", function(...) {function(...) {200L}})
  mockery::stub(hs_api_handler, "httr::content", function(...) {'{"json_output": 1}'})

  expect_identical(hs_api_handler(type = "GET"), list(json_output = 1L))

  # Test text output
  mockery::stub(hs_api_handler, "get", function(...) {function(...) {200L}})
  mockery::stub(hs_api_handler, "httr::content", function(...) {"content is text output"})

  expect_identical(hs_api_handler(type = "GET"), "content is text output")

})

test_that("Check search_annotations", {

  # Test if args was removed functions calls
  mockery::stub(search_annotations, "hs_api_handler", function(query, ...) {stop(query)})
  expect_error(search_annotations(limit = NULL, sort = "id", offset = NULL, order = "desc"), "iddesc")
  expect_error(search_annotations(limit = 100, sort = "id", offset = NULL, order = "asc"), "100idasc")

  # Test if the row elements is return from api answer
  mockery::stub(search_annotations, "hs_api_handler", function(...) {list(rows = iris)})
  expect_equal(search_annotations(), iris)
})

test_that("Check annotation", {

  # Test if type is correctly in corresponding to request action
  mockery::stub(annotation, "hs_api_handler", function(type, ...) {stop("Type is: ", type)})
  expect_error(annotation("anno_id"), "Type is: GET")
  expect_error(annotation("anno_id", action = "flag"), "Type is: PUT")

  # Test that function return output
  mockery::stub(annotation, "hs_api_handler", function(...) {iris})
  expect_equal(annotation("iris"), iris)
})

test_that("Check get_groups", {
  # Test if args was removed functions calls when NULL
  mockery::stub(get_groups, "hs_api_handler", function(query, ...) {stop(query)})
  expect_error(get_groups(authority = "error"), "error")
  expect_error(get_groups(authority = "error", document_uri = "abc"), "errorabc")

  # Test that function return output
  mockery::stub(get_groups, "hs_api_handler", function(...) {iris})
  expect_equal(get_groups(), iris)

})

test_that("Check group", {

  # Test if type is correctly in corresponding to request action
  mockery::stub(group, "hs_api_handler", function(type, ...) {stop("Type is: ", type)})
  expect_error(group("anno_id"), "Type is: GET")
  expect_error(group("anno_id", action = "update"), "Type is: PATCH")

  # Test that function return output
  mockery::stub(group, "hs_api_handler", function(...) {iris})
  expect_equal(group("iris"), iris)
})

test_that("Check get_profile", {

  # Test that function return output
  mockery::stub(get_profile, "hs_api_handler", function(...) {iris})
  expect_equal(get_profile(), iris)

  # Test if api_key was pass to hs_api_handler
  mockery::stub(get_profile, "hs_api_handler", function(..., api_key) {stop(api_key)})
  expect_error(get_profile(api_key = "test_api_key"), "test_api_key")
})

test_that("Check get_profile_groups", {

  # Test that function return output
  mockery::stub(get_profile_groups, "hs_api_handler", function(...) {iris})
  expect_equal(get_profile_groups(), iris)
})
