letters_list <- list(a = "A", b = "B", c = "C")
letters_list_with_NULL <- list(a = "A", b = "B", c = "C", null = NULL)
test_that("drop_nulls operates properly on list", {
  expect_equal(
    drop_nulls(letters_list_with_NULL),
    letters_list
  )
  expect_equal(
    drop_nulls(letters_list_with_NULL[c("a", "null")]),
    letters_list_with_NULL["a"]
  )
  expect_equal(
    drop_nulls(letters_list_with_NULL["null"]),
    NULL
  )
  expect_equal(
    drop_nulls(list()),
    NULL
  )
})

test_that("as_json attaches json class to string", {
  json_string <- as_json("hello")
  raw_json <- '{"msg":hello}'
  class(raw_json) <- "json"

  expect_s3_class(
    json_string,
    "json"
  )
  expect_equal(
    jsonlite::toJSON(list(msg = json_string), json_verbatim = TRUE),
    raw_json
  )
})

test_that("make_verbatim converts strings to json objects", {
  character_list <- list(fun1 = "function(x) x", fun2 = "function(x) x")
  json_list <- list(fun1 = "function(x) x", fun2 = as_json("function(x) x"))
  expect_equal(
    make_verbatim(character_list),
    character_list
  )
  expect_equal(
    make_verbatim(character_list, which = "fun2"),
    json_list
  )
})

test_that("hypothesisBranding returns only non-missing args", {
  twoArgsList <- list(accentColor = "#fff", ctaBackgroundColor = "#000")
  expect_equal(
    hypothesisBranding(accentColor = "#fff"),
    twoArgsList["accentColor"]
  )
  expect_equal(
    hypothesisBranding(accentColor = "#fff", ctaBackgroundColor = "#000"),
    twoArgsList
  )
  expect_equal(
    hypothesisBranding(),
    NULL
  )
})

test_that("hypothesisServices returns only non-missing args and converts json", {
  twoArgsList <- list(apiUrl = "https://example.com", onLoginRequest = "function(x) x")
  onLoginRequestJson <- list(onLoginRequest = 'function(x) x')
  class(onLoginRequestJson[[1]]) <- c("json", "character")
  expect_equal(
    hypothesisServices(apiUrl = "https://example.com"),
    twoArgsList["apiUrl"]
  )
  # check if json type args are converted to raw json
  expect_equal(
    hypothesisServices(onLoginRequest = "function(x) x"),
    onLoginRequestJson
  )
  expect_equal(
    hypothesisServices(),
    NULL
  )
})

test_that("hypothesisOnOff returns html button with valid attributes", {
  h_onoff_button <- hypothesisOnOff(initShow = TRUE)
  h_onoff_attrs <- h_onoff_button$attribs
  expect_s3_class(h_onoff_button, "shiny.tag")
  expect_equal(h_onoff_button$name, "button")
  expect_match(h_onoff_attrs$class, "hi-onoff")
  expect_match(h_onoff_attrs$`data-visible`, "yes")

  h_onoff_button_hidden <- hypothesisOnOff(initShow = FALSE)
  h_onoff_hidden_attrs <- h_onoff_button_hidden$attribs
  expect_match(h_onoff_hidden_attrs$`data-visible`, "no")
})

test_that("to_json converts list elements into list", {
  list_obj <- list(a = 1, b = 2, null = NULL)
  nested_list_obj <- list(a = 1, b = list(2), null = NULL)
  expect_equal(
    to_list(list_obj, "b"),
    nested_list_obj
  )
  # keeping NULL unchanged
  expect_equal(
    to_list(list_obj, "null"),
    list_obj
  )
})

test_that("useHypothesis is tag list with attributes", {
  expect_s3_class(useHypothesis(), "shiny.tag.list")
  expect_equal(
    useHypothesis()[[1]]$name,
    "hypothesis"
  )
})
