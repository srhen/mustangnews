test_that("slopd_log works", {

  log <- slopd_log()
  my_result <- length(log)

  expect_equal(my_result, 24)
})

test_that("upd_log works", {

  log <- upd_log()
  my_result <- length(log)

  expect_equal(my_result, 12)
})

test_that("get_gl_sanctions works", {

  log <- get_gl_sanctions()
  my_result <- length(log)

  expect_equal(my_result, 7)
})

test_that("check_county_names works", {

  names <- check_county_names(median_income)
  my_result <- length(names)

  expect_equal(my_result, 2)

  names2 <- check_county_names(median_income, state = "NM")
  my_result2 <- length(names2)

  expect_equal(my_result2, 4)

})

test_that("simple_line and web_theme works", {

  graph <- simple_line(stalking_3, title = "test", subtitle = "test",
                       caption = "test") +
    set_colors("mn", "line", red, navy, light_green, yellow) +
    web_theme()
  my_result <- graph$labels$linetype

  expect_equal(my_result, "University")

})

test_that("simple_bar and print_theme works", {

  graph <- simple_bar(ad_spending_total, title = "test", subtitle = "test",
                       caption = "test") +
    set_colors("cp", "line", cp_green, cp_gold) +
    print_theme()
  my_result <- graph$theme$plot.title$family

  expect_equal(my_result, "Soleil")

})





