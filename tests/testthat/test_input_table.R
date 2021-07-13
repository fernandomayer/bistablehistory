context("Function inputs")
library(bistablehistory)

## --- 1. Check if data is a data.frame (or something that is convertible to it) ---
test_that("data is a data.frame", {
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                         onset=c(0, 2, 4))
  # invalid
  expect_error(fit_cumhist(data=as.list(test_df), state="state", onset="onset"))
  expect_error(fit_cumhist(test_df$onset, state="state", onset="onset"))
})

# --- 2. Check the state column. ---
test_that("state is a valid column", {
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        onset=c(0, 2, 4))

  # invalid: missing column name
  expect_error(fit_cumhist(data=test_df, onset="onset"))

  # invalid: wrong column name
  expect_error(fit_cumhist(data=test_df, state="State", onset="onset"))

  # NAs
  test_df <- data.frame(state=c("L", "R", NA),
                        onset=c(0, 2, 4))
  expect_error(fit_cumhist(data=test_df, state="state", onset="onset"))

  # invalid: too few factor levels
  test_df <- data.frame(state=factor(c("L", "L", "L")),
                        onset=c(0, 2, 4))
  expect_error(fit_cumhist(data=test_df, state="State", onset="onset"))

  # invalid: too many factor levels
  test_df <- data.frame(state=factor(c("L", "R", "M", "T")),
                            onset=c(0, 2, 4, 6))
  expect_error(fit_cumhist(data=test_df, state="State", onset="onset"))

  # invalid: not a factor and not 2 unique values
  test_df <- data.frame(state=c("L", "R", "M"),
                            onset=c(0, 2, 4))
  expect_error(fit_cumhist(data=test_df, state="State", onset="onset"))
})

## --- 3. Check that either duration or onset a supplied ---
test_that("Duration or onset are supplied", {
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        onset=c(0, 2, 4))

  # neither onset nor duration
  expect_error(fit_cumhist(data=test_df, state="state"))

  # invalid name for onset column
  expect_error(fit_cumhist(data=test_df, state="state", onset="Onset"))

  # onset column has NAs
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        onset=c(0, NA, 4))
  expect_error(fit_cumhist(data=test_df, state="state", onset="onset"))


  # onset column is factor
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        onset=factor(c(0, 2, 4)))
  expect_error(fit_cumhist(data=test_df, state="state", onset="onset"))

  # onset column is string
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        onset=as.character(c(0, 2, 4)))
  expect_error(fit_cumhist(data=test_df, state="state", onset="onset"))

  # onset column is logical
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        onset=as.logical(c(0, 2, 4)))
  expect_error(fit_cumhist(data=test_df, state="state", onset="onset"))

  # invalid name for duration column
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        duration=c(2, 3, 1))
  expect_error(fit_cumhist(data=test_df, state="state", duration="Duration"))

  # duration column is factor
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        duration=factor(c(2, 3, 1)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration"))

  # duration column is string
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        duration=as.character(c(2, 3, 1)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration"))

  # duration column is logical
  test_df <- data.frame(state=factor(c("L", "R", "M")),
                        duration=as.logical(c(2, 3, 1)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration"))

  # duration column has negative values
  test_df <- data.frame(state=factor(c("L", "R", "L", "R", "L")),
                        duration=c(2, 3, 1, -1, 2))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration"))

  # duration column has zero values for clear percept
  test_df <- data.frame(state=factor(c("L", "R", "L", "M", "L"), levels = c("L", "R", "M")),
                        duration=c(2, 3, 0, 2, 2))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration"))
})

# --- 4. Check the participant column. ---
test_that("participant is a valid column", {
  test_df <- data.frame(state=factor(c("L", "R", "L", "R", "L")),
                        duration=c(2, 3, 1, 3, 2),
                        participant=c("A", "A", "A", NA, NA))

  # invalid column name
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", participant="Participant"))

  # cannot be converted to factor
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", participant="participant"))
})

# --- 5. Check the session column. ---
test_that("session is a valid column", {
  test_df <- data.frame(state=factor(c("L", "R", "L", "R", "L")),
                        duration=c(2, 3, 1, 3, 2),
                        session=c("A", "A", "A", NA, NA))

  # invalid column name
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", session="Session"))

  # cannot be converted to factor
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", session="session"))
})


# --- 6. Check the run column. ---
test_that("run is a valid column", {
  test_df <- data.frame(state=factor(c("L", "R", "L", "R", "L")),
                        duration=c(2, 3, 1, 3, 2),
                        run=c("A", "A", "A", NA, NA))

  # invalid column name
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", run="Run"))

  # cannot be converted to factor
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", run="run"))
})

# --- 7. Check norm_tau ---
test_that("norm_tau", {
  test_df <- data.frame(state=factor(c("L", "R", "L", "R", "L")),
                        duration=c(2, 3, 1, 3, 2))

  # negative fixed tau
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = -1))

  # zero fixed tau
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = 0))

  # non-numeric tau
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = "0"))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = c("1", "1")))

  # three element vector
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = c(1, 1, 1)))

  # negative priors or zero priors
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = c(0, 1)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = c(1, 0)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = c(-1, 1)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", norm_tau = c(1, -1)))
})

# --- 8. Check mixed_state ---
test_that("mixed_state", {
  test_df <- data.frame(state=factor(c("L", "R", "L", "R", "L")),
                        duration=c(2, 3, 1, 3, 2))

  # fixed mixed state outside of [0..1]
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = -1))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = 1.01))

  # non-numeric
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = "0.5"))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = c("0.5", 1)))

  # three element vector
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = c(0.5, 10, 10)))

  # prior values out of range
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = c(-0.1, 10)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = c(1.1, 10)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = c(0.5, 0)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", mixed_state = c(0.5, -1)))
})

# --- 9. Check history mixture ---
test_that("history_mixture", {
  test_df <- data.frame(state=factor(c("L", "R", "L", "R", "L")),
                        duration=c(2, 3, 1, 3, 2))

  # fixed mixed state outside of [0..1]
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = -1))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = 1.01))

  # non-numeric
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = "0.5"))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = c("0.5", 1)))

  # three element vector
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = c(0.5, 10, 10)))

  # prior values out of range
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = c(-0.1, 10)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = c(1.1, 10)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = c(0.5, 0)))
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", history_mixture = c(0.5, -1)))
})

# --- 9. Check family ---
test_that("family", {
  test_df <- data.frame(state=factor(c("L", "R", "L", "R", "L")),
                        duration=c(2, 3, 1, 3, 2))

  # unsupported distribution
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", family = "weibull"))

  # misspelled distribution (wrong case)
  expect_error(fit_cumhist(data=test_df, state="state", duration="duration", family = "LogNormal"))
})
