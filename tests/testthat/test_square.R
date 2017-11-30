context("Squaring non-numerics")

test_that("At least numeric values work.", {
	num_vec <- c(0, -4.6, 3.4)
	expect_identical(square(numeric(0)), numeric(0))
	expect_identical(square(num_vec), num_vec^2)
})

test_that("Logicals automatically convert to numeric.", {
	logic_vec <- c(TRUE, TRUE, FALSE)
	expect_identical(square(logic_vec), logic_vec^2)
})

context("Reciprocal")

test_that("Numeric works!", {
	numeric_vector <- c(2, 4, 7)
	expect_identical(reciprocal(numeric_vector), 1/numeric_vector)
	expect_error(reciprocal("Anh"))
})


Context("Link")

test_that("Link works!", {
	string_vector <- c("Anh", "Khoa", "Vo")
	expect_identical(link(string_vector), c("AnhAnh", "KhoaKhoa", "VoVo"))
	expect_error(link(NA))
})

Context("Boxcox")

test_that("Boxcox works!", {
	numeric_0_vector <- c(10,0)
	numeric_vector <- c(10,3)
	expect_identical(boxcox(numeric_0_vector), log(10))
	expect_identical(boxcox(numeric_vector), (10^2)/3)
	expect_error(boxcox("Anh", "Vo"))
})


Context("Reverse Boxcox")

test_that("Reverse Boxcox works!", {
	numeric_0_vector <- c(10,0)
	numeric_vector <- c(10,3)
	expect_identical(reverse_boxcox(numeric_0_vector), exp(10))
	expect_identical(reverse_boxcox(numeric_vector), (10*3+1)^(1/3))
	expect_error(reverse_boxcox("Anh", "Vo"))
})
