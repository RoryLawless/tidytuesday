# Tidytuesday ------------------------------------------------------------
# Week 26, 2025
# Gasoline prices in the US

# Load required libraries
library(tidytuesdayR)
library(tidyverse)
library(readxl)
library(slider)
library(ragg)

# Load TidyTuesday data for week 26 of 2025
tuesdata <- tt_load(2025, week = 26)
weekly_gas_prices <- tuesdata$weekly_gas_prices

# Load Consumer Price Index (CPI) data from Excel file
# Consumer Price Index for All Urban Consumers (CPI-U)
# Gasoline, unleaded regular in U.S. city average, all urban consumers, not
# seasonally adjusted - 1991-2025
# Downloaded on 2025-07-01 from the BLS website
# Skip first 10 rows which contain metadata
cpi <- read_xlsx("data/SeriesReport-20250701222600_df0782.xlsx", skip = 10)

# Data cleaning ----------------------------------------------------------

# Clean CPI data by removing annual and half-year summary columns
cpi <- cpi |>
	select(-c(Annual, HALF1, HALF2))

# Transform CPI data from wide to long format
# Convert monthly columns (Jan-Dec) into rows for easier joining
cpi_long <- cpi |>
	pivot_longer(cols = Jan:Dec, names_to = "month", values_to = "cpi") |>
	mutate(
		year_month = str_c(Year, month, sep = "-"),
		year_month = ym(year_month)
	) |>
	select(-c(Year, month))

# Filter gas price data for analysis
# Focus on regular gasoline with all formulations from 1991 onwards
gas_all <- weekly_gas_prices |>
	filter(
		fuel == "gasoline" &
			grade == "regular" &
			formulation == "all" &
			year(date) >= 1991
	)

# Add year-month column for joining with CPI data
# Floor date to first day of month for consistent joining
gas_all <- gas_all |>
	mutate(year_month = floor_date(date, unit = "month"))

# Join gas prices with CPI data by year-month
gas_all <- gas_all |>
	left_join(cpi_long, by = join_by(year_month))

# Remove rows where CPI data is missing
# These are records in June 2025, which is not included in the CPI dataset
gas_all <- gas_all |>
	filter(!is.na(cpi))

# Get the most recent CPI value for inflation adjustment
# This will be used as the base for adjusting all historical prices
adjustment_cpi <- gas_all |>
	filter(date == max(date)) |>
	pull(cpi)

# Calculate inflation-adjusted prices and additional metrics
gas_all <- gas_all |>
	mutate(
		price_adjusted = round(price * (adjustment_cpi / cpi), 2),
		rolling_mean = slide_dbl(
			price_adjusted,
			mean,
			.before = 25,
			.complete = TRUE
		),
		difference = price_adjusted - lag(price_adjusted),
		difference = sign(difference),
		difference = if_else(difference == 0, NA_real_, difference)
	)

# Plot the data ----------------------------------------------------------

ggplot(gas_all) +
	geom_line(
		aes(
			x = date,
			y = rolling_mean,
			color = difference
		),
		linewidth = 1,
		na.rm = TRUE
	) +
	scale_y_continuous(
		labels = scales::dollar_format(scale = 1),
	) +
	scale_x_date(
		date_labels = "%Y",
		date_breaks = "3 years",
		date_minor_breaks = "1 year",
		expand = expansion(mult = c(0, 0))
	) +
	scale_color_gradient2(guide = NULL) +
	labs(
		x = NULL,
		y = NULL,
		title = "26-week moving average of regular gasoline prices",
		caption = "Inflation-adjusted, CPI May 2025.",
	) +
	theme_minimal() +
	theme(
		text = element_text(family = "Lato"),
		plot.caption.position = "plot",
		plot.title.position = "plot",
		plot.caption = element_text(size = 8)
	)

# Save the plot ----------------------------------------------------------

ggsave(
	"output/tt-2025-26.png",
	device = agg_png,
	width = 10,
	height = 6,
	dpi = "retina",
	bg = "white"
)
