#!/usr/bin/env Rscript
# GrowthLens | E-commerce revenue and customer analytics
# Requires base R >= 4.1. No packages or API keys required.
# Real public data: Olist Brazilian E-commerce Dataset, CC BY-NC-SA 4.0.
# Usage from terminal: Rscript growthlens.R
# Usage from RStudio: open growthlens.R then click Source.

options(stringsAsFactors = FALSE, scipen = 999)
SEED <- 2026L
BOOTSTRAPS <- 300L
COHORT_MAX_MONTHS <- 12L
CONFIDENCE <- 0.95

# Resolve the file's directory instead of depending on the launch directory.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep('^--file=', args, value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub('^--file=', '', file_arg[1L]), mustWork = TRUE))
} else if (!is.null(sys.frame(1)$ofile)) {
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = TRUE))
} else {
  getwd()
}
raw_dir <- file.path(script_dir, 'data')
out_dir <- file.path(script_dir, 'outputs')
plot_dir <- file.path(out_dir, 'plots')
for (dir in c(raw_dir, out_dir, plot_dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

fmt_int <- function(x) format(round(x), big.mark = ',', scientific = FALSE, trim = TRUE)
fmt_money <- function(x) sprintf('R$ %s', format(round(x, 2), big.mark = ',', nsmall = 2, scientific = FALSE, trim = TRUE))
fmt_pct <- function(x) sprintf('%.2f%%', 100 * x)
month_id <- function(d) as.integer(format(d, '%Y')) * 12L + as.integer(format(d, '%m'))
month_str <- function(d) format(d, '%Y-%m')
write_table <- function(data, filename) {
  utils::write.csv(data, file.path(out_dir, filename), row.names = FALSE, na = '')
}

# Either use local Kaggle files or download original CSVs from Olist's GitHub repository.
# Never synthesize missing orders, customers, or outcomes.
required_files <- c(
  'olist_orders_dataset.csv',
  'olist_customers_dataset.csv',
  'olist_order_items_dataset.csv',
  'olist_products_dataset.csv',
  'product_category_name_translation.csv'
)
source_url <- 'https://raw.githubusercontent.com/olist/work-at-olist-data/master/datasets/'
for (name in required_files) {
  destination <- file.path(raw_dir, name)
  if (file.exists(destination) && file.info(destination)$size > 100) next
  message('Downloading ', name)
  tmp <- paste0(destination, '.partial')
  try(unlink(tmp), silent = TRUE)
  ok <- tryCatch({
    utils::download.file(paste0(source_url, name), tmp, quiet = TRUE, mode = 'wb', method = 'libcurl')
    file.exists(tmp) && file.info(tmp)$size > 100L
  }, error = function(e) { message('Download error: ', conditionMessage(e)); FALSE },
     warning = function(w) { message('Download warning: ', conditionMessage(w)); FALSE })
  if (!ok) {
    unlink(tmp)
    stop('Missing ', name, '. Download the Olist dataset from Kaggle and put its CSV files in ', raw_dir,
         '. Dataset: https://www.kaggle.com/datasets/olistbr/brazilian-ecommerce', call. = FALSE)
  }
  if (!file.rename(tmp, destination)) stop('Cannot save ', destination, call. = FALSE)
}
read_raw <- function(name) {
  utils::read.csv(file.path(raw_dir, name), check.names = FALSE, na.strings = c('', 'NA'),
                  colClasses = 'character')
}
message('Reading real Olist CSVs...')
orders <- read_raw(required_files[1L])
customers <- read_raw(required_files[2L])
items <- read_raw(required_files[3L])
products <- read_raw(required_files[4L])
translations <- read_raw(required_files[5L])
raw_rows <- c(orders = nrow(orders), customers = nrow(customers), order_items = nrow(items),
              products = nrow(products), translations = nrow(translations))
required_columns <- list(
  orders = c('order_id', 'customer_id', 'order_status', 'order_purchase_timestamp'),
  customers = c('customer_id', 'customer_unique_id', 'customer_state'),
  items = c('order_id', 'order_item_id', 'product_id', 'price', 'freight_value'),
  products = c('product_id', 'product_category_name'),
  translations = c('product_category_name', 'product_category_name_english')
)
for (name in names(required_columns)) {
  missing <- setdiff(required_columns[[name]], names(get(name)))
  if (length(missing)) stop(name, ' CSV missing columns: ', paste(missing, collapse = ', '))
}
for (check in list(c('orders', 'order_id'), c('customers', 'customer_id'),
                   c('products', 'product_id'), c('translations', 'product_category_name'))) {
  x <- get(check[1])[[check[2]]]
  if (anyNA(x) || anyDuplicated(x)) stop('Duplicate or missing key: ', check[1], '.', check[2])
}
if (anyDuplicated(paste(items$order_id, items$order_item_id, sep = '|'))) {
  stop('Duplicate (order_id, order_item_id) item keys.')
}

# Business definitions:
# - Count product SALES using order_item.price (freight excluded).
# - Restrict sales / repeat / cohorts to delivered orders with at least one valid item.
# - Resolve repeat buyers through customer_unique_id; customer_id is per order.
orders$purchase_date <- as.Date(substr(orders$order_purchase_timestamp, 1L, 10L))
orders <- orders[orders$order_status == 'delivered' & !is.na(orders$purchase_date), , drop = FALSE]
orders$customer_unique_id <- customers$customer_unique_id[match(orders$customer_id, customers$customer_id)]
orders$customer_state <- customers$customer_state[match(orders$customer_id, customers$customer_id)]
orders <- orders[!is.na(orders$customer_unique_id) & nzchar(orders$customer_unique_id), , drop = FALSE]
items$price <- suppressWarnings(as.numeric(items$price))
items$freight_value <- suppressWarnings(as.numeric(items$freight_value))
item_count_before <- nrow(items)
items <- items[!is.na(items$price) & is.finite(items$price) & items$price >= 0 &
                 items$order_id %in% orders$order_id, , drop = FALSE]
if (!nrow(items)) stop('No valid delivered items found: check source data.', call. = FALSE)
items$product_category_name <- products$product_category_name[match(items$product_id, products$product_id)]
items$category <- translations$product_category_name_english[
  match(items$product_category_name, translations$product_category_name)]
missing_category <- is.na(items$category) | !nzchar(items$category)
items$category[missing_category] <- items$product_category_name[missing_category]
items$category[is.na(items$category) | !nzchar(items$category)] <- 'unknown'

# Aggregate BEFORE joining: item-level data and per-order summaries must not fan out.
order_amounts <- stats::aggregate(items$price, list(order_id = items$order_id), sum)
names(order_amounts)[2L] <- 'product_revenue_brl'
order_fact <- merge(orders[, c('order_id', 'customer_unique_id', 'customer_state', 'purchase_date')],
                    order_amounts, by = 'order_id', all = FALSE, sort = FALSE)
order_fact$order_month <- month_str(order_fact$purchase_date)
order_fact$is_weekend <- as.POSIXlt(order_fact$purchase_date)$wday %in% c(0L, 6L)
order_fact <- order_fact[order(order_fact$customer_unique_id, order_fact$purchase_date,
                               order_fact$order_id), , drop = FALSE]
order_fact$is_repeat_order <- duplicated(order_fact$customer_unique_id)
rownames(order_fact) <- NULL
items$customer_unique_id <- order_fact$customer_unique_id[match(items$order_id, order_fact$order_id)]
items$purchase_date <- order_fact$purchase_date[match(items$order_id, order_fact$order_id)]
items$order_month <- month_str(items$purchase_date)

# KPIs, category concentration, and nonparametric bootstrap confidence interval.
revenue <- sum(items$price)
num_orders <- nrow(order_fact)
aov <- mean(order_fact$product_revenue_brl)
num_customers <- length(unique(order_fact$customer_unique_id))
category <- stats::aggregate(items$price, list(category = items$category), sum)
names(category)[2L] <- 'revenue_brl'
category <- category[order(-category$revenue_brl, category$category), , drop = FALSE]
category$share_of_revenue <- category$revenue_brl / revenue
category$cumulative_share <- cumsum(category$share_of_revenue)
top5 <- head(category$category, 5L)
top5_share <- sum(head(category$revenue_brl, 5L)) / revenue

top_item <- items$price * (items$category %in% top5)
by_order_top <- stats::aggregate(top_item, list(order_id = items$order_id), sum)
names(by_order_top)[2L] <- 'top5_revenue'
order_fact$top5_revenue <- by_order_top$top5_revenue[
  match(order_fact$order_id, by_order_top$order_id)]
stopifnot(!anyNA(order_fact$top5_revenue),
          isTRUE(all.equal(sum(order_fact$product_revenue_brl), revenue)),
          isTRUE(all.equal(sum(order_fact$top5_revenue) / revenue, top5_share)))
set.seed(SEED)
boot_shares <- replicate(BOOTSTRAPS, {
  sampled <- sample.int(num_orders, num_orders, replace = TRUE)
  sum(order_fact$top5_revenue[sampled]) / sum(order_fact$product_revenue_brl[sampled])
})
share_ci <- as.numeric(stats::quantile(boot_shares, c(0.025, 0.975), names = FALSE))

# Repeat buyers and 90-day repeat purchasing, excluding customers with insufficient follow-up.
frequency <- table(order_fact$customer_unique_id)
repeat_customers <- sum(frequency >= 2L)
repeat_share <- repeat_customers / length(frequency)
repeat_ci <- stats::binom.test(repeat_customers, length(frequency), conf.level = CONFIDENCE)$conf.int
first_index <- !order_fact$is_repeat_order
first_orders <- order_fact[first_index, , drop = FALSE]
first_date_for_order <- first_orders$purchase_date[
  match(order_fact$customer_unique_id, first_orders$customer_unique_id)]
within90 <- order_fact$is_repeat_order &
  order_fact$purchase_date <= first_date_for_order + 90
repeat90_customers <- unique(order_fact$customer_unique_id[within90])
observation_end <- max(order_fact$purchase_date)
first_orders$eligible90 <- first_orders$purchase_date <= observation_end - 90
eligible90 <- first_orders$customer_unique_id[first_orders$eligible90]
repeat90 <- sum(eligible90 %in% repeat90_customers)
repeat90_n <- length(eligible90)
repeat90_rate <- repeat90 / repeat90_n
repeat90_ci <- stats::binom.test(repeat90, repeat90_n, conf.level = CONFIDENCE)$conf.int

customer_summary <- stats::aggregate(order_fact$product_revenue_brl,
                                     list(customer_unique_id = order_fact$customer_unique_id), sum)
names(customer_summary)[2L] <- 'lifetime_product_revenue_brl'
customer_summary$order_count <- as.integer(frequency[match(customer_summary$customer_unique_id,
                                                            names(frequency))])
customer_summary$first_purchase <- as.character(first_orders$purchase_date[
  match(customer_summary$customer_unique_id, first_orders$customer_unique_id)])
customer_summary$is_repeat_customer <- customer_summary$order_count > 1L

# Calendar-month cohorts. Month 0 is the acquisition month; all future months are
# repeat-purchase activity. NA means the month has not yet been observed, not zero.
first_orders$cohort_month <- month_str(first_orders$purchase_date)
cohort_sizes <- as.data.frame(table(first_orders$cohort_month), stringsAsFactors = FALSE)
names(cohort_sizes) <- c('cohort_month', 'cohort_customers')
cohort_sizes$cohort_customers <- as.integer(cohort_sizes$cohort_customers)
order_fact$cohort_month <- first_orders$cohort_month[
  match(order_fact$customer_unique_id, first_orders$customer_unique_id)]
cohort_year <- as.integer(substr(order_fact$cohort_month, 1, 4))
cohort_mon <- as.integer(substr(order_fact$cohort_month, 6, 7))
order_fact$cohort_age_months <- month_id(order_fact$purchase_date) - (12L * cohort_year + cohort_mon)
activity <- unique(order_fact[, c('cohort_month', 'customer_unique_id', 'cohort_age_months')])
activity <- activity[activity$cohort_age_months >= 0L &
                       activity$cohort_age_months <= COHORT_MAX_MONTHS, , drop = FALSE]
active_counts <- stats::aggregate(activity$customer_unique_id,
                                  list(cohort_month = activity$cohort_month,
                                       months_since_first_purchase = activity$cohort_age_months), length)
names(active_counts)[3L] <- 'active_customers'
cohorts <- expand.grid(cohort_month = cohort_sizes$cohort_month,
                       months_since_first_purchase = 0:COHORT_MAX_MONTHS,
                       stringsAsFactors = FALSE)
cohorts <- merge(cohorts, cohort_sizes, by = 'cohort_month', all.x = TRUE, sort = FALSE)
cohorts <- merge(cohorts, active_counts,
                 by = c('cohort_month', 'months_since_first_purchase'), all.x = TRUE, sort = FALSE)
last_month <- month_id(observation_end)
cohort_index <- 12L * as.integer(substr(cohorts$cohort_month, 1, 4)) +
                as.integer(substr(cohorts$cohort_month, 6, 7))
cohorts$observable <- cohort_index + cohorts$months_since_first_purchase <= last_month
cohorts$active_customers[cohorts$observable & is.na(cohorts$active_customers)] <- 0L
cohorts$retention_rate <- cohorts$active_customers / cohorts$cohort_customers
cohorts$active_customers[!cohorts$observable] <- NA_integer_
cohorts$retention_rate[!cohorts$observable] <- NA_real_
cohorts <- cohorts[order(cohorts$cohort_month, cohorts$months_since_first_purchase), , drop = FALSE]

monthly <- stats::aggregate(order_fact$product_revenue_brl,
                            list(month = order_fact$order_month), sum)
names(monthly)[2L] <- 'product_revenue_brl'
monthly_orders <- as.data.frame(table(order_fact$order_month), stringsAsFactors = FALSE)
names(monthly_orders) <- c('month', 'orders')
monthly <- merge(monthly, monthly_orders, by = 'month', all.x = TRUE, sort = TRUE)
monthly$average_order_value_brl <- monthly$product_revenue_brl / monthly$orders

# Welch two-sided t test, observational; does NOT establish a causal weekend effect.
weekend_orders <- order_fact$product_revenue_brl[order_fact$is_weekend]
weekday_orders <- order_fact$product_revenue_brl[!order_fact$is_weekend]
weekend_test <- stats::t.test(weekend_orders, weekday_orders, conf.level = CONFIDENCE)
weekend_difference <- mean(weekend_orders) - mean(weekday_orders)

# A/A sanity check only: randomized customer-level labels on historical observations.
# The source has NO treatment assignment or experimental conversion outcome.
set.seed(SEED)
unique_customers <- unique(order_fact$customer_unique_id)
arm <- sample(rep(c('A', 'B'), length.out = length(unique_customers)))
order_fact$aa_arm <- arm[match(order_fact$customer_unique_id, unique_customers)]
# Analyze the same unit used for randomization (one mean AOV per customer).
customer_aa <- stats::aggregate(order_fact$product_revenue_brl,
                                list(customer_unique_id = order_fact$customer_unique_id), mean)
names(customer_aa)[2L] <- 'mean_order_value_brl'
customer_aa$aa_arm <- arm[match(customer_aa$customer_unique_id, unique_customers)]
aa_A <- customer_aa$mean_order_value_brl[customer_aa$aa_arm == 'A']
aa_B <- customer_aa$mean_order_value_brl[customer_aa$aa_arm == 'B']
aa_test <- stats::t.test(aa_B, aa_A, conf.level = CONFIDENCE)
aa_difference <- mean(aa_B) - mean(aa_A)

# Prospective A/B design: power calculation, not observed treatment effect.
# Detect a 5% relative change in per-customer mean AOV at alpha=.05, power=.80.
planned_lift <- 0.05
ab_baseline <- mean(customer_aa$mean_order_value_brl)
ab_sd <- stats::sd(customer_aa$mean_order_value_brl)
ab_power <- stats::power.t.test(delta = ab_baseline * planned_lift,
                                sd = ab_sd,
                                sig.level = 0.05, power = 0.80, type = 'two.sample',
                                alternative = 'two.sided')
planned_n_per_arm <- ceiling(ab_power$n)

# CSVs are directly importable with Power BI Desktop: Home > Get Data > Text/CSV.
write_table(order_fact[, c('order_id', 'customer_unique_id', 'customer_state', 'purchase_date',
                           'order_month', 'product_revenue_brl', 'is_weekend',
                           'is_repeat_order', 'cohort_month', 'cohort_age_months', 'aa_arm')],
            'orders_powerbi.csv')
write_table(items[, c('order_id', 'product_id', 'category', 'price', 'freight_value',
                      'purchase_date', 'order_month')], 'order_items_powerbi.csv')
write_table(category, 'category_revenue.csv')
write_table(monthly, 'monthly_revenue.csv')
write_table(cohorts[, c('cohort_month', 'months_since_first_purchase', 'cohort_customers',
                         'active_customers', 'retention_rate', 'observable')],
            'cohort_retention.csv')
write_table(customer_summary, 'customer_summary.csv')
stat_tests <- data.frame(
  test = c('Weekend minus weekday AOV (observational Welch t-test)',
           'A/A arm B minus arm A customer mean AOV (Welch t-test)'),
  n_group_1 = c(length(weekend_orders), length(aa_B)),
  n_group_2 = c(length(weekday_orders), length(aa_A)),
  difference_brl = c(weekend_difference, aa_difference),
  ci95_low_brl = c(weekend_test$conf.int[1L], aa_test$conf.int[1L]),
  ci95_high_brl = c(weekend_test$conf.int[2L], aa_test$conf.int[2L]),
  p_value = c(weekend_test$p.value, aa_test$p.value)
)
write_table(stat_tests, 'statistical_tests.csv')
write_table(data.frame(alpha = .05, power = .80, assumed_relative_AOV_lift = planned_lift,
                       baseline_customer_mean_AOV_brl = ab_baseline, baseline_customer_SD_brl = ab_sd,
                       minimum_customers_per_arm = planned_n_per_arm), 'ab_test_design.csv')
write_table(data.frame(metric = c(names(raw_rows), 'delivered_items_used',
                                         'delivered_orders_with_items', 'unique_buyers',
                                         'categories', 'orders_removed_from_all_statuses',
                                         'items_excluded_by_delivery_or_invalid_price',
                                         'unmapped_product_categories'),
                       count = c(unname(raw_rows), nrow(items), num_orders, num_customers,
                                 nrow(category), raw_rows[['orders']] - num_orders,
                                 item_count_before - nrow(items), sum(missing_category))),
            'data_quality.csv')

# Static plots: no non-base dependencies, all can be regenerated in RStudio.
png(file.path(plot_dir, 'monthly_revenue.png'), width = 1300, height = 700, res = 125)
plot(seq_len(nrow(monthly)), monthly$product_revenue_brl / 1e6, type = 'b', pch = 19,
     xaxt = 'n', xlab = 'Order month', ylab = 'Product revenue (million BRL)',
     main = 'Olist: delivered product sales by month')
axis(1, at = seq_len(nrow(monthly)), labels = monthly$month, las = 2, cex.axis = .65)
grid()
dev.off()

png(file.path(plot_dir, 'top_categories.png'), width = 1200, height = 740, res = 120)
par(mar = c(5, 19, 4, 2))
barplot(rev(head(category$revenue_brl, 12L)) / 1e6,
        names.arg = rev(head(category$category, 12L)), horiz = TRUE, las = 1,
        cex.names = .72,
        main = 'Top product categories by sales', xlab = 'Product revenue (million BRL)')
dev.off()

png(file.path(plot_dir, 'cohort_retention.png'), width = 1280, height = 850, res = 120)
heat <- matrix(NA_real_, nrow = nrow(cohort_sizes), ncol = COHORT_MAX_MONTHS + 1L,
               dimnames = list(cohort_sizes$cohort_month, paste0('M', 0:COHORT_MAX_MONTHS)))
for (i in seq_len(nrow(cohorts))) {
  ix <- match(cohorts$cohort_month[i], rownames(heat))
  jx <- cohorts$months_since_first_purchase[i] + 1L
  heat[ix, jx] <- cohorts$retention_rate[i]
}
par(mar = c(5, 9, 4, 2))
# Render unavailable months as white. Month 0 (100%) is intentionally displayed.
heat_colors <- colorRampPalette(c('white', 'lightsteelblue', 'steelblue4'))(101L)
image(x = seq_len(ncol(heat)), y = seq_len(nrow(heat)),
      z = t(heat[nrow(heat):1L, , drop = FALSE]), col = heat_colors,
      zlim = c(0, 1), axes = FALSE, xlab = 'Months after acquisition', ylab = '',
      main = 'Customer cohort retention (NA = not observed)')
axis(1, at = seq_len(ncol(heat)), labels = colnames(heat))
axis(2, at = seq_len(nrow(heat)), labels = rev(rownames(heat)), las = 2, cex.axis = .75)
box()
dev.off()

# Build a readable results report and automatically insert it into README.md.
results <- c(
  sprintf('**Data window:** %s to %s (delivered orders with valid product prices).',
          min(order_fact$purchase_date), max(order_fact$purchase_date)),
  '',
  '| Metric | Calculated value |', '|---|---:|',
  sprintf('| Original order records (all statuses) | %s |', fmt_int(raw_rows[['orders']])),
  sprintf('| Original order-item records | %s |', fmt_int(raw_rows[['order_items']])),
  sprintf('| Eligible delivered orders | %s |', fmt_int(num_orders)),
  sprintf('| Eligible delivered item records | %s |', fmt_int(nrow(items))),
  sprintf('| Unique purchasing customers | %s |', fmt_int(num_customers)),
  sprintf('| Product revenue (excluding freight) | %s |', fmt_money(revenue)),
  sprintf('| Average order value | %s |', fmt_money(aov)),
  sprintf('| Top five category revenue share | %s (95%% bootstrap CI %s–%s) |',
          fmt_pct(top5_share), fmt_pct(share_ci[1]), fmt_pct(share_ci[2])),
  sprintf('| Repeat customer rate (2+ delivered orders) | %s (%s / %s; 95%% exact CI %s–%s) |',
          fmt_pct(repeat_share), fmt_int(repeat_customers), fmt_int(num_customers),
          fmt_pct(repeat_ci[1]), fmt_pct(repeat_ci[2])),
  sprintf('| 90-day repeat customer rate (eligible first-time buyers) | %s (%s / %s; 95%% exact CI %s–%s) |',
          fmt_pct(repeat90_rate), fmt_int(repeat90), fmt_int(repeat90_n),
          fmt_pct(repeat90_ci[1]), fmt_pct(repeat90_ci[2])),
  '',
  sprintf('**Top five categories:** %s.', paste(top5, collapse = ', ')),
  '',
  '### Statistical analysis',
  sprintf('- **Weekend vs weekday AOV:** %s difference (weekend minus weekday); Welch 95%% CI [%s, %s]; p = %.5g. This is observational, not causal.',
          fmt_money(weekend_difference), fmt_money(weekend_test$conf.int[1]),
          fmt_money(weekend_test$conf.int[2]), weekend_test$p.value),
  sprintf('- **Customer-randomized A/A check:** %s difference (B minus A); Welch 95%% CI [%s, %s]; p = %.5g. No treatment was applied.',
          fmt_money(aa_difference), fmt_money(aa_test$conf.int[1]),
          fmt_money(aa_test$conf.int[2]), aa_test$p.value),
  sprintf('- **Prospective A/B test design:** detect 5%% relative AOV lift at 80%% power and two-sided alpha = 0.05: approximately **%s customers per arm** under an independent-customer normal approximation. Not an observed treatment result.',
          fmt_int(planned_n_per_arm)),
  '',
  '### Interpretation',
  sprintf('- The largest five product categories represent %s of sales; this is a selected-category concentration estimate, not proof of an intervention.', fmt_pct(top5_share)),
  sprintf('- Only %s of purchasing customers place at least two delivered orders, making retention a natural business focus.', fmt_pct(repeat_share)),
  '- Cohort retention is exported by acquisition month; cells beyond the observation window are **NA**, not zero.',
  '- Category revenue includes product prices only; adding freight or using payment totals produces different amounts.',
  '',
  sprintf('*Generated by `growthlens.R` with random seed %s and %s order-level bootstrap resamples. These are calculated on your local Olist CSVs.*',
          SEED, BOOTSTRAPS)
)
writeLines(results, file.path(out_dir, 'RESULTS.md'), useBytes = TRUE)
readme_file <- file.path(script_dir, 'README.md')
if (file.exists(readme_file)) {
  lines <- readLines(readme_file, warn = FALSE, encoding = 'UTF-8')
  begin <- which(trimws(lines) == '<!-- RESULTS:START -->')
  end <- which(trimws(lines) == '<!-- RESULTS:END -->')
  if (length(begin) == 1L && length(end) == 1L && begin < end) {
    revised <- c(lines[seq_len(begin)], results, lines[end:length(lines)])
    writeLines(revised, readme_file, useBytes = TRUE)
  } else warning('README results markers missing; inspect outputs/RESULTS.md instead.')
}
cat(paste(results, collapse = '\n'), '\n')
message('\nFinished. Files: ', out_dir, '; README results refreshed.')
