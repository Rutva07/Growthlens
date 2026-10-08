# GrowthLens: E-Commerce Revenue & Customer Analytics

**Stack:** R (base R and the `stats` package), RStudio, Power BI-compatible CSV exports  
**Focus:** revenue composition, customer repeat purchasing, cohort retention, confidence intervals, hypothesis testing, and experiment design.

GrowthLens is a single-script analytics project based on the real **Olist Brazilian E-Commerce Public Dataset**. It downloads the original data (or reads local copies), validates join keys, constructs reliable order- and customer-level datasets, calculates revenue KPIs, runs statistical analyses, creates charts, and exports tables that can be imported directly into Power BI.

## Dataset

- **Original dataset:** [Brazilian E-Commerce Public Dataset by Olist](https://www.kaggle.com/datasets/olistbr/brazilian-ecommerce), anonymized Brazilian marketplace transactions from 2016–2018.
- **Original publisher's CSV mirror:** [olist/work-at-olist-data](https://github.com/olist/work-at-olist-data/tree/master/datasets).
- **Actual published size:** **99,441 orders**, **112,650 order-item records**, and **99,441 customer/order IDs** in the respective core files. 
- **Data tables loaded:** orders, order items, customers, products, product-category English translation.
- **Source license:** [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) according to the Kaggle dataset page. Follow its attribution, noncommercial, and share-alike requirements.
- **Currency:** Brazilian reais (BRL). Category sales and order value are **sum of item prices, excluding freight**; they are not payment collections, refunds, margin, or profit.

The script downloads the five CSV files if they are not present in `data/`. No Kaggle API token, SQL database, fabricated data, or non-base R package is required. If GitHub access is blocked, download the Kaggle archive and put the five CSVs under `data/` manually.

## Reproduce in RStudio

1. Install **R 4.1+** and open `growthlens.R` in **RStudio**.
2. Click **Source**, or from a terminal run:

   ```bash
   Rscript growthlens.R
   ```

3. The script writes the full numerical report into **this README**, saves `outputs/RESULTS.md`, and creates Power BI-ready files in `outputs/`.

The first run needs internet access to download the published Olist CSVs. Subsequent runs use the cached files. A normal RStudio installation supports the script without `install.packages()`.

## Results

<!-- RESULTS:START -->
**Execution status:** The public dataset and its schema were verified, but the main R analysis has **not been executed in the delivery environment** (R is unavailable there and downloads to that environment are blocked). Consequently, no statistically computed output from this script is represented as an observed result. When `growthlens.R` runs on the authentic CSVs, it **automatically replaces this entire results section** with measured metrics and statistical tests, and separately writes `outputs/RESULTS.md`.

Published **comparison benchmarks** from other analyses of the Olist dataset (not measurements claimed for GrowthLens):

| Key | Value |
|---|---:|
| Source data size | 99,441 orders; 112,650 order items | 
| Top-five category share of product sales | ~41.58% 
| Customers with repeat orders | ~4.12% |

<!-- RESULTS:END -->

## What the R script computes

| Analysis | Method | Meaning |
|---|---|---|
| Revenue & product categories | Joined product/category records, item-price aggregations | Category rank, share of total revenue, cumulative share; total delivered sales and AOV |
| Category share uncertainty | 300 order-level bootstrap resamples, seed `2026` | 95% percentile interval for top-five share (top-five identities fixed from observed data) |
| Customer retention | `customer_unique_id`, distinct orders, exact binomial CI | Share of customers with two or more delivered purchases |
| 90-day retention | Eligibility-aware first-purchase cohort, exact binomial CI | Share repurchasing within 90 days, dropping right-censored first purchases |
| Monthly cohorts | Cohort month × purchase month | Number and rate of active buyers at months 0–12; future/unobservable cells are missing, not zero |
| Weekday vs weekend | Two-sided Welch t-test, 95% CI | Observational mean AOV difference; no causal claim |
| A/A randomization check | Random **customer-level** A/B labels and customer-level Welch t-test | Sanity check on historical order values; **no treatment administered** |
| Prospective A/B sizing | `power.t.test`, 5% hypothetical AOV lift, 80% power, alpha 0.05 | Approximate independent-customer sample size per arm; **not observed improvement** |
| Data quality | Key uniqueness, valid prices, join cardinality, exclusion counts | Confirms the aggregation avoids multiplying revenue in one-to-many joins |

**Interpretation of cohort retention:** month 0 is 100% by construction (customers' acquisition month). Month 1 counts customers who ordered in the following calendar month; it is not the 30-day retention rate. The separately computed 90-day repeat rate uses 90 elapsed days.

**Statistical caveats:** Olist is an observational, historical marketplace sample. Its CI formulas rely on sampling assumptions; customer purchase events are not randomized. The A/A check and A/B power design demonstrate experimentation technique but **the dataset has no authentic randomized experiment**. The prospective test's independent-order assumption is an approximation; a production experiment should randomize by customer, use one customer-level outcome per customer, and plan for missing outcomes.

## Outputs and Power BI

Only `growthlens.R` and `README.md` are shipped. Running the R file creates the local `data/` and `outputs/` directories:

```text
GrowthLens/
├── growthlens.R                 # Everything: data load, cleaning, stats, exports, charts, reporting
├── README.md                    # Automatically refreshed results after running
├── data/                        # Auto-downloaded original Olist CSVs (after first run)
└── outputs/                     # Generated by R after a successful run
    ├── RESULTS.md
    ├── orders_powerbi.csv       # Order-grain reporting dataset
    ├── order_items_powerbi.csv  # Item-grain products and revenue
    ├── category_revenue.csv
    ├── monthly_revenue.csv
    ├── cohort_retention.csv
    ├── customer_summary.csv
    ├── statistical_tests.csv
    ├── ab_test_design.csv
    ├── data_quality.csv
    └── plots/
        ├── monthly_revenue.png
        ├── top_categories.png
        └── cohort_retention.png
```

**Power BI:** Open Power BI Desktop → **Get Data → Text/CSV**, import `orders_powerbi.csv`, `category_revenue.csv`, `monthly_revenue.csv`, and `cohort_retention.csv`. Set dates and currencies using their exported types; category revenues and order revenues come from the same base and should not be added together. Recommended visuals: monthly revenue line, top categories bar, customer repeat rate card, and cohort-retention matrix. These are **ready-to-import data exports and charts**, not a prebuilt `.pbix` dashboard.
