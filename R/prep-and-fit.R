library(tidyverse)
library(readxl)
library(tidycensus)
library(broom)

states <- data_frame(
  state = state.name,
  state_abbr = state.abb
)

states <- add_row(states, state = "District of Columbia", state_abbr = "DC")

# import & prep -----------------------------------------------------------

### dependent variable

# https://cwoutcomes.acf.hhs.gov/cwodatasite/childrenReports/index
rts <- dir("C:/Users/andwilmo/Downloads/", pattern = "Child Welfare Outcomes Report", full.names = TRUE) %>%
  map_df(~read_csv(., col_types = "cccc")) %>%
  set_names(c("state", "year", "cps_n", "cps_rate")) %>%
  arrange(year, state) %>%
  mutate_at(vars(-state), parse_number)

### independent variable

# https://www.kff.org/health-reform/state-indicator/state-decisions-for-creating-health-insurance-exchanges-and-expanding-medicaid/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
aca <- read_csv(
  "C:/Users/andwilmo/Downloads/raw_data.csv",
  skip = 3,
  col_names = c("state", "marketplace", "status", "footnotes"),
  col_types = "cccc"
) %>% slice(1:52)

### controls: welfare characteristics

# http://www.ukcpr.org/data
ukcpr <- read_excel("C:/Users/andwilmo/Downloads/UKCPR_National_Welfare_Data_Final_Update_20180116_0.xlsx", sheet = "Data")

welfr <- ukcpr %>%
  filter(year %in% 2012:2016) %>%
  select(
    state_abbr = state_name,
    year,
    unemp   = `Unemployment rate`,
    eitcref = `Refundable State EITC (1=Yes)`,
    eitcrt  = `State EITC Rate`,
    minwage = `State Minimum Wage`,
    demgov  = `Governor is Democrat (1=Yes)`,
    tanf3p  = `AFDC/TANF Benefit for 3-person family`,
    snap3p  = `FS/SNAP Benefit for 3-person family`
  ) %>%
  left_join(states)

### controls: state demographics

racevars <- c(
  "total" = "B02001_001",
  "white" = "B02001_002",
  "black" = "B02001_003",
  "hisp"  = "B03001_003"
)

raceq <- map(
  2012:2016,
  ~get_acs(geography = "state", variables = racevars, survey = "acs1", year = ., output = "wide")
)

ageq <- map(
  2012:2016,
  ~get_acs(geography = "state", variables = "B01002_001", survey = "acs1", year = ., output = "wide")
)

race <- raceq %>%
  set_names(2012:2016) %>%
  bind_rows(.id = "year") %>%
  select(state = NAME, year, ends_with("E")) %>%
  set_names(c("state", "year", names(racevars))) %>%
  gather(race, val, white:hisp) %>%
  mutate(prop = val / total, year = as.numeric(year))

raceprop <- race %>%
  select(state, year, race, prop) %>%
  spread(race, prop)

age <- ageq %>%
  set_names(2012:2016) %>%
  bind_rows(.id = "year") %>%
  select(state = NAME, year, ends_with("E")) %>%
  set_names(c("state", "year", "median_age")) %>%
  mutate(year = as.numeric(year))

### controls: child population size

# https://datacenter.kidscount.org/data/tables/99-total-population-by-child-and-adult#detailed/2/2-53/false/870,573,869,36,868/39,40,41/416,417
chpop <- read_excel("C:/Users/andwilmo/Downloads/Total population by child and adult populations.xlsx")

chpop <- chpop %>%
  filter(DataFormat == "Percent", TimeFrame %in% 2012:2016) %>%
  select(state = Location, age = `Child and Adult Populations`, year = TimeFrame, val = Data) %>%
  mutate(year = as.numeric(year), val = as.numeric(val))

u18 <- filter(chpop, age == "Less than age 18") %>% rename(prop_u18 = val)

# modeling frame ----------------------------------------------------------

df <- rts %>%
  left_join(aca) %>%
  left_join(welfr) %>%
  left_join(u18) %>%
  left_join(raceprop) %>%
  left_join(age) %>%
  mutate(
    demgov = case_when(
      is.na(demgov) & state == "District of Columbia" ~ 1,
      TRUE ~ demgov
    ),
    mce = 0,
    mce = ifelse(status == "Adopted" & year >= 2014, 1, 0)
  ) %>%
  select(-cps_n, -marketplace, -footnotes, -state_abbr, -status)

df$has_na <- apply(df, 1, anyNA)

# build & inspect model ---------------------------------------------------

fit <- lm(
  cps_rate ~
    mce +
    minwage +
    unemp +
    eitcref +
    eitcrt +
    demgov +
    tanf3p +
    snap3p +
    white +
    black +
    # hisp +     ## most states missing?
    median_age +
    state * year,
  data = df,
  weights = prop_u18
)

summary(fit)

fit_coef <- tidy(fit) %>%
  mutate_at(vars(-term), funs(round(., 4))) %>%
  filter(!str_detect(term, "state"))

fit_coef

fit_diag <- glance(fit)

qqnorm(fit$residuals)
qqline(fit$residuals)

shapiro.test(fit$residuals)
