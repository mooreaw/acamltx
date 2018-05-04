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
    fedminw = `Federal Minimum Wage`,
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

eduvars <- c(
  "mtotal" = "B15002_002",
  "mhsd"   = "B15002_011",
  "msclt1" = "B15002_012",
  "mscnod" = "B15002_013",
  "massoc" = "B15002_014",
  "mbachd" = "B15002_015",
  "mmastr" = "B15002_016",
  "mprofd" = "B15002_017",
  "mdoct"  = "B15002_018",
  "ftotal" = "B15002_019",
  "fhsd"   = "B15002_028",
  "fsclt1" = "B15002_029",
  "fscnod" = "B15002_030",
  "fassoc" = "B15002_031",
  "fbachd" = "B15002_032",
  "fmastr" = "B15002_033",
  "fprofd" = "B15002_034",
  "fdoct"  = "B15002_035"
)

marvars <- c(
  "total"    = "B12001_001",
  "mmarried" = "B12001_004",
  "mdivorce" = "B12001_010",
  "fmarried" = "B12001_013",
  "fdivorce" = "B12001_019"
)

raceq <- map(
  2012:2016,
  ~get_acs(geography = "state", variables = racevars, survey = "acs1", year = ., output = "wide")
)

ageq <- map(
  2012:2016,
  ~get_acs(geography = "state", variables = "B01002_001", survey = "acs1", year = ., output = "wide")
)

eduq <- map(
  2012:2016,
  ~get_acs(geography = "state", variables = eduvars, survey = "acs1", year = ., output = "wide")
)

marq <- map(
  2012:2016,
  ~get_acs(geography = "state", variables = marvars, survey = "acs1", year = ., output = "wide")
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

edu <- eduq %>%
  set_names(2012:2016) %>%
  bind_rows(.id = "year") %>%
  select(state = NAME, year, ends_with("E")) %>%
  set_names(c("state", "year", names(eduvars))) %>%
  gather(edu, val, -mtotal, -ftotal, -state, -year) %>%
  mutate(
    year = as.numeric(year),
    prop = ifelse(str_sub(edu, 1, 1) == "m", val / mtotal, val / ftotal) 
  )

eduagg <- edu %>%
  mutate(total = mtotal + ftotal) %>%
  group_by(
    state,
    year,
    hsd  = str_detect(edu, "hsd"),
    sca  = str_detect(edu, "sc"),
    degu = str_detect(edu, "assoc|bachd|doct|mastr|profd") 
  ) %>%
  summarise(total = first(total), val = sum(val), prop = val / total) %>%
  ungroup()

eduagg <- eduagg %>%
  select(state, year, hsd:degu, prop) %>%
  gather(edu, log, hsd:degu) %>%
  filter(log) %>%
  arrange(state, year, edu) %>%
  select(-log) %>%
  spread(edu, prop) %>%
  mutate(hsl = 1 - (degu + hsd + sca))

eduprop <- edu %>%
  select(state, year, edu, prop) %>%
  spread(edu, prop)

mar <- marq %>%
  set_names(2012:2016) %>%
  bind_rows(.id = "year") %>%
  select(state = NAME, year, ends_with("E")) %>%
  set_names(c("state", "year", names(marvars))) %>%
  transmute(
    state = state,
    year  = as.numeric(year),
    pmar  = (mmarried + fmarried) / total,
    pdiv  = (mdivorce + fdivorce) / total
  )

### controls: child population size

# https://datacenter.kidscount.org/data/tables/99-total-population-by-child-and-adult#detailed/2/2-53/false/870,573,869,36,868/39,40,41/416,417
chpop <- read_excel("C:/Users/andwilmo/Downloads/Total population by child and adult populations.xlsx")

chpop <- chpop %>%
  filter(DataFormat == "Percent", TimeFrame %in% 2012:2016) %>%
  select(state = Location, age = `Child and Adult Populations`, year = TimeFrame, val = Data) %>%
  mutate(year = as.numeric(year), val = as.numeric(val))

u18 <- filter(chpop, age == "Less than age 18") %>% rename(prop_u18 = val)

# urbanity (2010, via wikipedia)
urb <- data_frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", 
            "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", 
            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
            "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
            "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
            "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
            "Oklahoma", "Oregon", "Pennsylvania", "Rhode", "South Carolina", 
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
            "Washington", "West", "Wisconsin", "Wyoming"),
  urban = c(0.59, 0.66, 0.898, 0.562, 0.952, 0.862, 0.88, 0.833, 1, 0.912, 0.751, 
            0.919, 0.706, 0.885, 0.724, 0.64, 0.742, 0.584, 0.732, 0.387, 
            0.872, 0.92, 0.746, 0.733, 0.493, 0.704, 0.559, 0.731, 0.942, 
            0.603, 0.947, 0.774, 0.879, 0.661, 0.599, 0.779, 0.662, 0.81, 
            0.787, 0.907, 0.663, 0.567, 0.664, 0.847, 0.906, 0.389, 0.755, 
            0.84, 0.487, 0.702, 0.648)
)

# modeling frame ----------------------------------------------------------

df <- rts %>%
  left_join(aca) %>%
  left_join(welfr) %>%
  left_join(u18) %>%
  left_join(raceprop) %>%
  left_join(age) %>%
  left_join(eduagg) %>%
  left_join(mar) %>%
  left_join(urb) %>%
  mutate(
    yrcat  = factor(year),
    demgov = case_when(
      is.na(demgov) & state == "District of Columbia" ~ 1,
      TRUE ~ demgov
    ),
    minwage = ifelse(fedminw > minwage, fedminw, minwage),
    mce = 0,
    mce = ifelse(status == "Adopted" & year >= 2014, 1, 0),
    mce = case_when(
      state == "Montana" & year < 2016 ~ 0,
      state == "Pennslyvania" & year < 2015 ~ 0,
      state == "Alaska" & year < 2015 ~ 0,
      state == "Louisiana" & year < 2016 ~ 0,
      TRUE ~ mce
    )
  ) %>%
  select(-cps_n, -marketplace, -footnotes, -state_abbr, -status)

df$has_na <- apply(df, 1, anyNA)

# build & inspect model ---------------------------------------------------

## missing: % hispanic, % children living in urban area

lm_eq <- 
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
  urban +
  # hisp +     ## most states missing?
  median_age +
  hsl +
  sca +
  degu +
  pmar +
  pdiv +
  yrcat +      ## year, as a categorical value
  state +
  state:year   ## state specific linear trend

fit <- lm(formula = lm_eq, data = df, weights = prop_u18)

fit_coef <- tidy(fit) %>%
  mutate_at(vars(-term), funs(round(., 4))) %>%
  filter(!str_detect(term, "state"))

fit_diag <- glance(fit)

fit_coef
fit_diag

plot(fit)

shapiro.test(fit$residuals)

set.seed(1)

data_frame(res = fit$residuals, sim = rnorm(length(fit$residuals), 0, 2.5)) %>%
  gather(var, val) %>%
  ggplot(aes(x = val, fill = var)) +
  geom_density(alpha = .8)

