# horno.r
Identification of heat waves based on the methodology proposed by [Russo et al
2015](https://iopscience.iop.org/article/10.1088/1748-9326/10/12/124003/meta).
In summary, each calendar day is compared to the 90th percentile of a 31-day
window center on each day. A heat wave occurs when the temperature surpasses
this threshold for a minimum of 3 consecutive days. The magnitude on the other
hand, is calculated as the difference between the daily temperature and the
25th percentile of the annual maximum temperature of the reference period.

## Install

```
library(devtools)
install_github("profesorpaiche/horno.r")
```

## How to use?

The simplest way to identify heatwaves is having a data frame with daily
temperature and call the `findWaves` function.

```
data(t2m_hamburg)
heatwaves <- findWaves(t2m_hamburg$date, t2m_hamburg$temperature)
```

This function will generate the following variables:

- `wave_day`: boolean value if the day is part of heat wave
- `wave_event`: integer with the ID of different heat waves
- `magnitude_day`: double with the daily magnitudes
- `magnitude_event`: double with the accumulated magnitude for each heat wave

By default `findWaves` will use the period between 1981 to 2010 as the
reference for the thresholds. However, you can pass any reference period. In
the same sense, you can change the threshold used to identify the heat wave
days (0.9 by default), or even use the function to find "cold waves" using the
same methodology.

```
# Using the reference period of 1971-2000 and a quantile threshold of 0.95 (more strict)
heatwaves <- findWaves(
    t2m_hamburg$date,
    t2m_hamburg$temperature,
    reference = c(1971, 2000),
    qthresh = 0.95
)
```

```
data(t2m_hamburg)
coldwaves <- findWaves(t2m_hamburg$date, t2m_hamburg$temperature, type = "cold wave")
```

