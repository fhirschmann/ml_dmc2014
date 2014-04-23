library(lubridate)

holidays <- list(
        "Baden-Wuerttemberg"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-13')) # easter
          , c(as.Date('2012-05-29'), as.Date('2012-06-09')) # pentecost
          , c(as.Date('2012-07-26'), as.Date('2012-09-08')) # summer
          , c(as.Date('2012-10-29'), as.Date('2012-11-02')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-05')) # christmas
          , c(as.Date('2013-03-25'), as.Date('2013-04-05')) # easter
            ),
        "Bavaria"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-14')) # easter
          , c(as.Date('2012-05-29'), as.Date('2012-06-09')) # pentecost
          , c(as.Date('2012-08-01'), as.Date('2012-09-12')) # summer
          , c(as.Date('2012-10-29'), as.Date('2012-11-03')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-05')) # christmas
          , c(as.Date('2013-02-11'), as.Date('2013-02-15')) # winter
          , c(as.Date('2013-03-25'), as.Date('2013-04-06')) # easter
            ),
        "Berlin"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-14')) # easter
          , c(as.Date('2012-06-20'), as.Date('2012-08-03')) # summer
          , c(as.Date('2012-10-01'), as.Date('2012-10-13')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-04')) # christmas
          , c(as.Date('2013-02-04'), as.Date('2013-02-09')) # winter
          , c(as.Date('2013-03-25'), as.Date('2013-04-06')) # easter
            ),
        "Brandenburg"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-14')) # easter
          , c(as.Date('2012-06-21'), as.Date('2012-08-03')) # summer
          , c(as.Date('2012-10-01'), as.Date('2012-10-13')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-04')) # christmas
          , c(as.Date('2013-02-04'), as.Date('2013-02-09')) # winter
          , c(as.Date('2013-03-27'), as.Date('2013-04-06')) # easter
            ),
        "Bremen"=list(
            c(as.Date('2012-03-26'), as.Date('2012-04-11')) # easter
          , c(as.Date('2012-07-23'), as.Date('2012-08-31')) # summer
          , c(as.Date('2012-10-22'), as.Date('2012-11-03')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-05')) # christmas
          , c(as.Date('2013-03-16'), as.Date('2013-04-02')) # easter
            ),
        "Hesse"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-14')) # easter
          , c(as.Date('2012-07-02'), as.Date('2012-08-10')) # summer
          , c(as.Date('2012-10-15'), as.Date('2012-10-27')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-12')) # christmas
          , c(as.Date('2013-03-25'), as.Date('2013-04-06')) # easter
            ),
        "Hamburg"=list(
            c(as.Date('2012-03-05'), as.Date('2012-03-16')) # easter
          , c(as.Date('2012-04-30'), as.Date('2012-05-04')) # pentecost
          , c(as.Date('2012-06-21'), as.Date('2012-08-01')) # summer
          , c(as.Date('2012-10-01'), as.Date('2012-10-12')) # autumn
          , c(as.Date('2012-12-21'), as.Date('2013-01-04')) # christmas
          , c(as.Date('2013-03-04'), as.Date('2013-03-15')) # easter
            ),
        "Lower Saxony"=list(
            c(as.Date('2012-03-26'), as.Date('2012-04-11')) # easter
          , c(as.Date('2012-07-23'), as.Date('2012-08-31')) # summer
          , c(as.Date('2012-10-22'), as.Date('2012-11-03')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-05')) # christmas
          , c(as.Date('2013-03-16'), as.Date('2013-04-02')) # easter
            ),
        "Mecklenburg-Western Pomerania"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-11')) # easter
          , c(as.Date('2012-05-25'), as.Date('2012-05-29')) # pentecost
          , c(as.Date('2012-06-23'), as.Date('2012-08-04')) # summer
          , c(as.Date('2012-10-01'), as.Date('2012-10-05')) # autumn
          , c(as.Date('2012-12-21'), as.Date('2013-01-04')) # christmas
          , c(as.Date('2013-02-04'), as.Date('2013-02-15')) # winter
          , c(as.Date('2013-03-25'), as.Date('2013-04-03')) # easter
            ),
        "North Rhine-Westphalia"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-14')) # easter
          , c(as.Date('2012-07-09'), as.Date('2012-08-21')) # summer
          , c(as.Date('2012-10-08'), as.Date('2012-10-20')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-04')) # christmas
          , c(as.Date('2013-03-25'), as.Date('2013-04-06')) # easter
            ),
        "Rhineland-Palatinate"=list(
            c(as.Date('2012-03-29'), as.Date('2012-04-13')) # easter
          , c(as.Date('2012-07-02'), as.Date('2012-08-10')) # summer
          , c(as.Date('2012-10-01'), as.Date('2012-10-12')) # autumn
          , c(as.Date('2012-12-20'), as.Date('2013-01-04')) # christmas
          , c(as.Date('2013-03-20'), as.Date('2013-04-05')) # easter
            ),
        "Schleswig-Holstein"=list(
            c(as.Date('2012-03-30'), as.Date('2012-04-13')) # easter
          , c(as.Date('2012-06-25'), as.Date('2012-08-04')) # summer
          , c(as.Date('2012-10-04'), as.Date('2012-10-19')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-05')) # christmas
          , c(as.Date('2013-03-25'), as.Date('2013-04-09')) # easter
            ),
        "Saarland"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-14')) # easter
          , c(as.Date('2012-07-02'), as.Date('2012-08-14')) # summer
          , c(as.Date('2012-10-22'), as.Date('2012-11-03')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-05')) # christmas
          , c(as.Date('2013-02-11'), as.Date('2013-02-16')) # winter
          , c(as.Date('2013-03-25'), as.Date('2013-04-06')) # easter
            ),
        "Saxony"=list(
            c(as.Date('2012-04-06'), as.Date('2012-04-14')) # easter
          , c(as.Date('2012-07-23'), as.Date('2012-08-31')) # summer
          , c(as.Date('2012-10-22'), as.Date('2012-11-02')) # autumn
          , c(as.Date('2012-12-22'), as.Date('2013-01-02')) # christmas
          , c(as.Date('2013-02-04'), as.Date('2013-02-15')) # winter
          , c(as.Date('2013-03-29'), as.Date('2013-04-06')) # easter
            ),
        "Saxony-Anhalt"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-07')) # easter
          , c(as.Date('2012-05-18'), as.Date('2012-05-25')) # pentecost
          , c(as.Date('2012-07-23'), as.Date('2012-09-05')) # summer
          , c(as.Date('2012-10-29'), as.Date('2012-11-02')) # autumn
          , c(as.Date('2012-12-19'), as.Date('2013-01-04')) # christmas
          , c(as.Date('2013-02-01'), as.Date('2013-02-08')) # winter
          , c(as.Date('2013-03-25'), as.Date('2013-03-30')) # easter
            ),
        "Thuringia"=list(
            c(as.Date('2012-04-02'), as.Date('2012-04-13')) # easter
          , c(as.Date('2012-05-25'), as.Date('2012-05-29')) # pentecost
          , c(as.Date('2012-07-23'), as.Date('2012-08-31')) # summer
          , c(as.Date('2012-10-22'), as.Date('2012-11-03')) # autumn
          , c(as.Date('2012-12-24'), as.Date('2013-01-05')) # christmas
          , c(as.Date('2013-02-18'), as.Date('2013-02-23')) # winter
          , c(as.Date('2013-03-25'), as.Date('2013-04-06')) # easter
            )
    )

date.within <- function(date, interval) {
    date >= interval[1] && date <= interval[2]
}

judge.holidays <- function(date, state) {
    local.holidays <- holidays[state][[1]]
    judgements <- lapply(local.holidays, judge.holiday, date=date)
    if ("during" %in% judgements) {
        "during"
    } else if ("before" %in% judgements) {
        "before"
    } else if ("after" %in% judgements) {
        "after"
    } else {
        "other"
    }
}

judge.holiday <- function(date, interval) {
    start <- interval[1]
    end <- interval[2]
    before.int <- c(start - days(7), start)
    after.int <- c(end, end + days(7))
    if (date.within(date, interval)) {
        "during"
    } else if (date.within(date, before.int)) {
        "before"
    } else if (date.within(date, after.int)) {
        "after"
    } else "other"
}
