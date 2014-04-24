#!/usr/bin/env python
# -*- coding: utf-8 -*-

import csv
import datetime
import os

HOLIDAYS = {
        "Baden-Wuerttemberg": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 13)) # easter
          , (datetime.date(2012, 5, 29), datetime.date(2012, 6, 9)) # pentecost
          , (datetime.date(2012, 7, 26), datetime.date(2012, 9, 8)) # summer
          , (datetime.date(2012, 10, 29), datetime.date(2012, 11, 2)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 5)) # christmas
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 5))  # easter
            ],
        "Bavaria": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 14)) # easter
          , (datetime.date(2012, 5, 29), datetime.date(2012, 6, 9)) # pentecost
          , (datetime.date(2012, 8, 1), datetime.date(2012, 9, 12)) # summer
          , (datetime.date(2012, 10, 29), datetime.date(2012, 11, 3)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 5)) # christmas
          , (datetime.date(2013, 2, 11), datetime.date(2013, 2, 15)) # winter
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 6)) # easter
            ],
        "Berlin": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 14)) # easter
          , (datetime.date(2012, 6, 20), datetime.date(2012, 8, 3)) # summer
          , (datetime.date(2012, 10, 1), datetime.date(2012, 10, 13)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 4)) # christmas
          , (datetime.date(2013, 2, 4), datetime.date(2013, 2, 9)) # winter
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 6)) # easter
            ],
        "Brandenburg": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 14)) # easter
          , (datetime.date(2012, 6, 21), datetime.date(2012, 8, 3)) # summer
          , (datetime.date(2012, 10, 1), datetime.date(2012, 10, 13)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 4)) # christmas
          , (datetime.date(2013, 2, 4), datetime.date(2013, 2, 9)) # winter
          , (datetime.date(2013, 3, 27), datetime.date(2013, 4, 6)) # easter
            ],
        "Bremen": [
            (datetime.date(2012, 3, 26), datetime.date(2012, 4, 11)) # easter
          , (datetime.date(2012, 7, 23), datetime.date(2012, 8, 31)) # summer
          , (datetime.date(2012, 10, 22), datetime.date(2012, 11, 3)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 5)) # christmas
          , (datetime.date(2013, 3, 16), datetime.date(2013, 4, 2)) # easter
            ],
        "Hesse": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 14)) # easter
          , (datetime.date(2012, 7, 2), datetime.date(2012, 8, 10)) # summer
          , (datetime.date(2012, 10, 15), datetime.date(2012, 10, 27)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 12)) # christmas
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 6)) # easter
            ],
        "Hamburg": [
            (datetime.date(2012, 3, 5), datetime.date(2012, 3, 16)) # easter
          , (datetime.date(2012, 4, 30), datetime.date(2012, 5, 4)) # pentecost
          , (datetime.date(2012, 6, 21), datetime.date(2012, 8, 1)) # summer
          , (datetime.date(2012, 10, 1), datetime.date(2012, 10, 12)) # autumn
          , (datetime.date(2012, 12, 21), datetime.date(2013, 1, 4)) # christmas
          , (datetime.date(2013, 3, 4), datetime.date(2013, 3, 15)) # easter
            ],
        "Lower Saxony": [
            (datetime.date(2012, 3, 26), datetime.date(2012, 4, 11)) # easter
          , (datetime.date(2012, 7, 23), datetime.date(2012, 8, 31)) # summer
          , (datetime.date(2012, 10, 22), datetime.date(2012, 11, 3)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 5)) # christmas
          , (datetime.date(2013, 3, 16), datetime.date(2013, 4, 2)) # easter
            ],
        "Mecklenburg-Western Pomerania": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 11)) # easter
          , (datetime.date(2012, 5, 25), datetime.date(2012, 5, 29)) # pentecost
          , (datetime.date(2012, 6, 23), datetime.date(2012, 8, 4)) # summer
          , (datetime.date(2012, 10, 1), datetime.date(2012, 10, 5)) # autumn
          , (datetime.date(2012, 12, 21), datetime.date(2013, 1, 4)) # christmas
          , (datetime.date(2013, 2, 4), datetime.date(2013, 2, 15)) # winter
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 3)) # easter
            ],
        "North Rhine-Westphalia": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 14)) # easter
          , (datetime.date(2012, 7, 9), datetime.date(2012, 8, 21)) # summer
          , (datetime.date(2012, 10, 8), datetime.date(2012, 10, 20)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 4)) # christmas
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 6)) # easter
            ],
        "Rhineland-Palatinate": [
            (datetime.date(2012, 3, 29), datetime.date(2012, 4, 13)) # easter
          , (datetime.date(2012, 7, 2), datetime.date(2012, 8, 10)) # summer
          , (datetime.date(2012, 10, 1), datetime.date(2012, 10, 12)) # autumn
          , (datetime.date(2012, 12, 20), datetime.date(2013, 1, 4)) # christmas
          , (datetime.date(2013, 3, 20), datetime.date(2013, 4, 5)) # easter
            ],
        "Schleswig-Holstein": [
            (datetime.date(2012, 3, 30), datetime.date(2012, 4, 13)) # easter
          , (datetime.date(2012, 6, 25), datetime.date(2012, 8, 4)) # summer
          , (datetime.date(2012, 10, 4), datetime.date(2012, 10, 19)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 5)) # christmas
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 9)) # easter
            ],
        "Saarland": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 14)) # easter
          , (datetime.date(2012, 7, 2), datetime.date(2012, 8, 14)) # summer
          , (datetime.date(2012, 10, 22), datetime.date(2012, 11, 3)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 5)) # christmas
          , (datetime.date(2013, 2, 11), datetime.date(2013, 2, 16)) # winter
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 6)) # easter
            ],
        "Saxony": [
            (datetime.date(2012, 4, 6), datetime.date(2012, 4, 14)) # easter
          , (datetime.date(2012, 7, 23), datetime.date(2012, 8, 31)) # summer
          , (datetime.date(2012, 10, 22), datetime.date(2012, 11, 2)) # autumn
          , (datetime.date(2012, 12, 22), datetime.date(2013, 1, 2)) # christmas
          , (datetime.date(2013, 2, 4), datetime.date(2013, 2, 15)) # winter
          , (datetime.date(2013, 3, 29), datetime.date(2013, 4, 6)) # easter
            ],
        "Saxony-Anhalt": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 7)) # easter
          , (datetime.date(2012, 5, 18), datetime.date(2012, 5, 25)) # pentecost
          , (datetime.date(2012, 7, 23), datetime.date(2012, 9, 5)) # summer
          , (datetime.date(2012, 10, 29), datetime.date(2012, 11, 2)) # autumn
          , (datetime.date(2012, 12, 19), datetime.date(2013, 1, 4)) # christmas
          , (datetime.date(2013, 2, 1), datetime.date(2013, 2, 8)) # winter
          , (datetime.date(2013, 3, 25), datetime.date(2013, 3, 30)) # easter
            ],
        "Thuringia": [
            (datetime.date(2012, 4, 2), datetime.date(2012, 4, 13)) # easter
          , (datetime.date(2012, 5, 25), datetime.date(2012, 5, 29)) # pentecost
          , (datetime.date(2012, 7, 23), datetime.date(2012, 8, 31)) # summer
          , (datetime.date(2012, 10, 22), datetime.date(2012, 11, 3)) # autumn
          , (datetime.date(2012, 12, 24), datetime.date(2013, 1, 5)) # christmas
          , (datetime.date(2013, 2, 18), datetime.date(2013, 2, 23)) # winter
          , (datetime.date(2013, 3, 25), datetime.date(2013, 4, 6)) # easter
            ]
}


def judge_holidays(date, state):
    priorities = ['during', 'before', 'after', 'other']
    judgements = sorted([judge_holiday(date, interval) for interval in HOLIDAYS[state]], key=priorities.index)
    return judgements[0]


def judge_holiday(date, interval):
    start, end = interval
    before = start - datetime.timedelta(days=7)
    after = end + datetime.timedelta(days=7)
    if start <= date <= end:
        return 'during'
    elif before <= date <= start:
        return 'before'
    elif end <= date <= after:
        return 'after'
    else:
        return 'other'

def main(path, to):
    with open(path) as f, open(to, 'w') as fout:
        reader = csv.DictReader(f, delimiter=';')
        writer = csv.DictWriter(fout, reader.fieldnames + ['holiday'], delimiter=';')
        writer.writeheader()
        for entry in reader:
            entry['holiday'] = judge_holidays(datetime.date(*[int(x) for x in entry['orderDate'].split('-')]), entry['state'])
            writer.writerow(entry)

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        d = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        paths = [os.path.join(d, 'task', 'orders_class.txt'),
                 os.path.join(d, 'task', 'orders_train.txt')]
    else:
        paths = sys.argv[1:]
    for path in paths:
        base, ext = os.path.splitext(path)
        main(path, base + '-w-holidays' + ext)
