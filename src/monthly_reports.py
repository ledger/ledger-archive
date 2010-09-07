#!/usr/bin/python

import datetime
import subprocess
import sys

STARTING_MONTH=6
STARTING_YEAR=2007

MONTHS_IN_YEAR = 12

COMMAND_BEGIN=["/Users/travishartwell/Products/ledger/ledger", "-f", "/Users/travishartwell/Personal/Finance/ledger"]

PERIOD_REPORT="-p"
MONTHLY_REPORT="-e"

def get_next_month_and_year(month, year):
    next_month = (month % MONTHS_IN_YEAR) + 1

    if next_month == 1:
        next_year = year + 1
    else:
        next_year = year

    return next_month, next_year

    
def get_date_range(start_month, start_year, end_month, end_year):
    dates = []

    month = start_month ; year = start_year

    end_date = datetime.date(end_year, end_month, 1)
    date = datetime.date(year, month, 1)
    
    while date <= end_date:
        dates.append(date.strftime("%Y/%m/%d"))

        month, year = get_next_month_and_year(month, year)
	date = datetime.date(year, month, 1)

    return dates


def main():
    today = datetime.date.today()
    
    current_month = today.month
    current_year = today.year

    # We want to go through the beginning of next month
    next_month, next_year = get_next_month_and_year(current_month, current_year)
    dates = get_date_range(STARTING_MONTH, STARTING_YEAR, next_month, next_year)

    if sys.argv[1] == PERIOD_REPORT:
        report_style = PERIOD_REPORT
    elif sys.argv[1] == MONTHLY_REPORT:
        report_style = MONTHLY_REPORT

    arguments = sys.argv[2:]

    for index in range(len(dates) - 1):
        current = dates[index]
        next = dates[index + 1]

        if report_style == PERIOD_REPORT:
            cmd_line = COMMAND_BEGIN + ["-p", "from " + current + " to " + next] + arguments
        else:
            cmd_line = COMMAND_BEGIN + ["-e", next] + arguments
            
        print current + " - " + next + ":"

        process = subprocess.Popen(cmd_line, stdout=subprocess.PIPE)
        process.wait()

        for line in process.stdout:
            print line.strip()
            
        print 

if __name__=="__main__":
    main()
