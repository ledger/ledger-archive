#!/usr/bin/env python
# convert wells fargo CSV account data to ledger format
# Simon Michael 2008
# inspired by http://sixthdev.versionhost.com/viewvc.cgi/ledger/bofaconvert.py?view=markup
# Usage: wellsfargocsv2ledger.py Checking1.csv >Checking1.ledger

import sys, re, csv
from decimal import Decimal

thisacct           = 'assets:wells fargo:checking'
unknownexpenseacct = 'expenses'
unknownincomeacct  = 'income'

# add rules here to better identify your bank transactions
# an empty desc means use match group number 1, or the whole match
rules = [
    (r"ANTHEM BLUECROSS", "", "expenses:health:insurance"),
    (r"CHECK CRD PURCHASE ../.. (ONE LIFE NATURAL FOODS)", "", "expenses:food:groceries"),
    ]

def best_match_for(bankdesc,amount):
    for pattern,desc,acct in rules:
        m = re.match(pattern,bankdesc)
        if m: return (desc and desc or (m.groups() and m.group(1) or m.group())), acct
    return bankdesc, (amount > 0) and unknownexpenseacct or unknownincomeacct

def fixdate(d):
    m,d,y = d.split("/")
    return "/".join([y,m,d])

def print_ledger_entry(date,desc,acct,amount):
    print fixdate(date), desc
    print "    %-30s  %15s" % (acct, '$%s' % amount)
    print "    " + thisacct
    print

def main():
    infile = sys.argv[1]
    lines = csv.reader(open(infile, "rb"))
    for line in lines:
        date, amount, cleared, number, bankdesc = line
        amount = -Decimal(amount)
        desc, acct = best_match_for(bankdesc,amount)
        print_ledger_entry(date,desc,acct,abs(amount))

if __name__ == '__main__': main()
