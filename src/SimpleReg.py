import ledger
import re
import sys

account_re = sys.argv[1]

# A poor man's register report

running_total = ledger.Value(0)

for post in ledger.Journal("/Users/johnw/src/ledger/doc/sample.dat").collect("-M assets"):
    print "%s %-20.20s %-20.20s %12s %12s" % \
        (post.xact.date, post.xact.payee, post.account, post.amount,
         running_total)

print "Got to here!"

#running_total = ledger.Value(0)
#
#for txn in ledger.Journal("/Users/johnw/src/ledger/doc/sample.dat"):
#    for post in txn:
#        if re.match(account_re, str(post.account)):
#            running_total += post.amount
#            print "%s %-20.20s %-20.20s %12s %12s" % \
#                (txn.date, txn.payee, post.account, post.amount,
#                 running_total)
