# libledger.py - simple tools to extract data from a ledger file
# Simon Michael 2007
#
# This was developed for ledgerplot.py experiments.
# This scrapes ledger's standard output in a dumb and version-specific way.
# Consider using ledger xml output or using beancount's parser and data
# structures instead.

import os, time

LEDGER = 'ledger2.5'
YEAR = time.localtime()[0]
PERIOD = ''
ACCTSEP = ':'

# get lines of ledger register report in period
def get_register_lines(period=''): # period_arg -> [string]
    return os.popen('%s %s register' % (LEDGER,period)).readlines()

dates = [l.split()[0] for l in get_register_lines(period=PERIOD) if l[0] is not ' ']
start, end = dates[0], dates[-1]

def nocomma(s): return s.replace(',','')

# extract account name & balance from a ledger balance report line
def get_balance_fields(l): # string -> (string, float)
    amt, acc = l.strip().split("  ",1)
    if ACCTSEP in acc: acc = acc[acc.rfind(ACCTSEP)+1:] # account name was merged with parent
    return (acc.strip(), abs(float(nocomma((amt[1:])))))

# get lines of ledger balance report for level 2 accounts under acct in period
def get_l2_balance_lines(acct,period=''): # acct1_name period_arg -> [string]
    lines = os.popen('%s -s -d "l<3" %s balance %s' % (LEDGER,period,acct)).readlines()[:-2]
    if len(lines) > 1: lines = lines[1:]
    return lines

# get the account name, balance pairs immediately below a level 1 account in period
def get_balance_data(acc,period): # acct1_name period_arg -> [(acct2_name, float)]
    l = map(get_balance_fields, get_l2_balance_lines('^'+acc,period=period))
    l.reverse()
    return l

# overall balances
INCOME   = get_balance_data('income',PERIOD)
EXPENSES = get_balance_data('expenses',PERIOD)

# how to present account balances ?
# snapshot:
# vertical amount bars, accounts on x
# horizontal amount bars, accounts on y
# amounts pie with account labels
# over time:
# vertical amount bars, accounts on x, bars per time interval
# horizontal amount bars, accounts on y, bars per time interval
# sequence of amounts pies with account labels
# horizontal amount lines per account, time on x

# how to acquire data for balances over time ?
## class Period:
##     value = None # None | datetime.interval
##     def __init__(self,v): self.value = v and datetime.interval(v) or None
##     def __str__(self): return self.value and '-p %s' % self.value or ''
# Period = datetime.interval
#
# # split a period into months
# def months_in_period(p): # period -> [period]
#     return [p] #XXX
#
# INCOME = {}
# for m in months_in_period(PERIOD):
#     INCOME[m] = get_balance_data('income',PERIOD)

def period_for_month(m,y=YEAR): # number -> period_arg
    return '-p "from %s/%s/1 to %s/%s/1"' % (y,m,y,m+1)

# gather balances per month:
# 1: [('consulting',5000),('salary',4000)],
# 2: [('consulting',6000),('salary',0)],
# 3: [('consulting',None),('salary',7000)],
INCOME_MONTHLY_ACCOUNT_BALANCES = {}
for m in range(1,13):
    INCOME_MONTHLY_ACCOUNT_BALANCES[m] = get_balance_data('income',
                                                          period_for_month(m))
# transpose:
# 'consulting':[(1,5000),(2,6000)],
# 'salary':[(1,4000),(2,0),(3,7000)],
INCOME_ACCOUNT_MONTHLY_BALANCES = {}
for m, balances in INCOME_MONTHLY_ACCOUNT_BALANCES.items():
    for a,b in balances:
        l = INCOME_ACCOUNT_MONTHLY_BALANCES.get(a,[])
        l.append((m,b))
        INCOME_ACCOUNT_MONTHLY_BALANCES[a] = l

EXPENSES_MONTHLY_ACCOUNT_BALANCES = {}
for m in range(1,13):
    EXPENSES_MONTHLY_ACCOUNT_BALANCES[m] = get_balance_data('expenses',
                                                            period_for_month(m))
EXPENSES_ACCOUNT_MONTHLY_BALANCES = {}
for m, balances in EXPENSES_MONTHLY_ACCOUNT_BALANCES.items():
    for a,b in balances:
        l = EXPENSES_ACCOUNT_MONTHLY_BALANCES.get(a,[])
        l.append((m,b))
        EXPENSES_ACCOUNT_MONTHLY_BALANCES[a] = l

