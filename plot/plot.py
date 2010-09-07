#!/usr/bin/python
import web
import os
import sys
import datetime
import time
import decimal
import logging
from jinja2 import Environment, FileSystemLoader

logging.basicConfig(level=logging.DEBUG)

env = Environment(loader=FileSystemLoader(os.path.dirname(__file__) + '/templates/'))

urls = (
        '/?', 'plot',
        '/(\d\d\d\d.\d\d.\d\d)/(\d\d\d\d.\d\d.\d\d)/?', 'plot',
        '/register/(\d\d\d\d.\d\d.\d\d)/(\d\d\d\d.\d\d.\d\d)/?', 'register',
        '/balance/?', 'balance',
        )

app = web.application(urls, globals(), autoreload=True)
main = app.wsgifunc()

class plot:
    '''
    >>> response = app.request('/')
    >>> response.status
    '200 OK'
    >>> response = app.request('/2007.01.01/2008.01.01/')
    >>> response.status
    '200 OK'
    '''
    def GET(self, begin=None, end=None):
        tmpl = env.get_template('plot.html')
        if begin is not None:
            begin = parse_date(begin)
        else:
            #begin = datetime.date.today() + datetime.timedelta(-60)
            #begin = datetime.date(2006,1,1)
            begin = get_begin_date()
            logging.debug('found begin = %s', begin)
        if end is not None:
            end = parse_date(end)
        else:
            end = datetime.date.today()
        reg = get_register(begin, end)
        exp = get_register_exp(begin, end)
        assets = get_register_assets(begin, end)
        checking = get_register_checking(begin, end)
        expenses = get_expenses(begin, end)
        active = get_active(begin, end)
        today = datetime.date.today()
        return tmpl.render(reg=reg,exp=exp,assets=assets,
                checking=checking,expenseso=expenses[1],expenses=expenses[0],
                begin=begin,end=end,year=begin.year,active=active,
                exp_sum=sum([e[1] for e in expenses[0]]), today=today)

class register:
    '''
    '''
    def GET(self, begin=None, end=None):
        tmpl = env.get_template('register.html')
        if begin is not None:
            begin = parse_date(begin)
        else:
            begin = get_begin_date()
            logging.debug('found begin = %s', begin)
        if end is not None:
            end = parse_date(end)
        else:
            end = datetime.date.today()
        checking = get_register_checking(begin, end)
        active = get_active(begin, end)
        return tmpl.render(checking=checking,active=active)

class balance:
    '''
    '''
    def GET(self, begin=None, end=None):
        tmpl = env.get_template('register.html')
        checking = get_register_checking(begin, end)
        active = get_active(begin, end)
        return tmpl.render(checking=checking,active=active)

def get_active(begin, end):
    a = {
            'q1': '', 'q2': '', 'q3': '', 'q4': '',
            'jan': '', 'feb': '', 'mar': '', 'apr': '',
            'may': '', 'jun': '', 'jul': '', 'aug': '',
            'sep': '', 'oct': '', 'nov': '', 'dec': '',
        }

    if begin.month == 1 and end.month == 4 and begin.year == end.year:
        a['q1'] = 'active'
    elif begin.month == 4 and end.month == 7 and begin.year == end.year:
        a['q2'] = 'active'
    elif begin.month == 7 and end.month == 10 and begin.year == end.year:
        a['q3'] = 'active'
    elif begin.month == 10 and end.month == 1 and begin.year+1 == end.year:
        a['q4'] = 'active'
    elif begin.month == 1 and end.month == 2 and begin.year == end.year:
        a['jan'] = 'active'
    elif begin.month == 2 and end.month == 3 and begin.year == end.year:
        a['feb'] = 'active'
    elif begin.month == 3 and end.month == 4 and begin.year == end.year:
        a['mar'] = 'active'
    elif begin.month == 4 and end.month == 5 and begin.year == end.year:
        a['apr'] = 'active'
    elif begin.month == 5 and end.month == 6 and begin.year == end.year:
        a['may'] = 'active'
    elif begin.month == 6 and end.month == 7 and begin.year == end.year:
        a['jun'] = 'active'
    elif begin.month == 7 and end.month == 8 and begin.year == end.year:
        a['jul'] = 'active'
    elif begin.month == 8 and end.month == 9 and begin.year == end.year:
        a['aug'] = 'active'
    elif begin.month == 9 and end.month == 10 and begin.year == end.year:
        a['sep'] = 'active'
    elif begin.month == 10 and end.month == 11 and begin.year == end.year:
        a['oct'] = 'active'
    elif begin.month == 11 and end.month == 12 and begin.year == end.year:
        a['nov'] = 'active'
    elif begin.month == 12 and end.month == 1 and begin.year+1 == end.year:
        a['dec'] = 'active'

    return a

def get_begin_date():
    command = 'ledger --head 1 reg'
    logging.debug(command)
    lines = os.popen(command).readlines()
    return parse_account(lines[0], False)[0]

def parse_amount(amount):
    '''
    >>> parse_amount('$100.00')
    Decimal('100.00')
    >>> parse_amount('$-1,238.32')
    Decimal('-1238.32')
    >>> parse_amount('-1,238.32 USD')
    Decimal('-1238.32')
    >>> parse_amount('-1.238,32 EUR')
    Decimal('-1238.32')
    '''
    s = ''

    for c in amount:
        if c.isdigit() or c in ('.', '-', '+'):
            s += c

    return decimal.Decimal(s)

date_formats = (
        '%Y/%m/%d',
        '%Y.%m.%d',
        '%Y-%m-%d',
        )
def parse_date(date):
    '''
    >>> parse_date('2007/10/01')
    datetime.date(2007, 10, 1)
    >>> parse_date('2007.03.23')
    datetime.date(2007, 3, 23)
    >>> parse_date('2007-05-10')
    datetime.date(2007, 5, 10)
    >>> parse_date('2007/03')
    Traceback (most recent call last):
        ...
    ValueError: Could not parse date: 2007/03
    '''
    arr = None

    for format in date_formats:
        try:
            arr = time.strptime(date, format)
        except ValueError:
            pass

    if arr is None:
        raise ValueError('Could not parse date: %s' % date)

    return datetime.date(arr[0], arr[1], arr[2])

def parse_account(line, first=True):
    '''
    >>> parse_account('2007/10/01 - 2007/10/31         Income:Salary             $-951.81  $-19,867.53')
    (datetime.date(2007, 10, 1), Decimal('951.81'))
    >>> parse_account('2007/11/01 - 2007/11/30         Income:Salary             $-593.50  $-20,461.03')
    (datetime.date(2007, 11, 1), Decimal('593.50'))
    '''
    pos = line.find(' ')
    date = line[:pos]
    if first:
        pos = line.find('$')
        line = line[pos:]
        pos = line.find(' ')
        amount = line[:pos]
    else:
        pos = line.rfind('$')
        amount = line[pos:]
    return (parse_date(date), abs(parse_amount(amount)))

def parse_balance(lines):
    stack = []
    indentation = 2
    prev = ''
    result = []
    first = True
    for line in lines:
        cur_ind = 0
        pos = line.find('$')
        pos2 = line.find(' ', pos)
        a = line[pos:pos2]
        #print a
        amount = parse_amount(line[pos:pos2])
        while line[pos2] == ' ':
            pos2 += 1
            cur_ind += 1
        extra = ''
        if cur_ind > indentation:
            stack.append(prev)
            #extra = stack[0] + ':'
        elif cur_ind < indentation:
            stack.pop()
        elif not first:
            #extra = stack[0] + ':'
            pass
        indentation = cur_ind
        prev = line[pos2:line.find('\\')]
        result.append((prev[prev.find(':')+1:], amount))
        first = False

    return result

def get_register_assets(begin=None, end=None):
    t = end - begin
    if t.days < 32:
        period = ''
    elif t.days < 95:
        period = 'W'
    elif t.days < 365*5:
        period = 'M'
    else:
        period = 'Y'
    d = ''
    if begin is not None or end is not None:
        d = '-d "d>=[%s]&d<[%s]"' % (begin-datetime.timedelta(60), end+datetime.timedelta(60))
    a = []
    command = 'ledger -%snB %s reg ^assets ^liab' % (period, d)
    logging.debug(command)
    lines = os.popen(command)
    for line in lines:
        a.append(parse_account(line, False))
    return last_transactions(a)

def get_register(begin=None, end=None):
    t = end - begin
    if t.days < 32:
        period = ''
    elif t.days < 95:
        period = 'W'
    elif t.days < 365*5:
        period = 'M'
    else:
        period = 'Y'
    d = ''
    if begin is not None or end is not None:
        d = '-d "d>=[%s]&d<[%s]"' % (begin-datetime.timedelta(60), end+datetime.timedelta(60))
    a = []
    lines = os.popen('ledger -%snB %s reg ^income' % (period, d))
    for line in lines:
        a.append(parse_account(line))
    return zero_nonexistant(combine_transactions(a), period)

def get_register_checking(begin=None, end=None):
    d = ''
    if begin is not None or end is not None:
        d = '-d "d>=[%s]&d<[%s]"' % (begin, end)
    a = []
    command = 'ledger -c %s reg ^assets:bank:checking$ | tac' % d
    logging.debug(command)
    lines = os.popen(command)
    for line in lines:
        a.append([line.replace(' ', '&nbsp;'), line.find('-') != -1])
    return a

def get_register_exp(begin=None, end=None):
    t = end - begin
    if t.days < 32:
        period = ''
    elif t.days < 32*6:
        period = 'W'
    elif t.days < 365*5:
        period = 'M'
    else:
        period = 'Y'
    d = ''
    if begin is not None or end is not None:
        d = '-d "d>=[%s]&d<[%s]"' % (begin-datetime.timedelta(60), end+datetime.timedelta(60))
    a = []
    command = 'ledger -%snB %s reg ^expense' % (period, d)
    logging.debug(command)
    lines = os.popen(command)
    for line in lines:
        a.append(parse_account(line))
    return zero_nonexistant(combine_transactions(a), period)

def small_to_other(expenses, perc=4):
    '''
    Takes a list of transactions and combines everything that is less
    than perc into an other category.
    '''
    sum = 0
    for e in expenses:
        sum += e[1]

    flag = False
    got_first = False
    for e in expenses:
        if 100*e[1]/sum < perc:
            if got_first:
                flag = True
                break
            else:
                got_first = True

    # if not more than one category under the percent, do nothing
    if not flag:
        return expenses

    index = 0
    other = decimal.Decimal(0)
    while index < len(expenses):
        if 100*expenses[index][1]/sum < perc:
            other += expenses[index][1]
            expenses = expenses[:index] + expenses[index+1:]
        else:
            index += 1

    if other > 0:
        expenses.append(('Other', other))

    return expenses

def get_expenses(begin=None, end=None):
    a = []
    command = 'ledger -b "%s" -e "%s" -s -d "T" bal ^exp' % (begin,end)
    logging.debug(command)
    lines = os.popen(command).readlines()
    a = parse_balance(lines[1:-2])
    a.sort(key=lambda x: x[1], reverse=True)
    return (a, small_to_other(a))

def last_transactions(transactions):
    '''
    Merge all transactions with the same date, using the amount of the
    last transaction as the new amount.

    >>> from datetime import date
    >>> from decimal import Decimal
    >>> last_transactions([(date(2007,1,1), Decimal('100')), (date(2007,1,1), Decimal('123'))])
    [(datetime.date(2007, 1, 1), Decimal('123'))]
    '''
    index = 0
    prev = None
    while index < len(transactions):
        if prev is None:
            prev = transactions[index][0]
            index += 1
        elif transactions[index][0] == prev:
            transactions[index-1] = (transactions[index][0], transactions[index][1])
            transactions = transactions[:index] + transactions[index+1:]
        else:
            prev = transactions[index][0]
            index += 1

    return transactions

def combine_transactions(transactions):
    '''
    Merge all transactions with the same date, using the sum of the
    amounts as the new amount.

    >>> from datetime import date
    >>> from decimal import Decimal
    >>> combine_transactions([(date(2007,1,1), Decimal('100')), (date(2007,1,1), Decimal('100'))])
    [(datetime.date(2007, 1, 1), Decimal('200'))]
    '''
    index = 0
    prev = None
    while index < len(transactions):
        if prev is None:
            prev = transactions[index][0]
            index += 1
        elif transactions[index][0] == prev:
            transactions[index-1] = (transactions[index][0], transactions[index-1][1]+transactions[index][1])
            transactions = transactions[:index] + transactions[index+1:]
        else:
            prev = transactions[index][0]
            index += 1

    return transactions

def zero_nonexistant(transactions, period):
    '''
    Fills in missing periods with a zero amount. For instance, if ledger
    reports:
        January  -- $100
        April    -- $55
    Then we want to display for the graph:
        January  -- $100
        February -- $0
        March    -- $0
        April    -- $55

    >>> from datetime import date
    >>> from decimal import Decimal
    >>> len(zero_nonexistant([(date(2007,1,1), Decimal('100')), (date(2007,1,4), Decimal('100'))], ''))
    4
    >>> len(zero_nonexistant([(date(2007,1,1), Decimal('100')), (date(2007,1,15), Decimal('100'))], 'W'))
    3
    >>> len(zero_nonexistant([(date(2007,1,1), Decimal('100')), (date(2007,5,1), Decimal('100'))], 'M'))
    5
    '''
    if len(transactions) == 0:
        return []

    if period == '':
        next = lambda d: d + datetime.timedelta(1)
    elif period == 'W':
        next = lambda d: d + datetime.timedelta(7)
    elif period == 'M':
        def next(d):
            if d.month == 12:
                return datetime.date(d.year+1, 1, d.day)
            else:
                return datetime.date(d.year, d.month+1, d.day)
    elif period == 'Y':
        next = lambda d: datetime.date(d.year+1, d.month, d.day)
    else:
        raise Exception('period not handled')

    date = next(transactions[0][0])
    index = 1
    while index < len(transactions):
        if transactions[index][0] > date:
            transactions.insert(index, (date, decimal.Decimal('0')))
        index += 1
        date = next(date)
        if index > 200:
            break

    return transactions

if __name__ == "__main__":
    if 'test' in sys.argv:
        import doctest
        doctest.testmod()
    else:
        app.run()

