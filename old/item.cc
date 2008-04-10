#if 0
  case AMOUNT:
    if (details.xact) {
      if (transaction_has_xdata(*details.xact) &&
	  transaction_xdata_(*details.xact).dflags & TRANSACTION_COMPOUND)
	result = transaction_xdata_(*details.xact).value;
      else
	result = details.xact->amount;
    }
    else if (details.account && account_has_xdata(*details.account)) {
      result = account_xdata(*details.account).value;
    }
    else {
      result = 0L;
    }
    break;

  case PRICE:
    if (details.xact) {
      bool set = false;
      if (transaction_has_xdata(*details.xact)) {
	transaction_xdata_t& xdata(transaction_xdata_(*details.xact));
	if (xdata.dflags & TRANSACTION_COMPOUND) {
	  result = xdata.value.price();
	  set = true;
	}
      }
      if (! set)
	result = details.xact->amount.price();
    }
    else if (details.account && account_has_xdata(*details.account)) {
      result = account_xdata(*details.account).value.price();
    }
    else {
      result = 0L;
    }
    break;

  case COST:
    if (details.xact) {
      bool set = false;
      if (transaction_has_xdata(*details.xact)) {
	transaction_xdata_t& xdata(transaction_xdata_(*details.xact));
	if (xdata.dflags & TRANSACTION_COMPOUND) {
	  result = xdata.value.cost();
	  set = true;
	}
      }

      if (! set) {
	if (details.xact->cost)
	  result = *details.xact->cost;
	else
	  result = details.xact->amount;
      }
    }
    else if (details.account && account_has_xdata(*details.account)) {
      result = account_xdata(*details.account).value.cost();
    }
    else {
      result = 0L;
    }
    break;

  case TOTAL:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = transaction_xdata_(*details.xact).total;
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total;
    else
      result = 0L;
    break;
  case PRICE_TOTAL:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = transaction_xdata_(*details.xact).total.price();
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total.price();
    else
      result = 0L;
    break;
  case COST_TOTAL:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = transaction_xdata_(*details.xact).total.cost();
    else if (details.account && account_has_xdata(*details.account))
      result = account_xdata(*details.account).total.cost();
    else
      result = 0L;
    break;

  case VALUE_EXPR:
    if (amount_expr.get())
      amount_expr->compute(result, args);
    else
      result = 0L;
    break;
  case TOTAL_EXPR:
    if (total_expr.get())
      total_expr->compute(result, args);
    else
      result = 0L;
    break;

  case DATE:
    if (details.xact && transaction_has_xdata(*details.xact) &&
	transaction_xdata_(*details.xact).date)
      result = transaction_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->date();
    else if (details.entry)
      result = details.entry->date();
    else
      result = terminus;
    break;

  case ACT_DATE:
    if (details.xact && transaction_has_xdata(*details.xact) &&
	transaction_xdata_(*details.xact).date)
      result = transaction_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->actual_date();
    else if (details.entry)
      result = details.entry->actual_date();
    else
      result = terminus;
    break;

  case EFF_DATE:
    if (details.xact && transaction_has_xdata(*details.xact) &&
	transaction_xdata_(*details.xact).date)
      result = transaction_xdata_(*details.xact).date;
    else if (details.xact)
      result = details.xact->effective_date();
    else if (details.entry)
      result = details.entry->effective_date();
    else
      result = terminus;
    break;

  case CLEARED:
    if (details.xact)
      result = details.xact->state == transaction_t::CLEARED;
    else
      result = false;
    break;
  case PENDING:
    if (details.xact)
      result = details.xact->state == transaction_t::PENDING;
    else
      result = false;
    break;

  case REAL:
    if (details.xact)
      result = ! (details.xact->flags & TRANSACTION_VIRTUAL);
    else
      result = true;
    break;

  case ACTUAL:
    if (details.xact)
      result = ! (details.xact->flags & TRANSACTION_AUTO);
    else
      result = true;
    break;

  case INDEX:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = long(transaction_xdata_(*details.xact).index + 1);
    else if (details.account && account_has_xdata(*details.account))
      result = long(account_xdata(*details.account).count);
    else
      result = 0L;
    break;

  case COUNT:
    if (details.xact && transaction_has_xdata(*details.xact))
      result = long(transaction_xdata_(*details.xact).index + 1);
    else if (details.account && account_has_xdata(*details.account))
      result = long(account_xdata(*details.account).total_count);
    else
      result = 0L;
    break;

  case DEPTH:
    if (details.account)
      result = long(details.account->depth);
    else
      result = 0L;
    break;

  case F_PRICE: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result = result.price();
    break;
  }

  case F_DATE: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result = result.date();
    break;
  }

  case F_DATECMP: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result = result.date();
    if (! result)
      break;

    arg_index = 0;
    expr = find_leaf(args, 1, arg_index);
    value_t moment;
    expr->compute(moment, args);
    if (moment.type == value_t::DATETIME) {
      result.cast(value_t::INTEGER);
      moment.cast(value_t::INTEGER);
      result -= moment;
    } else {
      throw new compute_error("Invalid date passed to datecmp(value,date)",
			      new valexpr_context(expr));
    }
    break;
  }

  case F_YEAR:
  case F_MONTH:
  case F_DAY: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);

    if (result.type != value_t::DATETIME)
      throw new compute_error("Invalid date passed to year|month|day(date)",
			      new valexpr_context(expr));

    datetime_t& moment(*((datetime_t *)result.data));
    switch (kind) {
    case F_YEAR:
      result = (long)moment.year();
      break;
    case F_MONTH:
      result = (long)moment.month();
      break;
    case F_DAY:
      result = (long)moment.day();
      break;
    }
    break;
  }

  case F_ARITH_MEAN: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    if (details.xact && transaction_has_xdata(*details.xact)) {
      expr->compute(result, args);
      result /= amount_t(long(transaction_xdata_(*details.xact).index + 1));
    }
    else if (details.account && account_has_xdata(*details.account) &&
	     account_xdata(*details.account).total_count) {
      expr->compute(result, args);
      result /= amount_t(long(account_xdata(*details.account).total_count));
    }
    else {
      result = 0L;
    }
    break;
  }

  case F_PARENT:
    if (details.account && details.account->parent)
      left->compute(result, details_t(*details.account->parent), args);
    break;

  case F_ABS: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result.abs();
    break;
  }

  case F_ROUND: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    result.round();
    break;
  }

  case F_COMMODITY: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);
    if (result.type != value_t::AMOUNT)
      throw new compute_error("Argument to commodity() must be a commoditized amount",
			      new valexpr_context(expr));
    amount_t temp("1");
    temp.set_commodity(((amount_t *) result.data)->commodity());
    result = temp;
    break;
  }

  case F_SET_COMMODITY: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    value_t temp;
    expr->compute(temp, args);

    arg_index = 0;
    expr = find_leaf(args, 1, arg_index);
    expr->compute(result, args);
    if (result.type != value_t::AMOUNT)
      throw new compute_error
	("Second argument to set_commodity() must be a commoditized amount",
	 new valexpr_context(expr));
    amount_t one("1");
    one.set_commodity(((amount_t *) result.data)->commodity());
    result = one;

    result *= temp;
    break;
  }

  case F_QUANTITY: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);

    balance_t * bal = NULL;
    switch (result.type) {
    case value_t::BALANCE_PAIR:
      bal = &((balance_pair_t *) result.data)->quantity;
      // fall through...

    case value_t::BALANCE:
      if (! bal)
	bal = (balance_t *) result.data;

      if (bal->amounts.size() < 2) {
	result.cast(value_t::AMOUNT);
      } else {
	value_t temp;
	for (amounts_map::const_iterator i = bal->amounts.begin();
	     i != bal->amounts.end();
	     i++) {
	  amount_t x = (*i).second;
	  x.clear_commodity();
	  temp += x;
	}
	result = temp;
	assert(temp.type == value_t::AMOUNT);
      }
      // fall through...

    case value_t::AMOUNT:
      ((amount_t *) result.data)->clear_commodity();
      break;

    default:
      break;
    }
    break;
  }

  case F_CODE_MASK:
    assert(mask);
    if (details.entry)
      result = mask->match(details.entry->code);
    else
      result = false;
    break;

  case F_PAYEE_MASK:
    assert(mask);
    if (details.entry)
      result = mask->match(details.entry->payee);
    else
      result = false;
    break;

  case F_NOTE_MASK:
    assert(mask);
    if (details.xact)
      result = mask->match(details.xact->note);
    else
      result = false;
    break;

  case F_ACCOUNT_MASK:
    assert(mask);
    if (details.account)
      result = mask->match(details.account->fullname());
    else
      result = false;
    break;

  case F_SHORT_ACCOUNT_MASK:
    assert(mask);
    if (details.account)
      result = mask->match(details.account->name);
    else
      result = false;
    break;

  case F_COMMODITY_MASK:
    assert(mask);
    if (details.xact)
      result = mask->match(details.xact->amount.commodity().base_symbol());
    else
      result = false;
    break;

  case F_VALUE: {
    int arg_index = 0;
    value_expr_t * expr = find_leaf(args, 0, arg_index);
    expr->compute(result, args);

    arg_index = 0;
    expr = find_leaf(args, 1, arg_index);
    value_t moment;
    expr->compute(moment, args);
    if (moment.type != value_t::DATETIME)
      throw new compute_error("Invalid date passed to P(value,date)",
			      new valexpr_context(expr));

    result = result.value(*((datetime_t *)moment.data));
    break;
  }
#endif

#if 0
    case element_t::AMOUNT:
    case element_t::TOTAL:
    case element_t::VALUE_EXPR: {
      value_expr calc;
      switch (elem->type) {
      case element_t::AMOUNT:     calc = amount_expr; break;
      case element_t::TOTAL:      calc = total_expr; break;
      case element_t::VALUE_EXPR: calc = elem->val_expr; break;
      default:
	assert(0);
	break;
      }
      if (! calc)
	break;

      value_t     value;
      balance_t * bal = NULL;

      calc->compute(value, details);

      if (! amount_t::keep_price ||
	  ! amount_t::keep_date ||
	  ! amount_t::keep_tag) {
	switch (value.type) {
	case value_t::AMOUNT:
	case value_t::BALANCE:
	case value_t::BALANCE_PAIR:
	  value = value.strip_annotations();
	  break;
	default:
	  break;
	}
      }

      bool highlighted = false;

      switch (value.type) {
      case value_t::BOOLEAN:
	out << (*((bool *) value.data) ? "true" : "false");
	break;

      case value_t::INTEGER:
	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*((long *) value.data) > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*((long *) value.data) < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	out << *((long *) value.data);
	break;

      case value_t::DATETIME:
	out << *((datetime_t *) value.data);
	break;

      case value_t::AMOUNT:
	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*((amount_t *) value.data) > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*((amount_t *) value.data) < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	out << *((amount_t *) value.data);
	break;

      case value_t::BALANCE:
	bal = (balance_t *) value.data;
	// fall through...

      case value_t::BALANCE_PAIR:
	if (! bal)
	  bal = &((balance_pair_t *) value.data)->quantity;

	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*bal > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*bal < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	bal->write(out, elem->min_width,
		   (elem->max_width > 0 ?
		    elem->max_width : elem->min_width));

	ignore_max_width = true;
	break;
      default:
	assert(0);
	break;
      }

      if (highlighted)
	mark_plain(out);
      break;
    }

    case element_t::OPT_AMOUNT:
      if (details.xact) {
	std::string disp;
	bool use_disp = false;

	if (details.xact->cost && details.xact->amount) {
	  std::ostringstream stream;
	  if (! details.xact->amount_expr.expr.empty())
	    stream << details.xact->amount_expr.expr;
	  else
	    stream << details.xact->amount.strip_annotations();

	  if (! details.xact->cost_expr.empty())
	    stream << details.xact->cost_expr;
	  else
	    stream << " @ " << amount_t(*details.xact->cost /
					details.xact->amount).unround();
	  disp = stream.str();
	  use_disp = true;
	}
	else if (details.entry) {
	  unsigned int    xacts_count = 0;
	  transaction_t * first = NULL;
	  transaction_t * last  = NULL;

	  for (transactions_list::const_iterator i
		 = details.entry->transactions.begin();
	       i != details.entry->transactions.end();
	       i++)
	    if (transaction_has_xdata(**i) &&
		transaction_xdata_(**i).dflags & TRANSACTION_TO_DISPLAY) {
	      xacts_count++;
	      if (! first)
		first = *i;
	      last = *i;
	    }

	  use_disp = (xacts_count == 2 && details.xact == last &&
		      first->amount == - last->amount);
	}

	if (! use_disp) {
	  if (! details.xact->amount_expr.expr.empty())
	    out << details.xact->amount_expr.expr;
	  else
	    out << details.xact->amount.strip_annotations();
	} else {
	  out << disp;
	}
      }
      break;

    case element_t::SOURCE:
      if (details.entry && details.entry->journal) {
	int idx = details.entry->src_idx;
	for (strings_list::iterator i = details.entry->journal->sources.begin();
	     i != details.entry->journal->sources.end();
	     i++)
	  if (! idx--) {
	    out << *i;
	    break;
	  }
      }
      break;

    case element_t::ENTRY_BEG_POS:
      if (details.entry)
	out << (unsigned long)details.entry->beg_pos;
      break;

    case element_t::ENTRY_BEG_LINE:
      if (details.entry)
	out << details.entry->beg_line;
      break;

    case element_t::ENTRY_END_POS:
      if (details.entry)
	out << (unsigned long)details.entry->end_pos;
      break;

    case element_t::ENTRY_END_LINE:
      if (details.entry)
	out << details.entry->end_line;
      break;

    case element_t::XACT_BEG_POS:
      if (details.xact)
	out << (unsigned long)details.xact->beg_pos;
      break;

    case element_t::XACT_BEG_LINE:
      if (details.xact)
	out << details.xact->beg_line;
      break;

    case element_t::XACT_END_POS:
      if (details.xact)
	out << (unsigned long)details.xact->end_pos;
      break;

    case element_t::XACT_END_LINE:
      if (details.xact)
	out << details.xact->end_line;
      break;

    case element_t::DATE_STRING: {
      datetime_t date;
      if (details.xact)
	date = details.xact->date();
      else if (details.entry)
	date = details.entry->date();

      char buf[256];
      std::strftime(buf, 255, elem->chars.c_str(), date.localtime());
      out << (elem->max_width == 0 ? buf : truncate(buf, elem->max_width));
      break;
    }

    case element_t::COMPLETE_DATE_STRING: {
      datetime_t actual_date;
      datetime_t effective_date;
      if (details.xact) {
	actual_date    = details.xact->actual_date();
	effective_date = details.xact->effective_date();
      }
      else if (details.entry) {
	actual_date    = details.entry->actual_date();
	effective_date = details.entry->effective_date();
      }

      char abuf[256];
      std::strftime(abuf, 255, elem->chars.c_str(), actual_date.localtime());

      if (effective_date && effective_date != actual_date) {
	char buf[512];
	char ebuf[256];
	std::strftime(ebuf, 255, elem->chars.c_str(),
		      effective_date.localtime());

	std::strcpy(buf, abuf);
	std::strcat(buf, "=");
	std::strcat(buf, ebuf);

	out << (elem->max_width == 0 ? buf : truncate(buf, elem->max_width));
      } else {
	out << (elem->max_width == 0 ? abuf : truncate(abuf, elem->max_width));
      }
      break;
    }

    case element_t::CLEARED:
      if (details.xact) {
	switch (details.xact->state) {
	case transaction_t::CLEARED:
	  out << "* ";
	  break;
	case transaction_t::PENDING:
	  out << "! ";
	  break;
	}
      }
      break;

    case element_t::ENTRY_CLEARED:
      if (details.entry) {
	transaction_t::state_t state;
	if (details.entry->get_state(&state))
	  switch (state) {
	  case transaction_t::CLEARED:
	    out << "* ";
	    break;
	  case transaction_t::PENDING:
	    out << "! ";
	    break;
	  }
      }
      break;

    case element_t::CODE: {
      std::string temp;
      if (details.entry && ! details.entry->code.empty()) {
	temp += "(";
	temp += details.entry->code;
	temp += ") ";
      }
      out << temp;
      break;
    }

    case element_t::PAYEE:
      if (details.entry)
	out << (elem->max_width == 0 ?
		details.entry->payee : truncate(details.entry->payee,
						elem->max_width));
      break;

    case element_t::OPT_NOTE:
      if (details.xact && ! details.xact->note.empty())
	out << "  ; ";
      // fall through...

    case element_t::NOTE:
      if (details.xact)
	out << (elem->max_width == 0 ?
		details.xact->note : truncate(details.xact->note,
					      elem->max_width));
      break;

    case element_t::OPT_ACCOUNT:
      if (details.entry && details.xact) {
	transaction_t::state_t state;
	if (! details.entry->get_state(&state))
	  switch (details.xact->state) {
	  case transaction_t::CLEARED:
	    name = "* ";
	    break;
	  case transaction_t::PENDING:
	    name = "! ";
	    break;
	  }
      }
      // fall through...

    case element_t::ACCOUNT_NAME:
    case element_t::ACCOUNT_FULLNAME:
      if (details.account) {
	name += (elem->type == element_t::ACCOUNT_FULLNAME ?
		 details.account->fullname() :
		 partial_account_name(*details.account));

	if (details.xact && details.xact->flags & TRANSACTION_VIRTUAL) {
	  if (elem->max_width > 2)
	    name = truncate(name, elem->max_width - 2, true);

	  if (details.xact->flags & TRANSACTION_BALANCE)
	    name = "[" + name + "]";
	  else
	    name = "(" + name + ")";
	}
	else if (elem->max_width > 0)
	  name = truncate(name, elem->max_width, true);

	out << name;
      } else {
	out << " ";
      }
      break;

    case element_t::SPACER:
      out << " ";
      break;

    case element_t::DEPTH_SPACER:
      for (const account_t * acct = details.account;
	   acct;
	   acct = acct->parent)
	if (account_has_xdata(*acct) &&
	    account_xdata_(*acct).dflags & ACCOUNT_DISPLAYED) {
	  if (elem->min_width > 0 || elem->max_width > 0)
	    out.width(elem->min_width > elem->max_width ?
		      elem->min_width : elem->max_width);
	  out << " ";
	}
      break;
#endif
