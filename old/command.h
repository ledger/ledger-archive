#if 0
class format_transactions : public item_handler<transaction_t>
{
 protected:
  std::ostream&   output_stream;
  format_t	  first_line_format;
  format_t	  next_lines_format;
  entry_t *       last_entry;
  transaction_t * last_xact;

 public:
  format_transactions(std::ostream& _output_stream,
		      const std::string& format);

  virtual void flush() {
    output_stream.flush();
  }
  virtual void operator()(transaction_t& xact);
};

class format_entries : public format_transactions
{
 public:
  format_entries(std::ostream& output_stream, const std::string& format)
    : format_transactions(output_stream, format) {}

  virtual void format_last_entry();

  virtual void flush() {
    if (last_entry) {
      format_last_entry();
      last_entry = NULL;
    }
    format_transactions::flush();
  }
  virtual void operator()(transaction_t& xact);
};

bool disp_subaccounts_p(const account_t& account,
			const item_predicate<account_t>& disp_pred,
			const account_t *& to_show);

inline bool disp_subaccounts_p(const account_t& account) {
  const account_t * temp;
  return disp_subaccounts_p(account, item_predicate<account_t>(NULL), temp);
}

bool display_account(const account_t& account,
		     const item_predicate<account_t>& disp_pred);

class format_account : public item_handler<account_t>
{
  std::ostream& output_stream;

  item_predicate<account_t> disp_pred;

 public:
  format_t format;

  format_account(std::ostream&      _output_stream,
		 const std::string& _format,
		 const std::string& display_predicate = NULL)
    : output_stream(_output_stream), disp_pred(display_predicate),
      format(_format) {}

  virtual void flush() {
    output_stream.flush();
  }

  virtual void operator()(account_t& account);
};

class format_equity : public item_handler<account_t>
{
  std::ostream& output_stream;
  format_t	first_line_format;
  format_t	next_lines_format;

  item_predicate<account_t> disp_pred;

  mutable value_t total;

 public:
  format_equity(std::ostream&      _output_stream,
		const std::string& _format,
		const std::string& display_predicate);

  virtual void flush();
  virtual void operator()(account_t& account);
};

#if 0
  if (command->wants_args) {
  } else {
    string regexps[4];

    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    int base = 0;
    for (strings_list::iterator i = arg; i != args.end(); i++)
      if ((*i)[0] == '-') {
	if ((*i)[1] == '-') {
	  if (base == 0)
	    base += 2;
	  continue;
	}
	if (! regexps[base + 1].empty())
	  regexps[base + 1] += "|";
	regexps[base + 1] += (*i).substr(1);
      } else {
	if (! regexps[base].empty())
	  regexps[base] += "|";
	regexps[base] += *i;
      }

#if 0
    // jww (2006-09-21): Escape the \ in these strings!

    if (! regexps[3].empty())
      report->transforms.push_front
	(new remove_transform
	 (string("//entry[payee =~ /(") + regexps[3] + ")/]"));

    if (! regexps[2].empty())
      report->transforms.push_front
	(new select_transform
	 (string("//entry[payee =~ /(") + regexps[2] + ")/]"));

    if (! regexps[1].empty())
      report->transforms.push_front
	(new remove_transform
	 (string("//xact[account =~ /(") + regexps[1] + ")/]"));

    if (! regexps[0].empty())
      report->transforms.push_front
	(new select_transform
	 (string("//xact[account =~ /(") + regexps[0] + ")/]"));
#endif
  }
#endif

#endif
