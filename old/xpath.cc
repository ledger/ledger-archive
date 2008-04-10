#if 0
xpath_t::ptr_op_t
xpath_t::parse_querycolon_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_or_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::QUESTION) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_QUES);
      node->set_left(prev);
      node->set_right(new op_t(op_t::O_COLON));
      node->right()->set_left(parse_querycolon_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
      tok = next_token(in, tflags);
      if (tok.kind != token_t::COLON)
	tok.unexpected();	// jww (2006-09-09): wanted :
      node->right()->set_right(parse_querycolon_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node;
}
#endif

#if 0
xpath_t::context::context(const xpath_t&  _xpath,
			  const ptr_op_t& _err_node,
			  const string&   desc) throw()
  : error_context(desc), xpath(_xpath), err_node(_err_node)
{
}

void xpath_t::context::describe(std::ostream& out) const throw()
{
  if (! xpath) {
    out << "xpath_t::context expr not set!" << std::endl;
    return;
  }

  if (! desc.empty())
    out << desc << std::endl;

  out << "  ";
  unsigned long start = (long)out.tellp() - 1;
  unsigned long begin;
  unsigned long end;
  bool found = false;
  if (xpath)
    xpath.print(out, true, err_node, &begin, &end);
  out << std::endl;
  if (found) {
    out << "  ";
    for (unsigned int i = 0; i < end - start; i++) {
      if (i >= begin - start)
	out << "^";
      else
	out << " ";
    }
    out << std::endl;
  }
}
#endif

