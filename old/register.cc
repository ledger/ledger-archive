/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "register.h"
#include "journal.h"

namespace ledger {

static void scan_for_transactions(std::ostream& out, const xml::node_t * node)
{
#if 0
  if (! node->has_flags(XML_NODE_IS_PARENT))
    return;

  foreach (const xml::node_t * child, node->as_parent_node()) {
    if (child->name_id == xml::TRANSACTION_NODE) {
      const xml::transaction_node_t * xact_node =
	dynamic_cast<const xml::transaction_node_t *>(child);
      assert(xact_node);

      const transaction_t * xact = xact_node->transaction;
      assert(xact);

      out << xact->entry->date() << ' '
	  << std::setw(21) << std::left
	  << abbreviate(xact->entry->payee, 21) << ' '
	  << std::setw(21) << std::left
	  << abbreviate(xact->account->fullname(), 21,
			ABBREVIATE, true) << ' '
	  << std::setw(12) << std::right;
      if (xact->amount)
	out << *xact->amount;
      out << '\n';
    } else {
      scan_for_transactions(out, child);
    }
#endif
}

void register_command::print_document(std::ostream&	out,
				      xml::document_t * doc)
{
#if 1
  scan_for_transactions(out, doc);
  out.flush();
#else
  value_t nodelist;
  xml::xpath_t::eval(nodelist, "//transaction", doc);

  value_t::sequence_t& xact_list(nodelist.as_sequence());
  assert(xact_list);

  for (value_t::sequence_t::const_iterator i = xact_list->begin();
       i != xact_list->end();
       i++) {
    const xml::node_t * node = (*i).as_xml_node();
    assert(node);

    const xml::transaction_node_t * xact_node =
      dynamic_cast<const xml::transaction_node_t *>(node);
    assert(xact_node);

    const transaction_t * xact = xact_node->transaction;
    assert(xact);

    std::cout << xact->entry->date() << ' '
	      << std::setw(21) << std::left
	      << abbreviate(xact->entry->payee, 21) << ' '
	      << std::setw(21) << std::left
	      << abbreviate(xact->account->fullname(), 21,
			    ABBREVIATE, true) << ' '
	      << std::setw(12) << std::right
	      << xact->amount
	      << std::endl;
  }
#endif
}

} // namespace ledger
