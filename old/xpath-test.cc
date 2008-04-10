#ifdef TEST

#if ! defined(HAVE_EXPAT) && ! defined(HAVE_XMLPARSE)
#error No XML parser library was found during configure
#endif

#if 0
#include "session.h"
#include "format.h"
#endif

int main(int argc, char *argv[])
{
  using namespace ledger;
  using namespace ledger::xml;

  try {
    parser_t parser;
    std::auto_ptr<document_t> doc;

    std::ifstream input(argv[1]);
    if (parser.test(input)) {
      doc.reset(parser.parse(input));
      doc->write(std::cout);
    } else {
      std::cerr << "Could not parse XML file: " << argv[1] << std::endl;
      return 1;
    }

    xpath_t expr(argv[2]);
    if (expr) {
      std::cout << "Parsed:" << std::endl;
      expr.dump(std::cout);
      std::cout << std::endl;

      expr.compile(doc.get());
      std::cout << "Compiled:" << std::endl;
      expr.dump(std::cout);
      std::cout << std::endl;

      value_t temp;
      expr.calc(temp, doc->top);
      std::cout << "Calculated value: " << temp << std::endl;
    } else {
      std::cerr << "Failed to parse value expression!" << std::endl;
    }

#if 0
    {
      ledger::session_t session;
      std::auto_ptr<xpath_t::scope_t>
	locals(new xpath_t::scope_t(&session.globals));

      ledger::format_t fmt(std::string("%20|%40{") + argv[1] + "}\n");
      fmt.format(std::cout, locals.get());
    }
#endif
  }
  catch (error * err) {
    std::cout.flush();
    if (err->context.empty())
      err->context.push_front(new error_context(""));
    err->reveal_context(std::cerr, "Error");
    std::cerr << err->what() << std::endl;
    delete err;
    return 1;
  }
  catch (fatal * err) {
    std::cout.flush();
    if (err->context.empty())
      err->context.push_front(new error_context(""));
    err->reveal_context(std::cerr, "Fatal");
    std::cerr << err->what() << std::endl;
    delete err;
    return 1;
  }
  catch (const std::exception& err) {
    std::cout.flush();
    std::cerr << "Error: " << err.what() << std::endl;
    return 1;
  }
}

#endif // TEST
