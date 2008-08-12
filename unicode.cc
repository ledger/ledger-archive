#include <iostream>
#include <string>
#include <boost/program_options/detail/convert.hpp>

int main()
{
  using namespace std;

  wstring test(L"€");

  cout << " test.length = " << test.length() << endl;
  cout << "wcslen(test) = " << wcslen(test.c_str()) << endl;
  cout << "     test[0] = '" << test[0] << "'" << endl;
  cout << "        test = '" << boost::to_utf8(test) << "'" << endl;

  cout.width(4);
  cout << right << boost::to_utf8(test) << endl;
}
