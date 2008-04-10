#if 0
template <typename T, typename U, typename V = int, typename W = int>
struct pystream_handler_wrap : public ledger::item_handler<U>
{
  PyFileObject * file;
  pyofstream *	 output;

  T handler;

  pystream_handler_wrap(PyObject * file_)
    : file((PyFileObject *)file_), output(new pyofstream(file)),
      handler(*output) {}
  pystream_handler_wrap(PyObject * file_, const V& arg)
    : file((PyFileObject *)file_), output(new pyofstream(file)),
      handler(*output, arg) {}
  pystream_handler_wrap(PyObject * file_, const V& arg1, const W& arg2)
    : file((PyFileObject *)file_), output(new pyofstream(file)),
      handler(*output, arg1, arg2) {}

  virtual ~pystream_handler_wrap() {
    delete output;
  }

  virtual void flush() {
    handler.flush();
  }
  virtual void operator()(U& item) {
    handler.operator()(item);
  }
};
#endif
