#ifndef UNISTREAM_HPP
#define UNISTREAM_HPP

#include <istream>
#include <ostream>
#include <streambuf>
#include <cstdio>

#include <unicode/unistr.h>
#include <unicode/ustring.h>
#include <unicode/ustdio.h>

#include <boost/filesystem/path.hpp>

class ounistream : public std::basic_ostream<UChar>
{
public:
  class outbuf : public std::basic_streambuf<UChar>
  {
  protected:
    UFILE * out;

  public:
    outbuf () : out(NULL) {}
    outbuf (FILE * file_handle) {
      out = u_finit(file_handle, "UTF-8", NULL);
    }
    outbuf (UFILE * ufile_handle) : out(ufile_handle) {}
    outbuf (const boost::filesystem::path& pathname) {
      out = u_fopen(pathname.string().c_str(), "r", "UTF-8", NULL);
    }
    ~outbuf() {
      u_fclose(out);
    }

  protected:
    // write one character
    virtual int_type overflow (int_type c) {
#if 0
      if (c != EOF) {
	char z = c;
	if (write (fd, &z, 1) != 1) {
	  return EOF;
	}
      }
#endif
      return c;
    }
    // write multiple characters
    virtual
    std::streamsize xsputn (const UChar* s, std::streamsize num) {
      return u_file_write(s,num,out);
    }
  };

protected:
  outbuf buf;

public:
  ounistream () : std::basic_ostream<UChar>(0), buf() {}

  ounistream (FILE * file_handle)
    : std::basic_ostream<UChar>(0), buf(file_handle) {
    rdbuf(&buf);
  }
  ounistream (UFILE * ufile_handle)
    : std::basic_ostream<UChar>(0), buf(ufile_handle) {
    rdbuf(&buf);
  }
  ounistream (const boost::filesystem::path& pathname)
    : std::basic_ostream<UChar>(0), buf(pathname) {
    rdbuf(&buf);
  }

  ounistream& operator<<(const UnicodeString& unistr) {
    this->std::basic_ostream<UChar>::operator<<(unistr.getBuffer());
    return *this;
  }
};

class ounifdstream : public ounistream
{
public:
  class fdoutbuf : public ounistream::outbuf
  {
  protected:
    FILE * handle;

  public:
    fdoutbuf (int fd) {
      handle = fdopen(fd, "w");
      out = u_finit(handle, "UTF-8", NULL);
    }
    ~fdoutbuf() {
      fclose(handle);
    }
  };

protected:
  fdoutbuf buf;

public:
  ounifdstream (int fd) : ounistream(), buf(fd) {
    rdbuf(&buf);
  }
};

class ounistrstream : public std::basic_ostream<UChar>
{
protected:
  class stroutbuf : public ounistream::outbuf
  {
  protected:
    UChar buffer[256];

  public:
    stroutbuf () {
      out = u_fstropen(buffer, 255, "UTF-8");
    }

    friend class ounistrstream;
  };

  stroutbuf buf;

public:
  ounistrstream () : std::basic_ostream<UChar>(0), buf() {
    rdbuf(&buf);
  }

  UnicodeString u_str() const {
    return buf.buffer;
  }
};

#if 0
class iunistream : public std::istream
{
  class inbuf : public std::streambuf
  {
  protected:
    int fd;    // file descriptor
  protected:
    /* data buffer:
     * - at most, pbSize characters in putback area plus
     * - at most, bufSize characters in ordinary read buffer
     */
    static const int pbSize = 4;        // size of putback area
    static const int bufSize = 1024;    // size of the data buffer
    char buffer[bufSize+pbSize];        // data buffer

  public:
    /* constructor
     * - initialize file descriptor
     * - initialize empty data buffer
     * - no putback area
     * => force underflow()
     */
    inbuf (int _fd) : fd(_fd) {
      setg (buffer+pbSize,     // beginning of putback area
	    buffer+pbSize,     // read position
	    buffer+pbSize);    // end position
    }

  protected:
    // insert new characters into the buffer
    virtual int_type underflow () {
#ifndef _MSC_VER
      using std::memmove;
#endif

      // is read position before end of buffer?
      if (gptr() < egptr()) {
	return traits_type::to_int_type(*gptr());
      }

      /* process size of putback area
       * - use number of characters read
       * - but at most size of putback area
       */
      int numPutback;
      numPutback = gptr() - eback();
      if (numPutback > pbSize) {
	numPutback = pbSize;
      }

      /* copy up to pbSize characters previously read into
       * the putback area
       */
      memmove (buffer+(pbSize-numPutback), gptr()-numPutback,
	       numPutback);

      // read at most bufSize new characters
      int num;
      num = read (fd, buffer+pbSize, bufSize);
      if (num <= 0) {
	// ERROR or EOF
	return EOF;
      }

      // reset buffer pointers
      setg (buffer+(pbSize-numPutback),   // beginning of putback area
	    buffer+pbSize,                // read position
	    buffer+pbSize+num);           // end of buffer

      // return next character
      return traits_type::to_int_type(*gptr());
    }
  };

protected:
  inbuf buf;

public:
  iunistream (int fd) : std::istream(0), buf(fd) {
    rdbuf(&buf);
  }
};
#endif

#endif // UNISTREAM_HPP
