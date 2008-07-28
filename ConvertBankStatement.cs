using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

using CSVReader;

namespace JohnWiegley
{
  public class Transaction
  {
    public DateTime Date;
    public DateTime PostedDate;
    public string  Code;
    public string  Payee;
    public Decimal  Amount;
  }

  public interface IStatementConverter
  {
    List<Transaction> ConvertRecords(Stream s);
  }

  public class ConvertNrlGoldMasterCardStatement : IStatementConverter
  {
    public List<Transaction> ConvertRecords(Stream s) {
      List<Transaction> xacts = new List<Transaction>();

      using (CSVReader.CSVReader csv = new CSVReader.CSVReader(s)) {
        string[] fields;
        while ((fields = csv.GetCSVLine()) != null) {
          if (fields[0] == "TRANSACTION DATE")
            continue;
          
          Transaction xact = new Transaction();

		  try {
			  xact.Date		  = DateTime.ParseExact(fields[0], "mm/dd/yy", null);
			  xact.PostedDate = DateTime.ParseExact(fields[1], "mm/dd/yy", null);
			  xact.Payee	  = fields[2].Trim();
			  xact.Code		  = fields[3].Trim();
			  xact.Amount	  = Convert.ToDecimal(fields[4].Trim());

			  if (xact.Code.Length == 0)
				  xact.Code = null;

			  xacts.Add(xact);
		  }
		  catch (System.FormatException) {}
        }
      }
      return xacts;
    }
  }
  
  public class ConvertUsaaMastercardStatement : IStatementConverter
  {
    public List<Transaction> ConvertRecords(Stream s) {
      List<Transaction> xacts = new List<Transaction>();

      using (CSVReader.CSVReader csv = new CSVReader.CSVReader(s)) {
        string[] fields;
        while ((fields = csv.GetCSVLine()) != null) {
          Transaction xact = new Transaction();

          xact.Date   = DateTime.ParseExact(fields[0], "m/dd/yyyy", null);
          xact.Payee  = fields[2].Trim();
          xact.Code  = fields[3].Trim();
          xact.Amount  = - Convert.ToDecimal(fields[4].Trim());

          if (xact.Code.Length == 0)
            xact.Code = null;

          xacts.Add(xact);
        }
      }
      return xacts;
    }
  }

  public class AccountMatch
  {
    public Regex  Expr;
    public string Account;
  }

  public class PrintTransactions
  {
    List<AccountMatch> accountExprs = new List<AccountMatch>();
    
	public PrintTransactions() {}
    public PrintTransactions(string regexPath)
    {
      return;
      using (StreamReader reader = new StreamReader(regexPath)) {
        String line = reader.ReadLine();
        while (line != null) {
          int firstSpace = line.IndexOf(' ');
          if (firstSpace > 0) {
            AccountMatch matcher = new AccountMatch();
            matcher.Expr    = new Regex(line.Substring(0, firstSpace));
            matcher.Account = line.Substring(firstSpace + 1);
            accountExprs.Add(matcher);
            Console.WriteLine(String.Format("Found matcher: {0}, {1}",
                            matcher.Expr, matcher.Account));
          }
          line = reader.ReadLine();
        }
      }
    }

    public string IdentifyAccount(Transaction xact) {
      return "Expenses:Food";
    }
    
    public void Print(string AccountName, string PayAccountName,
              List<Transaction> xacts)
    {
      foreach (Transaction xact in xacts) {
        if (xact.Amount < 0) {
          Console.WriteLine("{0} * {1}{2}", xact.Date.ToString("yyyy/mm/dd"),
                    xact.Code != null ? "(" + xact.Code + ") " : "",
                    xact.Payee);
          Console.WriteLine("    {0,-36}{1,12}", AccountName,
                    "$" + (- xact.Amount).ToString());
          Console.WriteLine("    {0}", PayAccountName);
        } else {
          Console.WriteLine("{0} {1}{2}", xact.Date.ToString("yyyy/mm/dd"),
                    xact.Code != null ? "(" + xact.Code + ") " : "",
                    xact.Payee);
          Console.WriteLine("    {0,-36}{1,12}", IdentifyAccount(xact),
                    "$" + xact.Amount.ToString());
          Console.WriteLine("    * {0}", AccountName);
        }
        Console.WriteLine();
      }
    }
  }

  public class ConvertBankStatement
  {
    public static int Main(string[] args) {
      StreamReader reader = new StreamReader(args[0]);
      string firstLine = reader.ReadLine();

      string CardAccount, BankAccount;

      IStatementConverter converter;
      if (firstLine.StartsWith("TRANSACTION DATE")) {
        converter = new ConvertNrlGoldMasterCardStatement();

        CardAccount = "Liabilities:NRL:MasterCard";
        BankAccount = "Assets:NRL:Checking";
      } else {
        converter = new ConvertUsaaMastercardStatement();

        CardAccount = "Liabilities:USAA:MasterCard";
        BankAccount = "Assets:E*Trade:Bank";
      }

      reader = new StreamReader(args[0]);
      List<Transaction> xacts = converter.ConvertRecords(reader.BaseStream);

      //PrintTransactions printer = new PrintTransactions(args[1]);
      PrintTransactions printer = new PrintTransactions();
      printer.Print(CardAccount, BankAccount, xacts);

      return 0;
    }
  }
}
