package edu.binghamton.cs571;

public class UglyRegexpParser {

  Token _lookahead;
  Scanner _scanner;

  UglyRegexpParser(Scanner scanner) {
    _scanner = scanner;
    _lookahead = _scanner.nextToken();
  }


  /** parse a sequence of lines containing ugly-regexp's; for each
   *  ugly regexp print out the corresponding standard regexp.
   *  If there is an error, print diagnostic and continue with
   *  next line.
   */
  public void parse() {
    while (_lookahead.kind != Token.Kind.EOF) {
      try {
        String out = uglyRegexp();
        if (check(Token.Kind.NL)) System.out.println(out);
        match(Token.Kind.NL);
      }
      catch (ParseException e) {
        System.err.println(e.getMessage());
        while (_lookahead.kind != Token.Kind.NL) {
          _lookahead = _scanner.nextToken();
        }
        _lookahead = _scanner.nextToken();
      }
    }
  }

  /** Return standard syntax regexp corresponding to ugly-regexp
   *  read from _scanner.
   */
  //IMPLEMENT THIS FUNCTION and any necessary functions it may call.
 private String uglyRegexp() {
      String s = start();
      return  s; 
      //placeholder for compile
  }
  
  private String start(){
      String s1 = exp1();
      String s2 = startRest();
      if(s1.contains("[") && (s2.contains("]")))
          return "(" + s1 + s2 +")";
      else return s1 + s2;
  }
  
  private String startRest(){
      if(check(Token.Kind.CHAR, ".")){
          match(Token.Kind.CHAR, _lookahead.lexeme);
          String s1 = exp1();
          String s2 = startRest(); 
          return  s1 + s2;
      } else {
          return "";
      }
  }
  
  private String exp1(){
      String e1 = exp2();
      String e2 = exp1Rest();
      if(e1.contains("[") && e2.contains("]"))
          return "(" + e1 + e2 + ")";
      else 
          return e1 + e2;
  }
  
  private String exp1Rest(){
      if(check(Token.Kind.CHAR, "+")){
          match(Token.Kind.CHAR, _lookahead.lexeme);
          String s1 = exp2();
          String s2 = exp1Rest();
          if (s1.startsWith("([") && s1.endsWith("])"))
              return s1 + s2;
          else if(s1.endsWith("*"))
              return "(" + s1 + s2 + ")";
          else
              return "|" + s1 + s2;
      } else {
          return "";    
      }                
  }
  
  private String exp2(){
      String e = "";
      if(check(Token.Kind.CHAR, "*")){
          match(Token.Kind.CHAR, "*");
          e = exp2();
          e = e + "*";
      } 
      else {
          e = term();
      }
      return e;
  }
  
   private String term(){
       String t = "";
      if(check(Token.Kind.CHAR, "(")){
          match(Token.Kind.CHAR, _lookahead.lexeme);
          t = "(" + start(); 
          if(check(Token.Kind.CHAR, ")")){
            match(Token.Kind.CHAR, _lookahead.lexeme);
            t = t + ")";
          }
          else {
          match(Token.Kind.CHAR, ")");
          }
      }
      else if(check(Token.Kind.CHARS, "chars")){
        match(Token.Kind.CHARS, _lookahead.lexeme);
        if(check(Token.Kind.CHAR, "(")){
            match(Token.Kind.CHAR, _lookahead.lexeme);
            t = factor();  
            if(check(Token.Kind.CHAR, ")")){
                match(Token.Kind.CHAR, _lookahead.lexeme);  
            } else 
                if(check(Token.Kind.CHAR, ")"))
                match(Token.Kind.CHAR, _lookahead.lexeme);
        }  
      } 
      return t;
    }
  
  private String factor(){
      String f1 = ascii();
      String f2 = factorRest();
      return "[" + f1 + f2 + "]";
      
  }
  
  private String factorRest(){
      if(check(Token.Kind.CHAR, ",")){
          match(Token.Kind.CHAR, _lookahead.lexeme);
          String s = ascii();
          String s2 = factorRest();
          return s + s2;
      } else {
          return "";
      }
  }
  
  private String ascii(){
      String str = _lookahead.lexeme;
      match(Token.Kind.CHAR, _lookahead.lexeme);
      return quote(str) ;
  }  

    /** Return s with first char escaped using a '\' if it is
   * non-alphanumeric.
   */
  private static String quote(String s) {
    return (Character.isLetterOrDigit(s.charAt(0))) ? s : "\\" + s;
  }

  /** Return true iff _lookahead.kind is equal to kind. */
  private boolean check(Token.Kind kind) {
    return check(kind, null);
  }

  /** Return true iff lookahead kind and lexeme are equal to
   *  corresponding args.  Note that if lexeme is null, then it is not
   *  used in the match.
   */
  private boolean check(Token.Kind kind, String lexeme) {
    return (_lookahead.kind == kind &&
            (lexeme == null || _lookahead.lexeme.equals(lexeme)));
  }

  /** If lookahead kind is equal to kind, then set lookahead to next
   *  token; else throw a ParseException.
   */
  private void match(Token.Kind kind) {
    match(kind, null);
  }

  /** If lookahead kind and lexeme are not equal to corresponding
   *  args, then set lookahead to next token; else throw a
   *  ParseException.  Note that if lexeme is null, then it is
   *  not used in the match.
   */
  private void match(Token.Kind kind, String lexeme) {
    if (check(kind, lexeme)) {
      _lookahead = _scanner.nextToken();
    }
    else {
      String expected = (lexeme == null) ? kind.toString() : lexeme;
      String message = String.format("%s: syntax error at '%s', expecting '%s'",
                                     _lookahead.coords, _lookahead.lexeme,
                                     expected);
      throw new ParseException(message);
    }
  }

  private static class ParseException extends RuntimeException {
    ParseException(String message) {
      super(message);
    }
  }


  /** main program: parses and translates ugly-regexp's contained in
   *  the file specified by it's single command-line argument.
   */
  public static void main(String[] args) {
    if (args.length != 1) {
      System.err.format("usage: java %s FILENAME\n",
                        UglyRegexpParser.class.getName());
      System.exit(1);
    }
    Scanner scanner =
      ("-".equals(args[0])) ? new Scanner() : new Scanner(args[0]);
    (new UglyRegexpParser(scanner)).parse();
  }


}
