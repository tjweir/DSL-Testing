package com.tyler

import scala.util.parsing.combinator.syntactical._

object OrderDsl extends StandardTokenParsers { 
  lexical.reserved += ("to", "buy", "sell", "min", "max", "for", "account", "shares", "at") 
  lexical.delimiters += ("(", ")", ",")	

  lazy val order = items ~ account_spec  
  lazy val items = "(" ~> rep1sep(line_item, ",") <~ ")"
  lazy val line_item = security_spec ~ buy_sell ~ price_spec
  lazy val buy_sell = "to" ~> "buy" | "to" ~> "sell" 
  lazy val security_spec = numericLit ~ (ident <~ "shares")
  lazy val price_spec = "at" ~> (min_max?) ~ numericLit
  lazy val min_max = "min" | "max"
  lazy val account_spec = "for" ~> "account" ~> stringLit
}


object DSL {
  import OrderDsl._
  
  val str = """(100 IBM shares to buy at max 45, 40 Sun shares to sell at min 24, 25 CISCO shares to buy at max 56) for account "A1234""""
  
  def main(args: Array[String]) {
    order(new lexical.Scanner(str)) match { 
      case Success(order, _) => println("Success: " + order)	
      case Failure(msg, _) => println("Failure: " + msg)
      case Error(msg, _) => println("Error: " + msg)
    }
  }
}
