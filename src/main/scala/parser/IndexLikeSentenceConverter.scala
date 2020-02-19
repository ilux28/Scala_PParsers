package parser

import java.io

import scala.collection.mutable
import scala.util.parsing.combinator._

/***
 * The general idea of this parser combinator it is compilation parsers from simple parsers representing regular expression,
 * they then compiling in the more difficult parsers.
 * ^^ documented as «parser combinator for function application».
 * If parsing is on the left of ^^ is success, possible function is on the right.
 * If method `anyMethod` return Parser is type String, then function is on the right of ^^ must return String.
 */

class IndexLikeSentenceConverter extends RegexParsers {


  val  patternOneBool = "and|or|not".r
  val  patternTwoBool = "and|or|!\\(".r
  /***
   * Auxiliary arrays, processing of which, added in result
   */

  var bufferIndexLog: mutable.Buffer[String] = mutable.Buffer[String]()
  var bufferOtherSentence: mutable.Buffer[String] = mutable.Buffer[String]()
  var bufferWithoutIndex: mutable.Buffer[String] = mutable.Buffer[String]()
  var bufferWithBoolSentence: mutable.Buffer[String] = mutable.Buffer[String]()

  var treeBoolSentence: mutable.TreeSet[String] = mutable.TreeSet[String]()

  //-------------------------------------------------------------------//

  /***
   * Number simple elements for parsing - this is tokens
   * This tokens for consisting more difficult expression
   */

  def `anySentenceWithoutGap`: Parser[String] = """[^"'\s]+""".r ^^ ( _.toString )

  def `anySentenceWithColWithoutGap`: Parser[String] = """[^\s]+""".r ^^ ( _.toString )

  def `anySentenceWithoutGapElse`: Parser[String] = """\S+""".r ^^ (_.toString)

  /**
   * Group for parsing various expressions:
   * 1. Some number symbols with sign compare on the end, view """\S+\s?[=<>]""".r
   * 2. Some number symbols prisoners between in double or ones quotes , view """["|'].+?["|']""".r
   * 3. Some number symbols prisoners between in double or ones quotes is a not contains they inner, view """["|'][^\\"|'].+?[^\\"|']["|']""".r
   * 4. Some number symbols begin is not quotes and end they, view """[^"|']+["|']""".r
   * 5. Some number symbols begin and end is not quotes, view """["|'][^"|'\s]+""".r
   */
  def `commonColWithCompare`: Parser[String] = """\S+\s?[=<>]""".r ^^ (_.toString)

  def `lazyQuantifierWithBrackets`: Parser[String] = """["|'].+?["|']""".r  ^^ { _.toString }

  def `lazyQuantifierInnerShieldedBrackets`: Parser[String] = """["|'][^\\"|'].+?[^\\"|']["|']""".r ^^ { _.toString }

  def `sentenceWithRightBrackets`: Parser[String] = """[^"|']+["|']""".r  ^^ {  _.toString  }

  def `sentenceWithLeftBrackets`: Parser[String] = """["|'][^"|'\s]+""".r  ^^ { _.toString }

  /**
   * For parsing expressions type "index\\s?=\\s?\\S+".r
   * and added success result in bufferIndexLog,
   * responsible for the number json and consistent they headers
   */

  def `indexUniversal`: Parser[String] = "index\\s?=\\s?\\S+".r ^^ { str => {
    parse(`index=log`, str) match {
      case Success(result, _) =>
        bufferIndexLog += result
        result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
  }}

  def `index`: Parser[String] = "index\\s?=".r //^^ (_ => "index=") //cases expression with index

  def `index=log`: Parser[String] =
    `index` ~>
      `anySentenceWithoutGapElse` ^^ (
      _.toString
      )

  /**
   * For parsing boolean expressions type "AND|OR|NOT"
   */
  def `anyBooleanSentence`: Parser[String] = "AND|OR|NOT|And|ANd|aNd|AnD|anD|Or|oR|Not|NoT|nOT|noT|nOt".r ^^ { str => {
    val strRes = if (str.toLowerCase() == "not") s"!(" else str.toUpperCase()
    strRes
  } }

  /**
   * For parsing expressions-requests type "\\*?G\\*?E\\*?T\\*?+|\\*?P\\*?O\\*?S\\*?T\\*?+"
   */

  def `anyRequests`: Parser[String] = "\\*?G\\*?E\\*?T\\*?+|\\*?P\\*?O\\*?S\\*?T\\*?+".r ^^ { str => {
    val strRequest = if (str.contains("*")) {
      val tempStr = str.replaceAll("""\*""", """.*""")
      s"_raw rlike \\'$tempStr\\'"
    } else {
      s"_raw like \\'%$str%\\'"
    }
    strRequest
  } }

  /***
   * Method for parsing all other expressions for perhaps variants type 'col1='20*'
   * @return
   */


  //TODO required refactoring 26.12.19
  def `sentenceWithCol`: Parser[String] = `commonColWithCompare`.? ~ `anyVariantsWithBrackets` ^^ {
    case colOpt ~ anySent => {
      val col = colOpt.fold("")(_.toString)
      val resStr = if (col.contains("index")) {
        parse(`index=log`, s"$col$anySent") match {
          case Success(result, _) => {
            bufferIndexLog += result
          }
          case failure: NoSuccess => scala.sys.error(failure.msg)
        }
        ""
      } else {
        val  res = if (anySent.contains("""\*""")) {
          s"$col\'${anySent.replaceAll("""\\\*""", """*""").replaceAll(""""""", """""")}\'"
        } else if (anySent.contains("""*""")) {
          s"${col.substring(0, col.length - 1)} rlike \\'${anySent.replaceAll("""\*""", """.*""").replaceAll(""""""", """""")}\\'"
        } else {
          if (col == "" && !anySent.contains("%")) {
            val anySentTemp = anySent.replaceAll("""\\""", """""")
            s"""_raw like \'%${anySentTemp.toString().substring(1, anySentTemp.length - 1)}%\'"""
          } else if (col == "" && anySent.contains("%")) {
            s"""_raw rlike \'${anySent.substring(1, anySent.length - 1)}\'"""
          } else {
            "%s%s".format(col, anySent.replaceAll("([\"'])", """\\'"""))
          }
          /*
          if (col == "" && !anySent.contains("%")) {
            s"_raw like \\'%${anySent.substring(1, anySent.length - 1)}%\\'"
          } else if (col == "" && anySent.contains("%")) {
            s"_raw rlike \\'${anySent.substring(1, anySent.length - 1)}\\'"
          } else {
            "%s%s".format(col, anySent.replaceAll("([\"'])", """\\'"""))
          }
           */
        }
        bufferWithBoolSentence += res
        res
      }
      resStr
    }
  }

  /***
   * Methods for parsing expressions "high level"
   * (Pay attention on priority - order make difference: begin check on accordance which first, else accordance
   * not find, checked next)
   */

  def `anyVariantsWithBrackets`: Parser[String] =
    `lazyQuantifierInnerShieldedBrackets` |
      `lazyQuantifierWithBrackets` |
      `sentenceWithRightBrackets` |
      `sentenceWithLeftBrackets` |
      `anySentenceWithColWithoutGap`

  def `commonSentenceWithBrackets`: Parser[String] =
    `anyBooleanSentence` |
      `anyRequests` |
      `sentenceWithCol`|
      `anySentenceWithoutGap`

  def `basicBodySentence`: Parser[List[String]] = rep1(`commonSentenceWithBrackets`) ^^ {
    innerListSentence => {
      bufferOtherSentence.insertAll(0, innerListSentence)
      innerListSentence
    }
  }

  def `universalSentence`: Parser[io.Serializable] =
    `indexUniversal` |
      `basicBodySentence`

  /** *
   * Method replaced result buffer @param = bufferOtherSentence in compliance with notBrackets policy
   */
  def notFilterConverter() = {
    var bufferRemoveElements: mutable.Buffer[Int] = mutable.Buffer[Int]()
    def strIsExistCurrentBoolExpr (x: Int) = patternOneBool.findFirstIn(bufferOtherSentence(x).toLowerCase).getOrElse("")
    def strIsExistPreviewBoolExpr(x: Int) = patternTwoBool.findFirstIn(bufferOtherSentence(x - 1).toLowerCase).getOrElse("")
    for (x <- bufferOtherSentence.indices) {
      if (bufferOtherSentence(x) == "!(") {
        if (bufferOtherSentence(x + 1).contains('(')) {
          bufferOtherSentence(x + 1) = s"!${bufferOtherSentence(x + 1)}"
        } else {
          bufferOtherSentence(x + 1) = s"!(${bufferOtherSentence(x + 1)})"
        }
        bufferRemoveElements += x
      } else if (strIsExistCurrentBoolExpr(x) == "" && x != 0 && bufferOtherSentence(x) != "") {
        if (strIsExistPreviewBoolExpr(x) == "") {
          bufferOtherSentence(x) = s"AND ${bufferOtherSentence(x)}"
        }
      }
    }
    bufferRemoveElements.reverse.foreach(elementNumber => bufferOtherSentence.remove(elementNumber))

  }

  /** *
   * Method replaced result buffer @param = bufferOtherSentence in compliance with andBrackets policy
   * @return countBracketsString consistent some endBrackets " ) "
   */

  def andFilterConverter(): mutable.Seq[Char] = {
    val countBracketsString: mutable.StringBuilder = new mutable.StringBuilder("")
    for (x <- bufferOtherSentence.indices) {
      if (bufferOtherSentence(x) == "AND" && x > 0) {
        bufferOtherSentence(x + 1) = s"(${bufferOtherSentence(x + 1)}"
        countBracketsString ++= ")"
      } else if(bufferOtherSentence(x).contains("AND")) {
        bufferOtherSentence(x) = bufferOtherSentence(x).replace("AND ", "AND (")
        countBracketsString ++= ")"
      }
    }
    countBracketsString
  }

   /** *
   * Method returning the result based on the prepared data of the auxiliary array
   */

  //TODO May require refactoring 26.12.19

  def commonSentenceWorkerVariant: Parser[IndexApacheLog] = rep1(`universalSentence`) ^^ { _ =>
    bufferWithoutIndex = bufferOtherSentence.filter(x => {
      x.contains(">") || x.contains("<") || x.contains("=")
    }).map(str => s"""["${str.split("\\W")(0)}"]""")

    notFilterConverter()
    bufferOtherSentence = bufferOtherSentence.filterNot(x => x == "")
    //val  countBracketsString = andFilterConverter()
    //bufferOtherSentence(bufferOtherSentence.length - 1) = s"""${bufferOtherSentence.last}${countBracketsString.toString()}"""
    //println(bufferOtherSentence.mkString(" "))
    IndexApacheLog(bufferIndexLog, bufferOtherSentence.mkString(" "))
  }


  /***
   * Case class in method @toString forming resulting to JSON
   */

  case class IndexApacheLog(bufferIndexLogs: mutable.Buffer[String], groupCol: String) {
    val apBuffer: mutable.Buffer[String] = bufferIndexLogs.map(_.stripPrefix("\"").stripSuffix("\""))
    override def toString: String = {
      if (apBuffer.isEmpty) {
        s"""{"query":"$groupCol","fields": ${bufferWithoutIndex.mkString(", ")}}"""
      } else {
      apBuffer.map(ap => {
        s"""{"$ap":{"query": "$groupCol", "tws": 0, "twf": 0}}"""
      }).mkString(",")
      }
    }
  }

  /**
  * This method a parsed str to res, if it is equal => return Success
  * @return
  */

  def parseResult(resIndex: Parser[IndexApacheLog], str: String): String = {
    parse(resIndex, str) match {
      case Success(matched, _) => matched.toString
      case Failure(msg, _) => s"FAILURE  $msg"
      case Error(msg, _) => s"Error $msg"
    }
  }
}

object IndexLikeSentenceConverter {
  def getParser(str: String): String = {
    val  createParserInstance = new IndexLikeSentenceConverter
    createParserInstance.parseResult(createParserInstance.commonSentenceWorkerVariant, str)
  }
}

object SimpleParser {
  def main(args: Array[String]): Unit = {
    val str = s"""index=test (text=\"RUB\" text{2}.val!=null) OR 1=1""""
    val originalSpl = s"""index=test (text=\"RUB\" text{2}.val!=null) OR 1=1""""
    //{"test":{"query": "((text=\"RUB\") AND ('text{2}.val'!=\"null\") OR (1=1))", "tws": 0, "twf": 0}}
    //val str = s"""index=test NOT col1=20"""
    //println("""{"test":{"query": "((text=\"RUB\") AND ('text{2}.val'!=\"null\") OR (1=1))", "tws": 0, "twf": 0}}""")
    println(IndexLikeSentenceConverter.getParser(str))
  }
}