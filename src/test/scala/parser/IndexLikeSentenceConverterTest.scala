package parser

import org.scalatest.FunSuite

class IndexLikeSentenceConverterTest extends FunSuite {

  test("Check methods for SimpleParser class for case test 1 for parsing entry string " +
    "\"index=test Колонка=Test\"") {
    val str = "index=test Колонка=Test"
    val compareStr = s"""{"test":{"query": "Колонка=Test", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 2 for parsing entry string " +
    "\"index=test\"") {
    val str = "index=test"
    val compareStr = s"""{"test":{"query": "", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 3 for parsing string " +
    "\"index=\\\"test\\\"\"") {
    val str = "index=\"test\""
    val compareStr = s"""{"test":{"query": "", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 4 for parsing string " +
    "\"index= test\"") {
    val str = "index= test"
    val compareStr = s"""{"test":{"query": "", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 5 for parsing string " +
    "\"index=Тестовый_Индекс\"") {
    val str = "index=Тестовый_Индекс"
    val compareStr = s"""{"Тестовый_Индекс":{"query": "", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 6 for parsing string " +
    "\"index=test col1=20\"") {
    val str = "index=test col1=20"
    val compareStr = s"""{"test":{"query": "col1=20", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 7 for parsing string " +
    "\"index=test NOT col1=20\"") {
    val str = "index=test NOT col1=20"
    val compareStr = s"""{"test":{"query": "!(col1=20)", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 8 for parsing string " +
    "\"index=test col1=20 OR col2>30\"") {
    val str = "index=test col1=20 OR col2>30"
    val compareStr = s"""{"test":{"query": "col1=20 OR col2>30", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 9 for parsing string " +
    "\"index=test (col1=20 AND col3=40) OR col2=30\"") {
    val str = "index=test (col1=20 AND col3=40) OR col2=30"
    val compareStr = s"""{"test":{"query": "(col1=20 AND col3=40) OR col2=30", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 10 for parsing string " +
    "\"index=test col1=\\\"20\\\"\"") {
    val str = "index=test col1=\"20\""
    val compareStr = s"""{"test":{"query": "col1=\\"20\\"", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 11 for parsing string " +
    "\"index=test col1=\\\"test value\\\"\"") {
    val str = "index=test col1=\"test value\""
    val compareStr = s"""{"test":{"query": "col1=\\"test value\\"", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 12 for parsing string " +
    "\"index=test GET\"") {
    val str = "index=test GET"
    val compareStr = s"""{"test":{"query": "_raw like \\'%GET%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 13 for parsing string " +
    "\"index=test GET POST\"") {
    val str = "index=test GET POST"
    val compareStr = s"""{"test":{"query": "_raw like \\'%GET%\\' AND _raw like \\'%POST%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 14 for parsing string " +
    "\"index=test GET AND POST\"") {
    val str = "index=test GET AND POST"
    val compareStr = s"""{"test":{"query": "_raw like \\'%GET%\\' AND _raw like \\'%POST%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 15 for parsing string " +
    "\"index=test col1=20 GET\"") {
    val str = "index=test col1=20 GET"
    val compareStr = s"""{"test":{"query": "col1=20 AND _raw like \\'%GET%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 16 for parsing string " +
    "\"index=test col1=20 AND GET\"") {
    val str = "index=test col1=20 AND GET"
    val compareStr = s"""{"test":{"query": "col1=20 AND _raw like \\'%GET%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 17 for parsing string " +
    "\"index=test col1=20 OR GET OR POST\"") {
    val str = "index=test col1=20 OR GET OR POST"
    val compareStr = s"""{"test":{"query": "col1=20 OR _raw like \\'%GET%\\' OR _raw like \\'%POST%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 18 for parsing string " +
    "\"index=test \\\"русские trololo;? символы\\\"\"") {
    val str = "index=test \"русские trololo;? символы\""
    val compareStr = s"""{"test":{"query": "_raw like \'%русские trololo;? символы%\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 19 for parsing string " +
    "\"index=test \\\"русские символы\\\" OR \\\"слово\\\"  POST\"") {
    val str = "index=test \"русские символы\" OR \"слово\"  POST"
    val compareStr = s"""{"test":{"query": "_raw like \'%русские символы%\' OR _raw like \'%слово%\' AND _raw like \\'%POST%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }
  test("Check methods for SimpleParser class for case test 20 for parsing string " +
    "\"index=test \\\"русские \\\\\\\"символы\\\\\\\" host\\\" OR \\\"слово\\\"  POST\"") {
    val str = "index=test \"русские \\\"символы\\\" host\" OR \"слово\" POST"
    val compareStr = s"""{"test":{"query": "_raw like \'%русские "символы" host%\' OR _raw like \'%слово%\' AND _raw like \\'%POST%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 21 for parsing string " +
    "\"index=test GET* AND POST*\"") {
    val str = "index=test GET* AND POST*"
    val compareStr = s"""{"test":{"query": "_raw rlike \\'GET.*\\' AND _raw rlike \\'POST.*\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 22 for parsing string " +
    "\"index=test *G*ET* AND PO*ST*\"") {
    val str = "index=test *G*ET* AND PO*ST*"
    val compareStr = s"""{"test":{"query": "_raw rlike \\'.*G.*ET.*\\' AND _raw rlike \\'PO.*ST.*\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 23 for parsing string " +
    "\"index=test col1=20* AND GET\"") {
    val str = "index=test col1=20* AND GET"
    val compareStr = s"""{"test":{"query": "col1 rlike \\'20.*\\' AND _raw like \\'%GET%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 24 for parsing string " +
    "\"index=test col1=\\\"20*\\\" AND GET\"") {
    val str = "index=test col1=\"20*\" AND GET"
    val compareStr = s"""{"test":{"query": "col1 rlike \\'20.*\\' AND _raw like \\'%GET%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 25 for parsing string " +
    "\"index=test col1=20 OR GET OR POST OR col2=20 OR col3=20 OR col4=20 OR col5=20 OR col6=20 OR col7=20\"") {
    val str = "index=test col1=20 OR GET OR POST OR col2=20 OR col3=20 OR col4=20 OR col5=20 OR col6=20 OR col7=20"
    val compareStr = s"""{"test":{"query": "col1=20 OR _raw like \\'%GET%\\' OR _raw like \\'%POST%\\' OR col2=20 OR col3=20 OR col4=20 OR col5=20 OR col6=20 OR col7=20", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 26 for parsing string" +
    " \"index=test col1=\\\"20\\\\*\\\" AND GET\"") {
    val str = "index=test col1=\"20\\*\" AND GET"
    val compareStr = s"""{"test":{"query": "col1=\'20*\' AND _raw like \\'%GET%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 27 for parsing string " +
    "\"GET AND POST index=test\"") {
    val str = "GET AND POST index=test"
    val compareStr = s"""{"test":{"query": "_raw like \\'%GET%\\' AND _raw like \\'%POST%\\'", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 28 for parsing string \"index=test index=test2 col1=20\"") {
    val str = "index=test index=test2 col1=20"
    val compareStr = s"""{"test":{"query": "col1=20", "tws": 0, "twf": 0}},{"test2":{"query": "col1=20", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 29 for parsing string " +
    "index=indexcsv NOT \"%cpu%\" OR NOT \"%sss%\" AND NOT \"%ddd%\"") {
    val str = "index=indexcsv NOT \"%cpu%\" OR NOT \"%sss%\" AND NOT \"%ddd%\""
    val compareStr = s"""{"indexcsv":{"query": "!(_raw rlike '%cpu%') OR !(_raw rlike '%sss%') AND !(_raw rlike '%ddd%')", "tws": 0, "twf": 0}}"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }

  test("Check methods for SimpleParser class for case test 30 for parsing string " +
    "NOT \"%cpu%\" OR NOT \"%sss%\" AND NOT \"%ddd%\"") {
    val str = "NOT \"%cpu%\" OR NOT \"%sss%\" AND NOT \"%ddd%\""
    val compareStr = s"""{"query": "!(_raw rlike '%cpu%') OR !(_raw rlike '%sss%') AND !(_raw rlike '%ddd%')","fields": }"""
    val testStr = IndexLikeSentenceConverter.getParser(str)
    assert(testStr == compareStr)
  }
}
