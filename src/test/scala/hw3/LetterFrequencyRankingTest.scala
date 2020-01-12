package hw3

import hw3.Main.letterFrequencyRanking
import org.scalatest.{FunSuite, Matchers}

class LetterFrequencyRankingTest extends FunSuite with Matchers {

  test("Simple") {
    letterFrequencyRanking("hello") shouldBe "leho"
  }

  test("Hard") {
    letterFrequencyRanking("Sic semper tyranis.") shouldBe "seiracmnpty"
  }

  test("Capital letters") {
    letterFrequencyRanking("AaaAaaAaa") shouldBe "a"
  }

  test("Punctuation") {
    letterFrequencyRanking("Sic!") shouldBe "cis"
  }

  test("Empty") {
    letterFrequencyRanking("") shouldBe ""
  }

  test("String with no letters") {
    letterFrequencyRanking("-1 3 456:\"+- + ") shouldBe ""
  }

}