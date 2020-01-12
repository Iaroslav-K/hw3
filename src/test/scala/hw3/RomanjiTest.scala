package hw3

import hw3.Main.romanji
import org.scalatest.{FunSuite, Matchers}

class RomanjiTest extends FunSuite with Matchers {
  val katakama = new StringBuilder()
  val distinctTranslation = new StringBuilder()
  for (_ <- 0 to 200) {
    katakama.append("トイレ ")
    distinctTranslation.append("toire ")
    katakama.append("テレビ ")
    distinctTranslation.append("terebi ")
    katakama.append("ドラマ ")
    distinctTranslation.append("dorama ")
    katakama.append("アイスクリーム ")
    distinctTranslation.append("aisukurīmu ")
    katakama.append("ノック ")
    distinctTranslation.append("nokku ")
  }

  test("Toilet") {
    romanji("トイレ") shouldBe "toire"
  }

  test("Television") {
    romanji("テレビ") shouldBe "terebi"
  }

  test("Drama") {
    romanji("ドラマ") shouldBe "dorama"
  }

  test("Ice-cream") {
    romanji("アイスクリーム") shouldBe "aisukurīmu"
  }

  test("Knock") {
    romanji("ノック") shouldBe "nokku"
  }

  test("string with ャ/ュ/ョ") {
    romanji("キヤ") shouldBe "kya"
  }

  test("wrong string with ャ/ュ/ョ") {
    intercept[IllegalArgumentException] {
      romanji("ノヤ")
    }.getMessage shouldBe "requirement failed: 'i' symbol missing"
  }

  test("string with ッ") {
    romanji("サッカ") shouldBe "sakka"
  }

  test("wrong string with ッ") {
    intercept[IllegalArgumentException] {
      romanji("サッ")
    }.getMessage shouldBe "requirement failed: symbol \'ー\' can't doubles nothing"
  }

  test("wrong string with \'ー\'") {
    intercept[IllegalArgumentException] {
      romanji("ール")
    }.getMessage shouldBe "requirement failed: symbol \'ー\' can't lengthen nothing"
  }

  test("string with ン") {
    romanji("ナンノ") shouldBe "nanno"
  }

  test("wrong string with ン") {
    intercept[IllegalArgumentException] {
      romanji("ナンサ")
    }.getMessage shouldBe "requirement failed: symbol ン doubles the following consonant " +
      "only in the case of na, ni, nu, ne, no syllables"
  }

  test("big data test") {
    romanji(katakama.toString()) shouldBe distinctTranslation.toString()
  }


}