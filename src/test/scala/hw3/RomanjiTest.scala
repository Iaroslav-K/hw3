package hw3

import hw3.Main.romanji
import org.scalatest.{FunSuite, Matchers}

class RomanjiTest extends FunSuite with Matchers {

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
    the[IllegalArgumentException] thrownBy {
      romanji("ノヤ")
    } should have message "requirement failed: 'i' symbol missing"
  }

  test("string with ッ") {
    romanji("サッカ") shouldBe "sakka"
  }

  test("wrong string with ッ") {
    the[IllegalArgumentException] thrownBy {
      romanji("サッ")
    } should have message "requirement failed: symbol \'ー\' can't doubles nothing"
  }

  test("wrong string with ー") {
    the[IllegalArgumentException] thrownBy {
      romanji("ール")
    } should have message "requirement failed: symbol \'ー\' can't lengthen nothing"
  }

  test("string with ン") {
    romanji("ナンノ") shouldBe "nanno"
  }

  test("wrong string with ン") {
    the[IllegalArgumentException] thrownBy {
      romanji("ナンサ")
    } should have message "requirement failed: symbol ン doubles the following consonant " +
      "only in the case of na, ni, nu, ne, no syllables"
  }

  test("Empty text") {
    romanji("") shouldBe ""
  }

  test("String with other symbols") {
    romanji("アイスクリーム!") shouldBe "aisukurīmu!"
  }

  test("illegal symbol") {
    the[IllegalArgumentException] thrownBy {
      romanji("アイスクリームa")
    } should have message "Symbol a is not supported"
  }

  test("big data test") {
    val katakana = new StringBuilder()
    val distinctTranslation = new StringBuilder()
    for (_ <- 0 to 200) {
      katakana.append("トイレ ")
      distinctTranslation.append("toire ")
      katakana.append("テレビ ")
      distinctTranslation.append("terebi ")
      katakana.append("ドラマ ")
      distinctTranslation.append("dorama ")
      katakana.append("アイスクリーム ")
      distinctTranslation.append("aisukurīmu ")
      katakana.append("ノック ")
      distinctTranslation.append("nokku ")
    }
    romanji(katakana.toString()) shouldBe distinctTranslation.toString()
  }

}
