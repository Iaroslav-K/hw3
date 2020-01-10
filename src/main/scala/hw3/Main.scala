package hw3

import math._


object Main {

  implicit class MyDouble(x: Double) {
    def ^(b: Double): scala.Double = pow(x, b)
  }

  /** The method uses: `(1/n)*(sum(1)(n)(Xi - Xn))^2`
   *
   * @param vector list of numbers
   * @return standard deviation of that vector
   */
  def standardDeviation(vector: List[Double]): Double = {
    require(vector.nonEmpty, "empty list")
    val Xn = vector.sum / vector.size
    (vector.foldLeft(0.0)(
      (acc, Xi) => {
        acc + ((Xi - Xn) ^ 2.0)
      }
    ) / vector.size) ^ 0.5
  }

  def letterFrequencyRanking(corpus: String): String = ???

  def romanji(katakana: String): String = ???

  def gray(bits: Int): List[String] = ???
}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'), 'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k', 'a'), 'キ' -> List('k', 'i'), 'ク' -> List('k', 'u'), 'ケ' -> List('k', 'e'), 'コ' -> List('k', 'o'),
    'ガ' -> List('g', 'a'), 'ギ' -> List('g', 'i'), 'グ' -> List('g', 'u'), 'ゲ' -> List('g', 'e'), 'ゴ' -> List('g', 'o'),
    'サ' -> List('s', 'a'), 'シ' -> List('s', 'i'), 'ス' -> List('s', 'u'), 'セ' -> List('s', 'e'), 'ソ' -> List('s', 'o'),
    'ザ' -> List('z', 'a'), 'ジ' -> List('z', 'i'), 'ズ' -> List('z', 'u'), 'ゼ' -> List('z', 'e'), 'ゾ' -> List('z', 'o'),
    'タ' -> List('t', 'a'), 'チ' -> List('t', 'i'), 'ツ' -> List('t', 'u'), 'テ' -> List('t', 'e'), 'ト' -> List('t', 'o'),
    'ダ' -> List('d', 'a'), 'ヂ' -> List('d', 'i'), 'ヅ' -> List('d', 'u'), 'デ' -> List('d', 'e'), 'ド' -> List('d', 'o'),
    'ナ' -> List('n', 'a'), 'ニ' -> List('n', 'i'), 'ヌ' -> List('n', 'u'), 'ネ' -> List('n', 'e'), 'ノ' -> List('n', 'o'),
    'ハ' -> List('h', 'a'), 'ヒ' -> List('h', 'i'), 'フ' -> List('h', 'u'), 'ヘ' -> List('h', 'e'), 'ホ' -> List('h', 'o'),
    'バ' -> List('b', 'a'), 'ビ' -> List('b', 'i'), 'ブ' -> List('b', 'u'), 'ベ' -> List('b', 'e'), 'ボ' -> List('b', 'o'),
    'パ' -> List('p', 'a'), 'ピ' -> List('p', 'i'), 'プ' -> List('p', 'u'), 'ペ' -> List('p', 'e'), 'ポ' -> List('p', 'o'),
    'マ' -> List('m', 'a'), 'ミ' -> List('m', 'i'), 'ム' -> List('m', 'u'), 'メ' -> List('m', 'e'), 'モ' -> List('m', 'o'),
    'ヤ' -> List('y', 'a'), 'ユ' -> List('y', 'u'), 'ヨ' -> List('y', 'o'),
    'ラ' -> List('r', 'a'), 'リ' -> List('r', 'i'), 'ル' -> List('r', 'u'), 'レ' -> List('r', 'e'), 'ロ' -> List('r', 'o'),
    'ワ' -> List('w', 'a'), 'ヰ' -> List('w', 'i'), 'ヱ' -> List('w', 'e'), 'ヲ' -> List('w', 'o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
}