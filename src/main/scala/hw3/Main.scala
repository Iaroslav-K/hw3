package hw3

import math._


object Main {

  /** The container for symbol frequency.
   *
   * @param container immutable map of Char -> Int
   */
  private class LetterContainer(container: Map[Char, Int]) {

    /** The leftmost letter in the ranking is the letter that occurs the most in the corpus.
     * The rightmost letter in the ranking is the letter that occurs the least in the corpus.
     * If two letters occur the same number of times, they are arranged alphabetically.
     *
     * @return sorted list of letters
     */
    private def sort: Seq[(Char, Int)] = {
      container
        .toList
        .sortWith((x, y) => if (x._2 == y._2) x._1 < y._1 else x._2 > y._2)
    }

    /** Convert sorted sequence to string.
     *
     * @return string of letter by frequency
     */
    override def toString: String = {
      sort.foldLeft("")((acc, x) => acc + x._1.toChar)
    }

    /** Increase letter frequency.
     *
     * @param char next letter
     * @return edited LetterContainer
     */
    def add(char: Char): LetterContainer = {
      if (container.contains(char)) {
        new LetterContainer((container - char) + (char -> (container(char) + 1)))
      } else {
        new LetterContainer(container + (char -> 0))
      }
    }

  }


  private implicit class MyDouble(x: Double) {
    def ^(b: Double): scala.Double = pow(x, b)
  }


  def standardDeviation(vector: List[Double]): Double = {
    require(vector.nonEmpty, "empty list")
    val Xn = vector.sum / vector.size
    (vector.foldLeft(0.0)(
      (acc, Xi) => {
        acc + ((Xi - Xn) ^ 2.0)
      }
    ) / vector.size) ^ 0.5
  }

  def letterFrequencyRanking(corpus: String): String = {
    corpus
      .toLowerCase
      .toList
      .filter(_.isLetter)
      .foldLeft(new LetterContainer(Map[Char, Int]()))(
        (acc, letter) => {
          acc.add(letter)
        })
      .toString
  }

  def romanji(katakana: String): String = {
    katakana.foldLeft(List[Char]())(
      (acc, next) => {
        next match {
          case 'ー' =>
            require(acc.nonEmpty, "symbol \'ー\' can't lengthen nothing")
            acc.dropRight(1) :+ Katakana.longVowels(acc.last)
          case 'ヤ' | 'ユ' | 'ヨ' =>
            require(acc.nonEmpty && acc.last == 'i', "\'i\' symbol missing")
            acc.dropRight(1) ++ Katakana.symbols(next)
          case 'ン' | 'ッ' | ' ' =>
            acc :+ next
          case _ => acc ++ Katakana.symbols(next)
        }
      }
    ).foldRight(List[Char]())(
      (next, acc) => {
        next match {
          case 'ン' =>
            require(acc.nonEmpty && acc.head == 'n', "symbol ン doubles the following consonant " +
              "only in the case of na, ni, nu, ne, no syllables")
            acc.head::acc
          case 'ッ' =>
            require(acc.nonEmpty, "symbol \'ー\' can't doubles nothing")
            acc.head::acc
          case _ => next::acc
        }
      }
    ).mkString
  }

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