package com.example.boyermoore

object BoyerMooreSearch {
  val alphabetSize: Int = 256

  def alphabetIndex(c: Char): Int = {
    val code = c.toInt
    assert(code >= 0 && code < alphabetSize, "Character must be within ASCII range")
    code
  }

  def matchLength(s: String, idx1: Int, idx2: Int): Int = {
    if (idx1 == idx2) return s.length - idx1
    var matchCount = 0
    var i = idx1
    var j = idx2
    while (i < s.length && j < s.length && s(i) == s(j)) {
      matchCount += 1
      i += 1
      j += 1
    }
    matchCount
  }

  def fundamentalPreprocess(s: String): Array[Int] = {
    if (s.isEmpty) return Array.empty
    if (s.length == 1) return Array(1)
    val z = Array.fill(s.length)(0)
    z(0) = s.length
    z(1) = matchLength(s, 0, 1)
    for (i <- 2 until 1 + z(1)) {
      z(i) = z(1) - i + 1
    }
    var l = 0
    var r = 0
    for (i <- 2 + z(1) until s.length) {
      if (i <= r) {
        val k = i - l
        val b = z(k)
        val a = r - i + 1
        z(i) = if (b < a) b else a + matchLength(s, a, r + 1)
        if (z(i) > 0) {
          l = i
          r = i + z(i) - 1
        }
      } else {
        z(i) = matchLength(s, 0, i)
        if (z(i) > 0) {
          l = i
          r = i + z(i) - 1
        }
      }
    }
    z
  }

  def badCharacterTable(s: String): Array[Array[Int]] = {
    if (s.isEmpty) return Array.fill(alphabetSize)(Array.empty)
    val r = Array.fill(alphabetSize)(Array(-1))
    val alpha = Array.fill(alphabetSize)(-1)
    for ((c, idx) <- s.zipWithIndex) {
      alpha(alphabetIndex(c)) = idx
      for (a <- alpha.indices) {
        r(a) = r(a) :+ alpha(a)
      }
    }
    r
  }

  def goodSuffixTable(s: String): Array[Int] = {
    val l = Array.fill(s.length)(-1)
    val n = fundamentalPreprocess(s.reverse).reverse
    for (j <- 0 until s.length - 1) {
      val i = s.length - n(j)
      if (i != s.length) {
        l(i) = j
      }
    }
    l
  }

  def fullShiftTable(s: String): Array[Int] = {
    val f = Array.fill(s.length)(0)
    val z = fundamentalPreprocess(s)
    var longest = 0
    for ((zv, i) <- z.reverse.zipWithIndex) {
      longest = if (zv == i + 1) math.max(zv, longest) else longest
      f(s.length - i - 1) = longest
    }
    f
  }

  def stringSearch(P: String, T: String): List[Int] = {
    if (P.isEmpty || T.length < P.length) return List.empty

    var matches: List[Int] = List.empty

    val r = badCharacterTable(P)
    val l = goodSuffixTable(P)
    val f = fullShiftTable(P)

    var k = P.length - 1
    var previousK = -1
    while (k < T.length) {
      var i = P.length - 1
      var h = k
      while (i >= 0 && h > previousK && P(i) == T(h)) {
        i -= 1
        h -= 1
      }
      if (i == -1 || h == previousK) {
        matches = matches :+ (k - P.length + 1)
        k += (if (P.length > 1) P.length - f(1) else 1)
      } else {
        val charShift = i - r(alphabetIndex(T(h)))(i)
        val suffixShift = if (i + 1 == P.length) 1
        else if (l(i + 1) == -1) P.length - f(i + 1)
        else P.length - 1 - l(i + 1)
        val shift = math.max(charShift, suffixShift)
        previousK = if (shift >= i + 1) k else previousK
        k += shift
      }
    }
    matches
  }

  def main(args: Array[String]): Unit = {

    println("BoyerMooreSearch is ready!")
  }
}
