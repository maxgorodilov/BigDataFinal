package com.example.boyermoore

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoyerMooreSearchTest extends AnyFlatSpec with Matchers {

  val TEXT1 = "InhisbookseriesTheArtofComputerProgrammingpublishedbyAddisonWesleyDKnuthusesanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguagestoillustratetheconceptsandalgorithmsastheyarepresented"
  val TEXT2 = "Nearby farms grew a half acre of alfalfa on the dairy's behalf, with bales of all that alfalfa exchanged for milk."
  val PAT1 = "put"
  val PAT2 = "and"
  val PAT3 = "alfalfa"

  "BoyerMooreSearch" should "find pattern 'put' in TEXT1" in {
    val result = BoyerMooreSearch.stringSearch(PAT1, TEXT1)
    result shouldEqual List(26,90)
  }

  it should "find pattern 'and' in TEXT1" in {
    val result = BoyerMooreSearch.stringSearch(PAT2, TEXT1)
    result shouldEqual List(101, 128, 171)
  }

  it should "find pattern 'alfalfa' in TEXT2" in {
    val result = BoyerMooreSearch.stringSearch(PAT3, TEXT2)
    result shouldEqual List(33, 87)
  }

  it should "handle empty pattern" in {
    val result = BoyerMooreSearch.stringSearch("", TEXT1)
    result shouldEqual List.empty
  }

  it should "handle pattern longer than text" in {
    val result = BoyerMooreSearch.stringSearch("This pattern is longer than the text", TEXT1)
    result shouldEqual List.empty
  }
}