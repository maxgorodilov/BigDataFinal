package com.example.boyermoore

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoyerMooreSearchTest extends AnyFlatSpec with Matchers {

  val TEXT1 = "InhisbookseriesTheArtofComputerProgrammingpublishedbyAddisonWesleyDKnuthusesanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguagestoillustratetheconceptsandalgorithmsastheyarepresented"
  val TEXT2 = "Nearby farms grew a half acre of alfalfa on the dairy's behalf, with bales of all that alfalfa exchanged for milk."
  val PAT1 = "put"
  val PAT2 = "and"
  val PAT3 = "alfalfa"
  val PAT4 = "the"
  val PAT5 = "xyz"
  val PAT6 = "a"
  val PAT7 = "InhisbookseriesTheArtofComputerProgrammingpublishedbyAddisonWesleyDKnuthusesanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguagestoillustratetheconceptsandalgorithmsastheyarepresented"

  "BoyerMooreSearch" should "find pattern 'put' in TEXT1" in {
    val result = BoyerMooreSearch.stringSearch(PAT1, TEXT1)
    result shouldEqual List(26, 90)
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

  it should "find pattern 'the' in TEXT1" in {
    val result = BoyerMooreSearch.stringSearch(PAT4, TEXT1)
    result shouldEqual List(95, 160, 186)
  }

  it should "not find non-existent pattern 'xyz' in TEXT1" in {
    val result = BoyerMooreSearch.stringSearch(PAT5, TEXT1)
    result shouldEqual List.empty
  }

  it should "find all occurrences of single character 'a' in TEXT1" in {
    val result = BoyerMooreSearch.stringSearch(PAT6, TEXT1)
    result shouldEqual List(36, 76, 80, 84, 101, 107, 113, 118, 128, 131, 140, 144, 157, 171, 174, 184, 190)
  }

  it should "find the entire text as a pattern" in {
    val result = BoyerMooreSearch.stringSearch(PAT7, TEXT1)
    result shouldEqual List(0)
  }

  it should "handle empty text" in {
    val result = BoyerMooreSearch.stringSearch(PAT1, "")
    result shouldEqual List.empty
  }

  it should "handle both empty pattern and empty text" in {
    val result = BoyerMooreSearch.stringSearch("", "")
    result shouldEqual List.empty
  }

  it should "handle pattern at the beginning of the text" in {
    val result = BoyerMooreSearch.stringSearch("Inhis", TEXT1)
    result shouldEqual List(0)
  }

  it should "handle pattern at the end of the text" in {
    val result = BoyerMooreSearch.stringSearch("presented", TEXT1)
    result shouldEqual List(193)
  }

  it should "handle pattern with repeated characters" in {
    val result = BoyerMooreSearch.stringSearch("aa", "aaaaa")
    result shouldEqual List(0, 1, 2, 3)
  }

  it should "handle pattern with no repeated characters" in {
    val result = BoyerMooreSearch.stringSearch("abc", "abcabcabc")
    result shouldEqual List(0, 3, 6)
  }

  it should "handle pattern with special characters" in {
    val result = BoyerMooreSearch.stringSearch("dairy's", TEXT2)
    result shouldEqual List(48)
  }
}