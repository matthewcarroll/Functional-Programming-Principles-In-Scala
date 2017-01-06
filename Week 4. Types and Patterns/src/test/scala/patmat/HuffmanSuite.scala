package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  def characterDictionary(xs: List[Char]): Map[Char, Int] = {
    xs.foldLeft(Map[Char, Int]())((d, c) =>
      if (d.contains(c)) d + (c -> (d(c) + 1))
      else d + (c -> 1)
    )
  }

    test("encode") {
      val text = "scala"
      var textList = List('l', 'i', 't', 'e', 'r', 'a', 't', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.')
      textList = text.toList
      var tree = createCodeTree(textList)
      var encodedBits = List(1,0,  1,1,0, 0, 1,1,1, 0)
      val d = decode(tree, encodedBits)
      assert(d.mkString === text)
      val e = encode(tree)(textList)
      assert(e == encodedBits)
      val ct2 = convert(tree)
      print(ct2)
}

    test("decode") {
    val text = "encoreuntextetressecret"
    var textList = List('l', 'i', 't', 'e', 'r', 'a', 't', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.')
    textList = text.toList
    var tree = createCodeTree(textList)
    tree = frenchCode
    var encodedBits = List(1,0,  1,1,0, 0, 1,1,1, 0)
    encodedBits = List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1)
    val d = decode(tree, encodedBits)
    assert(d.mkString === text)
    val e = encode(tree)(textList)
    assert(e == encodedBits)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val l = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))
    val c = combine(leaflist)
    val c2 = combine(leaflist)
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val enc = encode(t1)("ab".toList)
      val dec = decode(t1, enc)
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}

