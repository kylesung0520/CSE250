/**
 * cse250.pa4.tests.AVLTreeMapTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */

package cse250.pa4.tests

import cse250.pa4.{AVLTree, AVLTreeMap}
import org.scalatest.FlatSpec


class AVLTreeMapTests extends FlatSpec {
  val testSize = 10
  val inputKeys = Array.tabulate(testSize)(i => i + 1)
  val inputValues = Array.tabulate(testSize)(i => ('A'+i).toChar.toString)
  behavior of "AVLTreeMap.insert"
  it should "add the (key,value) pairs" in {
    val treeMap = new AVLTreeMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      assert(elem == elements(i))
    }
  }

  behavior of "Problem1.rotLeft."
  it should "..." in {
    val treeObj = new AVLTree[Int,String]
    val root = new treeObj.AVLNode[(Int,String)]((1,"A"), null, null, null, false, false)
    val node = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, null, false, false)
    val node1 = new treeObj.AVLNode[(Int, String)]((3,"C"), null, null, null, false, false)

    root._right = node
    root._rightH = true
    node._parent = root
    node._right = node1
    node._rightH = true
    node1._parent = node

    treeObj.rotateLeft(root)
    assert(root._left == node1)
    assert(node._rightH == false)
    assert(root._leftH == false && root._rightH == false)
  }

  behavior of "Problem1.rotRight."
  it should "..." in {
    val treeObj = new AVLTree[Int,String]
    val root = new treeObj.AVLNode[(Int,String)]((1,"A"), null, null, null, false, false)
    val node = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, null, false, false)
    val node1 = new treeObj.AVLNode[(Int, String)]((3,"C"), null, null, null, false, false)

    root._left = node
    root._leftH = true
    node._parent = root
    node._left = node1
    node._leftH = true
    node1._parent = node

    treeObj.rotateRight(root)
    assert(root._right == node1)
    assert(node._leftH == false)
    assert(root._leftH == false && root._rightH == false)

  }

  behavior of "Problem1.rotLeftRight."
  it should "..." in {
    val treeObj = new AVLTree[Int,String]
    val root = new treeObj.AVLNode[(Int,String)]((1,"A"), null, null, null, false, false)
    val node = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, null, false, false)
    val node1 = new treeObj.AVLNode[(Int, String)]((3,"C"), null, null, null, false, false)

    root._left = node
    root._leftH = true
    node._parent = root
    node._right = node1
    node._rightH = true
    node1._parent = node

    treeObj.rotateLeftRight(root)
    assert(root._right == node1)
    assert(node._rightH == false)
    assert(root._leftH == false && root._rightH == false)
  }

  behavior of "Problem1.rotRightLeft."
  it should "..." in {
    val treeObj = new AVLTree[Int,String]
    val root = new treeObj.AVLNode[(Int,String)]((1,"A"), null, null, null, false, false)
    val node = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, null, false, false)
    val node1 = new treeObj.AVLNode[(Int, String)]((3,"C"), null, null, null, false, false)

    root._right = node
    root._rightH = true
    node._parent = root
    node._left = node1
    node._leftH = true
    node1._parent = node

    treeObj.rotateRightLeft(root)
    assert(root._left == node1)
    assert(node._leftH == false)
    assert(root._leftH == false && root._rightH == false)
  }
}

