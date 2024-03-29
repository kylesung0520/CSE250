/**
 * cse250.pa1.tests.GroupByStoreTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa1.tests

import cse250.objects.{AssessmentUtilities, TaxParcel}
import cse250.pa1.objects.GroupByStore
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.collection.mutable

class GroupByStoreTests extends FlatSpec with BeforeAndAfter {
  var dataStore: GroupByStore = _

  // This code is run prior to every test.
  before {
    dataStore = new GroupByStore
  }

  // Your tests for problem 1 should be contained under this header.
  behavior of "GroupByStore.invariants 1(a)"
  it should "most recently inserted Tax parcel is head of its grouping list" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for(i <- 0 until entries.length){
      dataStore.insert(entries(i))
      for(j <- 0 until dataStore.length){
        if(dataStore._groupings(j)._value.parcelInfo(dataStore._groupingAttribute) == entries(i).parcelInfo(dataStore._groupingAttribute)){
          assert(dataStore(j) == entries(i))
        }
      }
    }
  }
  
  behavior of "GroupByStore.invariants 1(b)"
  it should "sort as Alphabetical order" in {
    for(i <- 0 until dataStore.length-1){
      val current = dataStore(i).parcelInfo(dataStore._groupingAttribute)
      val next = dataStore(i+1).parcelInfo(dataStore._groupingAttribute)
      assert(current < next)
    }
  }
  
  behavior of "GroupByStore.invariants 1(c)"
  it should "sort as Alphabetical order after regroup" in {
    dataStore.regroup(attribute = "NEIGHBOUR")
    for(i <- 0 until dataStore.length-1){
      val current = dataStore(i).parcelInfo(dataStore._groupingAttribute)
      val next = dataStore(i+1).parcelInfo(dataStore._groupingAttribute)
      assert(current < next)
    }
  }

  // ^^^
  behavior of "GroupByStore.length"
  it should "be 0 when initialized" in {
    assert(dataStore.length == 0)
  }

  it should "be updated after each insertion" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      assert(dataStore.length == i + 1)
    }
  }

  behavior of "GroupByStore.insert"
  it should "..." in {

  }

  behavior of "GroupByStore.regroup"
  it should "..." in {

  }

  behavior of "GroupByStore.iterator"
  it should "retrieve all stored entries" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    val testEntriesSet = new mutable.HashSet[TaxParcel]
    
    // Add all loaded values into your dataStore.
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      testEntriesSet.add(entries(i))
    }

    // Check that all loaded values are iterated through in your dataStore.
    val dataIterator = dataStore.iterator
    val storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.
      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)
  }

  it should "retrieve all stored entries after regrouping" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    val testEntriesSet = new mutable.HashSet[TaxParcel]

    // Add all loaded values into your dataStore.
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      testEntriesSet.add(entries(i))
    }

    // Check that all loaded values are iterated through in your dataStore.
    var dataIterator = dataStore.iterator
    var storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.
      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)

    // Make a call to regroup.
    dataStore.regroup("SBL")

    // Check that all loaded values are iterated through in your dataStore.
    dataIterator = dataStore.iterator
    storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.
      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)
  }

  behavior of "GroupByStore.iterator(String)"
  it should "..." in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for(i <- 0 until entries.length){
      dataStore.insert(entries(i))
    }
    assert(dataStore.iterator("MAIN").hasNext == false)
  }
}
