package com.art4ul.algorithms.sort

class InsertionSort[T <% Ordered[T]] {

  def swap(array: Array[T], firstIndex: Int, secondIndex: Int): Unit = {
    val tmp: T = array(firstIndex)
    array(firstIndex) = array(secondIndex)
    array(secondIndex) = tmp
  }

  def sort(array: Array[T]): Unit = {
    for (i <- 1 to array.length-1) {
      var j = i
      while (j>0 && array(j) < array(j - 1)) {
        swap(array,j,j-1)
        j-=1
      }
    }
  }
}

