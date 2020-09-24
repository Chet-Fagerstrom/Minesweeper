package scala

import java.io.PrintWriter
import java.io.FileWriter
import scala.io.Source

case class score(name: String, time: Double)

class Scores(a: Array[score]) {
  val pw = new PrintWriter(new FileWriter("MineSweeperScores.txt", false))
  
  def inSortion(): Unit = { //If there were a lot of entries, Insertion sort would be particularly useful since each initial Array is nearly sorted already except for the new entry
    for(i <- 1 until a.length) {
      var j = i-1
      var tmp = a(i)
      while(j >= 0 && tmp.time < a(j).time){
        a(j+1) = a(j)
        j -= 1
      }
      a(j+1) = tmp
    }
  }
  
  def toFile(): Unit = {
    a.map(s => s.name+" "+s.time).foreach(pw.println)
    pw.close()
  }
  
  
  def printScores(): Unit = {
    val output = Source.fromFile("MineSweeperScores.txt")
    println("Minesweeper High Scores: \n")
    val out = output.getLines.foreach(println) //Prints High Scores to console
    output.close
  }
}