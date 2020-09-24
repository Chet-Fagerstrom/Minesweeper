package scala

import scalafx.application.JFXApp
import scalafx.scene.control._
import scalafx.scene.text.Text
import scalafx.scene.Scene
import scalafx.scene.text.Text
import scalafx.geometry._
import scalafx.scene.paint.Color._
import scalafx.scene.layout._
import scalafx.scene.control.Button
import scalafx.event.ActionEvent
import scalafx.Includes._
import scalafx.scene.text.Font
import scalafx.scene.input._
import java.io.PrintWriter
import java.io.FileWriter
import scala.io.Source
import scalafx.scene.control.Alert._
import scala.io.StdIn._



object Minesweeper {
  def main(args: Array[String]): Unit = {

    
    def parseLine(line: String): score = { //Parses a line of the high scores file by separating the last "word" (the time) from the first words in case the username has spaces in it.
      val split = line.split(" ")
      score(split.reverse.tail.reverse.mkString(" "), split.reverse(0).toDouble)
    }

    

    val pw = new PrintWriter(new FileWriter("MineSweeperScores.txt", true))

    
    println("How many tiles tall do you want your minefield to be? I recommend 9.")
    // val h = readLine.toInt
    val h = 9

    println("How many tiles wide do you want your minefield to be? I recommend 9 again.")
    // val w = readLine.toInt
    val w = 9

    println("How many mines do you want in this minefield? I recommend 10.")
    // val m = readLine.toInt
    val m = 10

    val app = new JFXApp {
      stage = new JFXApp.PrimaryStage {
        val start = System.nanoTime()


        def winHandler(): Unit = {
          val end = System.nanoTime()
          val time = (end-start)/1e9 //Converts game time length to seconds
          val dialog = new TextInputDialog(defaultValue = ""){ //pops up Winner dialog box
            initOwner(stage)
              title = "Winner Winner Chicken Dinner"
              headerText = "Your time: " + time
              contentText = "Please Enter Your Name"
          }
          val ign = dialog.showAndWait() match {
            case Some(name) => if(name.trim=="") "Anonymous" else name //If a player is a moron and enters some spaces as their name, it won't make the parse function angry
            case None => "Anonymous" //If player cancels dialog box, their score is recorded as "Anonymous"
          }
          pw.println(ign + " " + time) //Adds player data to end of score file
          pw.close()
          val dataSource = Source.fromFile("MineSweeperScores.txt")
          val data = new Scores(dataSource.getLines.map(parseLine).toArray)
          dataSource.close
          data.inSortion //sorts score file with new addition
          data.toFile //saves new score file
          data.printScores() //prints leaderboard
        }

        def lossHandler(): Unit = {
          val dialog = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "(✖╭╮✖)"
            headerText = "You stepped on a mine."
            contentText = "Game lost."
          }.showAndWait()
        }

        title = w + " by " + h + " Minesweeper Game by Chet Fagerstrom"
        
        val adjH = if (h > 9) h/9.0 else 1
        val adjW = if (w > 19) w/19.0 else 1
        scene = new Scene((w*100)/adjW,(h*100)/adjH){
          val field = Minefield(m,h,w)
          var gameOver = false
          val buttons = for(i <- 1 to w; j <- 1 to h) yield {
            val button = new Button("")
            val thisTile = field.getTileAt(i,j)
            button.layoutX = (i-1) * (w*100)/adjW / w
            button.layoutY = (j-1) * (h*100)/adjH / h
            button.prefWidth = (w*100)/adjW / w
            button.prefHeight = (h*100)/adjH / h


            button.onMouseClicked = (me:MouseEvent) => {
              if(!gameOver){
                if(me.button == MouseButton.Secondary) {//Tile Marking
                  thisTile.changeMark()
                  button.text = if(thisTile.marked()) "Flag" else ""
                } else if(me.button == MouseButton.Primary){//Tile Action
                  if(!thisTile.marked()){
                    if(!thisTile.hasMine) {//Didn't step on a mine
                      thisTile.flip()
                      button.text = ""+field.getSurr(thisTile.x, thisTile.y)
                      button.background = new Background(Array(new BackgroundFill(Gray, new CornerRadii(4), Insets.apply(.25))))
                      if(field.isCleared()){//Win
                        gameOver = true
                        title = ":)"
                        winHandler()
                      }
                    } else {

                      button.text = "You have\n exploded."
                      gameOver = true
                      title = ":("
                      lossHandler()
                    }
                  }
                }
              }
            }
          button
          }
          content = buttons
        }
      }
    }
    app.main(args)
  }
}