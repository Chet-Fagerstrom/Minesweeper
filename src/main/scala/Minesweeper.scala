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
import scala.collection.mutable



object Minesweeper {
  def main(args: Array[String]): Unit = {

    
    def parseLine(line: String): score = { //Parses a line of the high scores file by separating the last "word" (the time) from the first words in case the username has spaces in it.
      val split = line.split(" ")
      score(split.reverse.tail.reverse.mkString(" "), split.reverse(0).toDouble)
    }

    

    val pw = new PrintWriter(new FileWriter("MineSweeperScores.txt", true))

    val app = new JFXApp {
      stage = new JFXApp.PrimaryStage {

        var hwm: Array[String] =  Array()
        while(hwm.length != 3){
          val gameStart = new TextInputDialog(defaultValue = ""){ //game prep dialog box
            initOwner(stage)
            title = "New Game"
            headerText = "Game prep: "
            contentText = "Enter comma separated game parameters as follows: height,width,number of mines "
          }
          
          val input = gameStart.showAndWait() match {
            case Some(value) => value
            case None => "9,9,10"
          }
          
          val pre = input.split(",").toArray
          if(pre.length == 3 && pre.forall(_.charAt(0).isDigit)){
            hwm = pre
          } else {
            Array()
          }
        }

        val h = hwm(0).toInt
        val w = hwm(1).toInt
        val m = hwm(2).toInt
        
        def winHandler(start: Long): Unit = {
          val end = System.nanoTime()
          val time = (end-start)/1e9 //Converts game time length to seconds
          val dialog = new TextInputDialog(defaultValue = ""){ //pops up Winner dialog box
            initOwner(stage)
            title = "Win!"
            headerText = "Your time: " + time
            contentText = "Please Enter Your Name"
          }
          val ign = dialog.showAndWait() match {
            case Some(name) => if(name.trim=="") "Anonymous" else name //If a player is lazy and enters some spaces as their name, it won't make the parse function angry
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
        
        def recurClear(currTile: Tile, currBut: Button, field: Minefield, buttons: IndexedSeq[Button]): Unit = {
          currTile.flip()
          val surr = field.getSurr(currTile.x, currTile.y)
          if (surr > 0){
            currBut.text = "" + surr
          } else {
            currBut.text = ""
            //field.getSurrTiles(currTile).map { case (x,y) => recurClear(field.getTileAt(x,y), getButtonAt(butTiles, x, y), field, buttons)}
          }
          currBut.background = new Background(Array(new BackgroundFill(Gray, new CornerRadii(4), Insets.apply(.25))))
        }
        
        def getButtonAt(bT: mutable.ListBuffer[(Button,Tile)], x: Int, y: Int): Button = {
          bT.find { case (b,t) => t.x == x && t.y == y}.get._1
        }
        
        title = w + " by " + h + " Minesweeper Game by Chet Fagerstrom"
        
        var butTiles: mutable.ListBuffer[(Button, Tile)] = mutable.ListBuffer()
        
        val adjH = if (h > 9) h/9.0 else 1
        val adjW = if (w > 19) w/19.0 else 1
        scene = new Scene((w*100)/adjW,(h*100)/adjH){
          val start = System.nanoTime()
          val field = Minefield(m,h,w)
          var gameOver = false  
          
          val buttons: IndexedSeq[Button] = for(i <- 1 to w; j <- 1 to h) yield {
            val button = new Button("")
            val thisTile = field.getTileAt(i,j)
            butTiles += ((button, thisTile))
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
                      recurClear(thisTile, button, field, buttons)
                      // thisTile.flip()
                      // val surr = field.getSurr(thisTile.x, thisTile.y)
                      // if (surr > 0) button.text = "" + surr
                      // else button.text = ""
                      // button.background = new Background(Array(new BackgroundFill(Gray, new CornerRadii(4), Insets.apply(.25))))
                      if(field.isCleared()){//Win
                        gameOver = true
                        title = ":)"
                        winHandler(start)
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