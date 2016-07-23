import java.awt.{BasicStroke, Color, Toolkit}
import javax.swing.ImageIcon

import scala.annotation.{switch, tailrec}
import scala.math._
import scala.swing.BorderPanel.Position._
import scala.swing.{Dimension, Graphics2D, Panel, TextArea, _}

/**
 * Created by Arpit Goyal on 7/8/2015.
 */
case class NQueens(var n : Int) {

  var queensPlaces : List[(Int,Int)] = List[(Int, Int)]()
  val textArea = new TextArea
  val chessPanel = new Panel {

    val image = new ImageIcon(this.getClass.getResource("/Queen.png")).getImage()

    val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
    val width = screenSize.getWidth()
    val height = screenSize.getHeight()

    override protected def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      val NUM_BLOCKS = n
      val BLOCK_SIZE = getBlockSize(n)
      val RECT_LENGTH = BLOCK_SIZE * NUM_BLOCKS + 10
      val RECT_HEIGHT = BLOCK_SIZE * NUM_BLOCKS + 10
      val RECT_X = (width/2 - RECT_LENGTH/2).toInt
      val RECT_Y = 10
      val BLOCK_X = RECT_X + 5
      val BLOCK_Y = RECT_Y + 5
      g.setColor(Color.BLACK)
      g.setStroke(new BasicStroke(5))
      g.drawRect(RECT_X, RECT_Y, RECT_LENGTH, RECT_HEIGHT)

      var (x, y) = (BLOCK_X, BLOCK_Y)
      for(i <- 1 to NUM_BLOCKS; j <- 1 to NUM_BLOCKS) {
        if ((i + j) % 2 == 0) g.setColor(Color.BLACK) else g.setColor(Color.WHITE)
        g.fillRect(x, y, BLOCK_SIZE, BLOCK_SIZE)
        if(queensPlaces.contains((i,j))) g.drawImage(image, x, y,BLOCK_SIZE, BLOCK_SIZE, null)
        x += BLOCK_SIZE
        if (j == NUM_BLOCKS) {
          x = BLOCK_X
          y += BLOCK_SIZE
        }
      }
    }

    def getBlockSize(n : Int) : Int = {
      (n : @switch) match {
        case 10 => 58
        case 11 => 56
        case 12 => 54
        case 13 => 49
        case 14 => 46
        case 15 => 43
        case _ => 60
      }
    }
  }

  val comboBox = new ComboBox(2.to(15).map(_.toString))
  var thread = new Thread
  val timeComboBox = new ComboBox("0 ms" :: (10 to 100 by 20).map(_.toString + " ms").toList ::: (100 to 1000 by 100).map(_.toString + " ms").toList)

  val controlPanel = new FlowPanel {
    contents += comboBox
    contents += Button("Start"){
      if(thread.isAlive) thread.stop
      thread = new Thread(new Runnable() {
        override def run(): Unit = {
          pauseTime = timeComboBox.selection.item.split(" ")(0).toInt
          n = comboBox.selection.item.toInt
          nQueen(n)
        }
      })
      thread.start
    }
    contents += Button("Stop"){
      thread.stop
    }
    contents += timeComboBox
  }

    val frame = new MainFrame{
    title = "NQueens"
    contents = new BorderPanel {
      layout += chessPanel -> Center
      layout += controlPanel -> South
    }
    size = new Dimension(1000, 600)
    centerOnScreen
  }
  frame.visible = true
  frame.maximize

  var pauseTime = 400

  def drawQueenAndWait(list : List[(Int, Int)]) : Unit = {
    queensPlaces = list
    chessPanel.repaint
    try{Thread.sleep(pauseTime)} catch{case e : Exception =>}
  }

  def nQueen(n : Int) : String = {
    @tailrec
    def searchRows(n : Int, list : List[(Int, Int)], row : Int) : List[(Int, Int)] = {
      if(row > n) list
      else if(row <= 0) list
      else if(list.length == n) {
        drawQueenAndWait(list)
        list
      }
      else {
        if(list.length == row) searchRows(n, searchColumns(n, list.init, list(row-1)._2+1, row), row+1)
        else searchRows(n, searchColumns(n, list, 1, row), row+1)
      }
    }
    @tailrec
    def searchColumns(n : Int, list : List[(Int, Int)], col : Int, row : Int) : List[(Int, Int)] = {
      if(col > n) searchRows(n, list, row-1)
      else if(validPlace(list, (row,col))) list :+ (row,col)
      else{
        drawQueenAndWait(list :+ (row, col))
        searchColumns(n, list, col+1, row)
      }
    }
    @tailrec
    def validPlace(list : List[(Int, Int)], place : (Int, Int)) : Boolean = list match {
      case Nil => true
      case head :: tail => if(head._2 == place._2 || abs(place._1 - head._1) == abs(place._2 - head._2)) false
      else validPlace(tail, place)
    }
    searchRows(n, List[(Int, Int)](), 1).map(_._2).mkString(", ")
  }
}

object NQueens extends App {
  val obj = NQueens(2)
}
