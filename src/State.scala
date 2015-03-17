import java.io.FileNotFoundException
import java.io.PrintWriter
import java.io.UnsupportedEncodingException
import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import State.length0
import java.io.FileWriter

class State(@BeanProperty var player: Player, @BeanProperty var board: Board, @BeanProperty var lastMove: Move)
  extends Comparable[Any] {

  @BeanProperty
  var children: Array[State] = length0

  @BeanProperty
  var value: Int = 0
  
  /**
   * Gets an array of all the possible moves for this player,
   * iterates through each one, creating a board which is a  
   * copy of the current board, and makes the appropriate move.
   * The resultant boards are stored, along with the players
   * opponent, in the children array. 
   */

  def initializeChildren() {
    
  var moves = board.getPossibleMoves(player.opponent);
    
  // Copy the current board, make the next move, and store in the
  // children array. Use ArrayBuffer to append elements to the array
  
    val childArray = ArrayBuffer[State]()
  
    for (m <- 0 until moves.length) {
      var b = new Board(board)
      b.makeMove(moves(m))
      childArray += new State(player.opponent, b, moves(m))
    }
    children = childArray.toArray
    
    
  }

  def writeToFile() {
	  var writer: PrintWriter = null
			  try {
				  writer = new PrintWriter("output.txt", "UTF-8")
				  writer.println(this)
			  } catch {
			  case e@(_: FileNotFoundException | _: UnsupportedEncodingException) => e.printStackTrace()
			  } finally {
				  writer.close();
			  }
  }

  override def toString(): String = {
    println("State.toString printing")
    toStringHelper(0, "")
  }
  

  override def compareTo(o: Any): Int = ???

  private def toStringHelper(d: Int, ind: String): String = {
    var str = ind + player + " to play"+System.lineSeparator()
    str = str + ind + "Value: " + value + System.lineSeparator()
    str = str + board.toString(ind) + System.lineSeparator()
    if (children != null && children.length > 0) {
      str = str + ind + "Children at depth " + (d + 1) + ":" + System.lineSeparator() + ind +
        "----------------" + System.lineSeparator()
      for (s <- children) {
        str = str + s.toStringHelper(d + 1, ind + "   ")
      }
    }
    str
  }
}

object State {

  val length0 = Array[State]()
}

