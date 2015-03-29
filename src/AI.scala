class AI(private var player: Player, private var depth: Int) extends Solver {

  
  /**
   * Get the best move(s) at the current state of board b
   * gets the children at the current state of board and finds the move(s) with the maximum value
   */
  override def getMoves(b: Board): Array[Move] = {
    return ???
  }

  
  /**
 * State s is a node of a game tree (i.e. the current State of the game).
 * Use the Minimax algorithm to assign a numerical value to each State of the
 * tree rooted at s, indicating how desirable that java.State is to this player.
 */
  def minimax(s: State) {
    def evalLeaf(rs:State) : Unit = {
      if(!rs.children.isEmpty){        
        for(childS <- rs.children)
          evalLeaf(childS)
      }
      else rs.value = this.evaluateBoard(rs.board)
    }
    evalLeaf(s)
    
    var minValue:Int = 0
    var maxValue:Int = 0
    
    def evalParents(d:Int, depthParam:Int, rs:State) : Unit = {
      if(d!=depthParam){
        if(!rs.children.isEmpty){        
          for(childS <- rs.children)
            evalParents(d+1,depthParam, childS)
        }
      }else{
          // we are at second last level with leaf children already evaluated
          if(rs.children != null && rs.children.length > 0)
            if(d%2==0) maxValue = rs.children(0).value else minValue = rs.children(0).value 
          for(i <- 0 until rs.children.length){
            if(d%2==0) maxValue = if(rs.children(i).value > maxValue) rs.children(i).value else maxValue
            else minValue = if(rs.children(i).value < minValue) rs.children(i).value else minValue
          }
          rs.value = if(d%2==0) maxValue else minValue
        }
    }
    
    for(i <- depth-1 to 0 by -1)
      evalParents(0,i,s)
}

  /**
   * Evaluate the desirability of Board b for this player
   * Precondition: b is a leaf node of the game tree (because that is most
   * effective when looking several moves into the future).
   */
  def evaluateBoard(b: Board): Int = {
    val winner = b.hasConnectFour()
    var value = 0
    if (!winner.isDefined) {
      val locs = b.winLocations()
      for (loc <- locs; p <- loc) {
        value += (if (p == player) 1 else if (p != null) -1 else 0)
      }
    } else {
      var numEmpty = 0
      var r = 0
      while (r < Board.NUM_ROWS) {
        var c = 0
        while (c < Board.NUM_COLS) {
          if (b.getTile(r, c) == null) numEmpty += 1
          c = c + 1
        }
        r = r + 1
      }
      value = (if (winner.get == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
}

object AI {

  /**
   * Generate the game tree with root s of depth d.
   * The game tree's nodes are State objects that represent the state of a game
   * and whose children are all possible States that can result from the next move.
   * <p/>
   * NOTE: this method runs in exponential time with respect to d.
   * With d around 5 or 6, it is extremely slow and will start to take a very
   * long time to run.
   * <p/>
   * Note: If s has a winner (four in a row), it should be a leaf.
   */
	def createGameTree(s: State, d: Int) : Unit = {
			if (d != 0) {
        s.initializeChildren()
				var children = s.getChildren()
						if (!children.isEmpty) { // Not a leaf, has children
							for (si <- children) {		
                createGameTree(si, (d-1))
							}
						}
			}
	}

  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}
  
