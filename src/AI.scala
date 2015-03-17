class AI(private var player: Player, private var depth: Int) extends Solver {

  
  /**
   * Here we will have only 1 preferred move or the 
   * best move at the current state of board b
   */
  override def getMoves(b: Board): Array[Move] = ???

  def minimax(s: State) {
      if(!s.children.isEmpty){
        for (si <- s.children) {
          minimax(si)
        }
      }else{
        s.value = evaluateBoard(s.board)
      }    
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
			if (d != 0) { // Still at
				var children = s.getChildren()
						if (!children.isEmpty) { // Not a leaf, has children
							for (si <- children) {
								si.initializeChildren()
                createGameTree(si, (d-1))
							}
						}
			}
	}

  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}
  