package models

object Game extends App {
  def isChecked(board: Board, team: Team) =
    genPseudoLegalMoves(board, team.otherTeam).contains(board.kingPos(team))

  def genPseudoLegalMoves(board: Board, team: Team): List[Pos] =
    board
      .getAllPositionsForTeam(team)
      .flatMap(pos => genPseudoLegalMovesFromPos(board, pos))

  def genPseudoLegalMovesFromPos(board: Board, from: Pos): List[Pos] =
    board
      .unsafePieceAt(from)
      .genNextPosList(from, board)
      .filter(to => isValidPseudoLegalMove(board, from, to))

  def isValidPseudoLegalMove(board: Board, from: Pos, to: Pos): Boolean =
    isClearPath(board, from, to) && isValidDestination(board, from, to)

  def isValidDestination(board: Board, from: Pos, to: Pos): Boolean = {
    val attackingTeam = board.unsafeTeamAt(from);
    val teamOfTileMovedToOptional = board.teamAt(to);

    teamOfTileMovedToOptional.forall(_ == attackingTeam)
  }

  def isClearPath(board: Board, from: Pos, to: Pos): Boolean =
    drawPathForPiece(board, from, to).forall(board.isTileClear(_))

  def drawPathForPiece(board: Board, from: Pos, to: Pos): List[Pos] =
    board.unsafePieceAt(from) match {
      case Knight(_) => List()
      case _         => drawPath(from, to)
    }

  def drawPath(from: Pos, to: Pos): List[Pos] = {
    val rowsInPath = from.row until to.row by getMoveDir(from.row, to.row)
    val colsInPath = from.col until to.col by getMoveDir(from.col, to.col)

    (rowsInPath zip colsInPath)
      .drop(1)
      .map(pair => Pos(pair._1, pair._2))
      .toList
  }

  def getMoveDir(from: Int, to: Int): Int = {
    val diff = to - from

    if (diff < 0) -1 else if (diff > 0) 1 else diff
  }
}
