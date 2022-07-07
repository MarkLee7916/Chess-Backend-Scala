package models

import constants.BoardSize

case class Pos(row: Int, col: Int) {
  def isOnBoard: Boolean =
    row >= 0 && row < BoardSize && col >= 0 && col < BoardSize
}

case class Board(board: Board.Matrix = Board.default) {
  type Matrix = Board.Matrix
  type Tile = Board.Tile

  def applyMove(from: Pos, to: Pos): Board = {
    val pieceToMove = unsafePieceAt(from);
    val boardWithPiecePlaced = placePieceAtPos(board, pieceToMove, to);

    Board(removePieceFromPos(boardWithPiecePlaced, from))
  }

  def placePieceAtPos(board: Matrix, piece: Piece, to: Pos): Matrix =
    board.updated(to.row, board(to.row).updated(to.col, Some(piece)))

  def removePieceFromPos(board: Matrix, from: Pos): Matrix =
    board.updated(from.row, board(from.row).updated(from.col, None))

  def kingPos(team: Team): Pos =
    getAllPositionsForTeam(team)
      .find(tileAt(_).exists(_.isInstanceOf[King]))
      .get

  def getAllPositionsForTeam(team: Team): List[Pos] =
    getAllPositionsOnBoard.filter(teamAt(_).exists(_ == team))

  def getAllPositionsOnBoard: List[Pos] = {
    val indexRange = (0 until BoardSize);

    indexRange.flatMap(row => indexRange.map(col => Pos(row, col))).toList
  }

  def unsafePieceAt(pos: Pos): Piece =
    tileAt(pos).get

  def unsafeTeamAt(pos: Pos): Team =
    teamAt(pos).get

  def tileAt(pos: Pos): Tile =
    board(pos.row)(pos.col)

  def teamAt(pos: Pos): Option[Team] =
    tileAt(pos).map(_.team)

  def isTileClear(pos: Pos): Boolean =
    pos.isOnBoard && tileAt(pos).isEmpty

  def isTileOccupiedByTeam(pos: Pos, team: Team): Boolean =
    pos.isOnBoard && tileAt(pos).exists(_.team == team)
}

case object Board {
  type Tile = Option[Piece]
  type Matrix = List[List[Tile]]

  val default: Matrix =
    List(
      List(
        Some(Pawn(Black())),
        Some(Knight(Black())),
        Some(Bishop(Black())),
        Some(Queen(Black())),
        Some(King(Black())),
        Some(Bishop(Black())),
        Some(Knight(Black())),
        Some(Rook(Black()))
      ),
      List.fill(BoardSize)(Some(Pawn(Black()))),
      List.fill(BoardSize)(None),
      List.fill(BoardSize)(None),
      List.fill(BoardSize)(None),
      List.fill(BoardSize)(None),
      List.fill(BoardSize)(Some(Pawn(White()))),
      List(
        Some(Pawn(White())),
        Some(Knight(White())),
        Some(Bishop(White())),
        Some(Queen(White())),
        Some(King(White())),
        Some(Bishop(White())),
        Some(Knight(White())),
        Some(Rook(White()))
      )
    )
}
