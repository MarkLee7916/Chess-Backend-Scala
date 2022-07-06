package models

import collection.immutable.List
import scala.annotation.unused
import constants.BoardSize

sealed trait Piece {
  def team: Team

  def genNextPosList(pos: Pos, board: Board): List[Pos]

  def genNextPosListFromPieceBehaviour(f: Int => List[Pos]): List[Pos] =
    (1 until BoardSize)
      .flatMap(f(_))
      .filter(_.isOnBoard)
      .distinct
      .toList
}

case class Pawn(team: Team) extends Piece {
  def genNextPosList(pos: Pos, board: Board): List[Pos] =
    List(
      genOnePlaceForwardPos(pos, board),
      genTwoPlaceForwardPos(pos, board),
      genLeftTakePos(pos, board),
      genRightTakePos(pos, board)
    ).flatten

  private def genOnePlaceForwardPos(pos: Pos, board: Board): Option[Pos] = {
    val posToMoveTo = Pos(pos.row + team.forwardDirection, pos.col)

    if (board.isTileClear(posToMoveTo)) Some(posToMoveTo) else None
  }

  private def genTwoPlaceForwardPos(pos: Pos, board: Board): Option[Pos] = {
    val posToMoveTo = Pos(pos.row + team.forwardDirection * 2, pos.col)
    val isRightRow = posToMoveTo.row == team.originalPawnRow;
    val canMove = board.isTileClear(posToMoveTo) && isRightRow

    if (canMove) Some(posToMoveTo) else None
  }

  private def genLeftTakePos(pos: Pos, board: Board): Option[Pos] = {
    val posToMoveTo = Pos(pos.row + team.forwardDirection, pos.col - 1)
    val canMove = board.isTileOccupiedByTeam(posToMoveTo, team.otherTeam)

    if (canMove) Some(posToMoveTo) else None
  }

  private def genRightTakePos(pos: Pos, board: Board): Option[Pos] = {
    val posToMoveTo = Pos(pos.row + team.forwardDirection, pos.col + 1)
    val canMove = board.isTileOccupiedByTeam(posToMoveTo, team.otherTeam)

    if (canMove) Some(posToMoveTo) else None
  }
}

case class Rook(team: Team) extends Piece {
  def genNextPosList(pos: Pos, @unused b: Board): List[Pos] =
    genNextPosListFromPieceBehaviour(moveSize =>
      List(
        Pos(pos.row, pos.col - moveSize),
        Pos(pos.row, pos.col + moveSize),
        Pos(pos.row - moveSize, pos.col),
        Pos(pos.row + moveSize, pos.col)
      )
    )
}

case class Knight(team: Team) extends Piece {
  def genNextPosList(pos: Pos, @unused b: Board): List[Pos] =
    genNextPosListFromPieceBehaviour(_ =>
      List(
        Pos(pos.row - 2, pos.col - 1),
        Pos(pos.row - 2, pos.col + 1),
        Pos(pos.row - 1, pos.col - 2),
        Pos(pos.row - 1, pos.col + 2),
        Pos(pos.row + 1, pos.col - 2),
        Pos(pos.row + 1, pos.col + 2),
        Pos(pos.row + 2, pos.col - 1),
        Pos(pos.row + 2, pos.col + 1)
      )
    )
}

case class King(team: Team) extends Piece {
  def genNextPosList(pos: Pos, @unused b: Board): List[Pos] =
    genNextPosListFromPieceBehaviour(_ =>
      List(
        Pos(pos.row - 1, pos.col - 1),
        Pos(pos.row - 1, pos.col),
        Pos(pos.row - 1, pos.col + 1),
        Pos(pos.row, pos.col - 1),
        Pos(pos.row, pos.col + 1),
        Pos(pos.row + 1, pos.col - 1),
        Pos(pos.row + 1, pos.col),
        Pos(pos.row + 1, pos.col + 1)
      )
    )
}

case class Queen(team: Team) extends Piece {
  def genNextPosList(pos: Pos, @unused b: Board): List[Pos] =
    genNextPosListFromPieceBehaviour(moveSize =>
      List(
        Pos(pos.row - moveSize, pos.col - moveSize),
        Pos(pos.row - moveSize, pos.col),
        Pos(pos.row - moveSize, pos.col + moveSize),
        Pos(pos.row, pos.col - moveSize),
        Pos(pos.row, pos.col + moveSize),
        Pos(pos.row + moveSize, pos.col - moveSize),
        Pos(pos.row + moveSize, pos.col),
        Pos(pos.row + moveSize, pos.col + moveSize)
      )
    )
}

case class Bishop(team: Team) extends Piece {
  def genNextPosList(pos: Pos, @unused b: Board): List[Pos] =
    genNextPosListFromPieceBehaviour(moveSize =>
      List(
        Pos(pos.row - moveSize, pos.col - moveSize),
        Pos(pos.row - moveSize, pos.col + moveSize),
        Pos(pos.row + moveSize, pos.col - moveSize),
        Pos(pos.row + moveSize, pos.col + moveSize)
      )
    )

}

sealed trait Team {
  def forwardDirection: Int
  def originalPawnRow: Int

  def otherTeam: Team =
    this match {
      case Black() => White()
      case White() => Black()
    }
}
case class White() extends Team {
  def forwardDirection = 1
  def originalPawnRow: Int = 1
}
case class Black() extends Team {
  def forwardDirection = -1
  def originalPawnRow: Int = 6
}
