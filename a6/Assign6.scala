import Color.Color
import Rank.Rank

import scala.collection.mutable
import scala.io.StdIn
import scala.util.Try

/**
 * Created by Dante on 2015-04-06.
 */

object Utils{
  implicit class StrUtils(s:String) {
    def tryToInt:Option[Int] = Try(s.toInt).toOption
  }
  type Coord = (Int, Int)
  implicit class CoordUtils(t:Coord) {
    def x = t._1
    def y = t._2
    def +(p: Coord):Coord = (p.x + t.x, p.y + t.y)
    def *(p:Int):Coord = (t.x*p, t.y*p)
  }
}

object Rank extends Enumeration {
  type Rank = Value
  val King =    Value("K")
  val Queen =   Value("Q")
  val Rook =    Value("R")
  val Knight =  Value("Kn")
  val Bishop =  Value("B")
  val Pawn =    Value("P")
}

object Color extends Enumeration {
  type Color = Value
  val Black =   Value("B")
  val White =   Value("W")
  def negate(color: Color) = if(color == White) Black else White
}

trait Square {
  def empty :Boolean
}
case class Piece (rank: Rank, color:Color) extends Square{
  override def toString:String =
    color.toString +
    rank.toString +
    (if (rank == Rank.Knight) "" else " ")
  def empty = false
}
case class Space() extends Square{
  override def toString:String = "   "
  def empty = true
}

class Board (board: List[mutable.MutableList[Square]]){
  import Utils._
  import Color._

  var whiteCaps = mutable.MutableList[Piece]()
  var blackCaps = mutable.MutableList[Piece]()
  var message = ""
  var turn = White
  var moving = false

  def select(c:Coord):Option[Square] = {
    if (c.x>=0 && c.x<8 && c.y>=0 && c.y<8) Some(board(c.y)(c.x))
    else None
  }
  def eatOrOccupy(from:Coord, to:Coord):Boolean = {
    val pce = board(from.y)(from.x).asInstanceOf[Piece]
    select(to) match {
      case Some(p: Space) =>
        board(from.y).update(from.x, Space())
        board(to.y).update(to.x, pce)
        moving = true
      case Some(p: Piece) if p.color != pce.color =>
        if (p.color == White) blackCaps += p
        else whiteCaps += p
        board(from.y).update(from.x, Space())
        board(to.y).update(to.x, pce)
        message = pce + " ate " + p
        turn = negate(turn)
        moving = false
      case _ =>
        if (moving) {
          message = pce + " moved"
          turn = negate(turn)
        } else {
          message = "Move Blocked. Try again."
        }
        moving = false
    }
    moving
  }

  def doneMoving(pce:Piece)={
    message = pce + " moved"
    turn = negate(turn)
    moving = false
  }
  val divider = "  +---+---+---+---+---+---+---+---+"
  val letters = "abcdefgh".map("   " + _).reduce(_+_)
  def show(): Unit = {
    println()
    println(letters)
    for (i <- 7 to 0 by -1) {
      println(divider)
      print((i+1) + " |")
      for (j <- 0 to 7) {
        print(board(i)(j))
        print("|")
      }
      print(i match {
        case 7 => "  White Captures: " + whiteCaps.mkString(", ") + "\n"
        case 5 => "  Black Captures: " + blackCaps.mkString(", ") + "\n"
        case 3 => "  Current Turn: " + turn + "\n"
        case 1 => "  " + message + "\n"
        case _ => "\n"
      })
    }
    println(divider)
  }
}

object Assign6{
  import Color._
  import Rank._
  import Utils._

  def parseDir(xy:String):Option[Coord] ={
    (xy:Seq[Char]) match {
      case Seq(x,y) => Some(x-'a', y - '0' -1)
      case _ => None
    }
  }

  val dirTable = Map(
    "N"  -> (0,1),
    "S"  -> (0,-1),
    "E"  -> (1,0),
    "W"  -> (-1,0)
  )

  def dirCoord(dir: Seq[Char]):Coord =
    dir.map((x)=>dirTable.get(x.toString).get).reduce(_+_)

  val straight = dirTable.keySet
  val diag = Set("NE","NW","SE", "SW")
  val allowedMoves= Map(
    King -> straight.union(diag),
    Queen -> straight.union(diag),
    Rook -> straight,
    Knight -> Set("NNE","NNW","EEN","EES","SSE","SSW","WWN","WWS"),
    Bishop -> diag
  )
  val maxDist = Map(
    King -> 1,
    Queen -> 8,
    Rook -> 8,
    Knight -> 1,
    Bishop -> 8
  )

  def pawnCheck(coord:Coord, board: Board, color: Color) = {
    var set = Set[String]()
    var dist = 1
    if (color == White){
      board.select(coord+(0,1)).collect{case Space() => set += "N"}
      board.select(coord+(-1,1)).collect{case Piece(_, Black) => set += "NW"}
      board.select(coord+(1,1)).collect{case Piece(_, Black) => set += "NE"}
      if(coord.y == 1) board.select(coord+(0,2)).collect{
        case p:Space => dist = 2
      }
    }
    if (color == Black){
      board.select(coord+(0,-1)).collect{case Space() => set += "S"}
      board.select(coord+(-1,-1)).collect{case Piece(_, White) => set += "SW"}
      board.select(coord+(1,-1)).collect{case Piece(_, White) => set += "SE"}
      if(coord.y == 6) board.select(coord+(0,-2)).collect{
        case p:Space => dist = 2
      }
    }
    (set, dist)
  }


  val ranks = List(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
  val defaultBoard = new Board(List(
  mutable.MutableList(ranks.map((x)=>Piece(x,White)) :_*),
    mutable.MutableList.fill(8)(Piece(Pawn,White)),
    mutable.MutableList.fill(8)(Space()),
    mutable.MutableList.fill(8)(Space()),
    mutable.MutableList.fill(8)(Space()),
    mutable.MutableList.fill(8)(Space()),
    mutable.MutableList.fill(8)(Piece(Pawn,Black)),
    mutable.MutableList(ranks.map((x)=>Piece(x,Black)) :_*)
  ))

  def main(args: Array[String]): Unit = {
    main2(defaultBoard)
  }
  def main2(board:Board){
    board.show()
    var args = StdIn.readLine().split(' ')
    if (args.length == 2) {args = Array(args(0), args(1), "8")}
    if (args.length == 3) {
      val coord = parseDir(args(0)).getOrElse((-1,-1))
      val sqr = board.select(coord).getOrElse(Space())
      val dir = args(1)
      val num = args(2).tryToInt
      (sqr, num) match {
        case (pce@Piece(rank, color), Some(dist)) =>
          val (pawnMoves, pawnDist) = pawnCheck(coord, board, color)
          val aMoves = allowedMoves.getOrElse(rank, pawnMoves)
          if (color != board.turn) board.message = "It's not your turn, " +color
          else if(!aMoves.contains(dir)) board.message = pce + "can't move like that!"
          else{
            val rDist = dist.min(maxDist.getOrElse(rank,pawnDist))
            val dc = dirCoord(dir)
            def move(i:Int):Unit={
              val curr = coord + (dc * i)
              val moved = board.eatOrOccupy(curr, curr + dc)
              if (moved && i<rDist-1) move(i+1)
              else if (moved)board.doneMoving(pce)
            }
            move(0)
          }
        case _ => board.message = "Input error!"
      }
    }

    main2(board)
  }
}