package gameoflife

object GameOfLife extends App {

  type Pos = (Int, Int)
  type Board = List[Pos]

  val pulsar: Board = List(
    (4,2),(5,2),(6,2),(10,2),(11,2),(12,2),
    (2,4),(7,4),(9,4),(14,4),
    (2,5),(7,5),(9,5),(14,5),
    (2,6),(7,6),(9,6),(14,6),
    (4,7),(5,7),(6,7),(10,7),(11,7),(12,7),
    (4,9),(5,9),(6,9),(10,9),(11,9),(12,9),
    (2,10),(7,10),(9,10),(14,10),
    (2,11),(7,11),(9,11),(14,11),
    (2,12),(7,12),(9,12),(14,12),
    (4,14),(5,14),(6,14),(10,14),(11,14),(12,14),
  )

  import cats.effect.IO
  import cats.implicits._

  val main: IO[Unit] = life(pulsar)

  def life(b: Board): IO[Unit] = 
    cls *> 
    showCells(b) *> 
    goto(width+1,height+1) *> 
    wait(1_000_000) >>
    life(nextgen(b))

  /////////////////////////////////////////////////////////

  def cls: IO[Unit] =
    putStr("\u001B[2J")

  def showCells(b: Board): IO[Unit] =
    ( for { p <- b } yield writeAt(p, "O") ).sequence_

  def wait(n:Int): IO[Unit] =
    List.fill(n)(IO.unit).sequence_

  /////////////////////////////////////////////////////////

  def goto(p: Pos): IO[Unit] = p match {
    case (x,y) => putStr(s"\u001B[${y};${x}H")}

  def writeAt(p: Pos, s: String): IO[Unit] =
    goto(p) *> putStr(s)

  def putStr(s: String): IO[Unit] =
    IO { scala.Predef.print(s) }

  /////////////////////////////////////////////////////////

  def nextgen(b: Board): Board = 
    survivors(b) ++ births(b)

  def survivors(b: Board): List[Pos] =
    for {
      p <- b
      if List(2,3) contains liveneighbs(b)(p)
    } yield p

  def births(b: Board): List[Pos] =
    for {
      p <- rmdups(b flatMap neighbs)
      if isEmpty(b)(p)
      if liveneighbs(b)(p) == 3
    } yield p

  /////////////////////////////////////////////////////////

  def rmdups[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x::xs => x::rmdups(xs filter(_ != x))
  }

  def isEmpty(b: Board)(p: Pos): Boolean =
    !(isAlive(b)(p))

  def liveneighbs(b: Board)(p: Pos): Int =
    neighbs(p).filter(isAlive(b)).length

  def isAlive(b: Board)(p: Pos): Boolean =
    b contains p

  /////////////////////////////////////////////////////////

  def neighbs(p: Pos): List[Pos] = p match {
    case (x,y) => List(
      (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
      (x - 1, y    ),  /* cell */ (x + 1, y    ),
      (x - 1, y + 1), (x,     y + 1), (x + 1, y + 1)
    ) map wrap }

  def wrap(p: Pos): Pos = p match {
    case (x, y) => (((x - 1) % width) + 1,
                    ((y - 1) % height) + 1)
  }

  /////////////////////////////////////////////////////////

  lazy val width = 20
  lazy val height = 20

  main.unsafeRunSync

}
