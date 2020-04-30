import scala.collection.mutable.ListBuffer
import scala.util.Random
import util.control.Breaks._

sealed trait Direction {
  def name: String
}

case object North extends Direction {
  val name = "north"
}

case object East extends Direction {
  val name = "east"
}

case object South extends Direction {
  val name = "south"
}

case object West extends Direction {
  val name = "west"
}

case class GridCoord(var row: Int, var col: Int) {

  def isEmpty: Boolean = row < 0 || col < 0


  def move(direction: Direction): Unit = {
    direction match {
      case East => col = col + 1
      case West => col = col - 1
      case North => row = row - 1
      case South => row = row + 1
    }
  }
}

case class WordLocation(word: String, startCoord: GridCoord, endCoord: GridCoord, direction: Direction)


class GridObj(numberGameRows: Int, numberGameColumns: Int) {

  type Grid = Array[Array[Char]]
  val matrix = Array.ofDim[Char](numberGameRows, numberGameColumns)
//  val numberGameRows = rows
//  val numberGameColumns = columns

  /*

     */
  def blankGrid(): Unit = {
    for (i <- 0 until numberGameRows; j <- 0 until numberGameColumns) {
      matrix(i)(j) = '-'
    }
  }

  def dumpGrid(): Unit = {
    println("=== Game ===")

    for (i <- 0 until numberGameRows) {
      for (j <- 0 until numberGameColumns) {
        val letter = matrix(i)(j)
        print(s"$letter")
      }
      println("")
    }
  }

  def randomLowercaseLetter(): Char = {
    //    val low = 65 // A
    //    val high = 90 // Z
    val low = 97 // a
    val high = 122 // z
    (Random.nextInt(high - low) + low).toChar
  }

  def fillEmptyCellsWithRandomLetters(): Unit = {

    for (row <- 0 until numberGameRows; column <- 0 until numberGameColumns) {
      if (matrix(row)(column) == '-') {
        val letter = randomLowercaseLetter()
        matrix(row)(column) = letter
      }
    }
  }

  /*


   */
  def isCellOutOfBounds(gridCoord: GridCoord): Boolean = {
    if (gridCoord.row < 0 || gridCoord.col < 0 || gridCoord.row >= numberGameRows
      || gridCoord.col >= numberGameColumns) {
      return true
    }
    false
  }

  def isCellFree(letter: Char, location: GridCoord): Boolean = {

    val gridChar = matrix(location.row)(location.col)

    // Overlap the same letter is ok
    val isCellFree = (gridChar == letter) || (gridChar == '-') //? true : false
    isCellFree
  }

  def calcLastCellForWord(wordLen: Int, startCoord: GridCoord, direction: Direction): GridCoord = {

    val wordLength0 = wordLen - 1

    direction match {
      case East => GridCoord(startCoord.row, startCoord.col + wordLength0)
      case West => GridCoord(startCoord.row, startCoord.col - wordLength0)
      case North => GridCoord(startCoord.row - wordLength0, startCoord.col)
      case South => GridCoord(startCoord.row + wordLength0, startCoord.col)
    }
  }

  def isDirFree(letters: String, gridCoord: GridCoord, direction: Direction): Boolean = {

    val currentLocation = gridCoord.copy()

    for (letter <- letters) {
      if (!isCellFree(letter, currentLocation)) {
        return false
      }
      currentLocation.move(direction)
    }
    true
  }

  def placeWord(wordLocation: WordLocation): Unit = {
    val word = wordLocation.word
//    val srow = wordLocation.startCoord.row
//    val scol = wordLocation.startCoord.col

    //    println(s"placing $word @ row=$srow, col=$scol)")

    for (letter <- word) {
      val row = wordLocation.startCoord.row
      val col = wordLocation.startCoord.col

      matrix(row)(col) = letter
      wordLocation.startCoord.move(wordLocation.direction)
    }
    println("")

  }

  def canPlaceWordAtCoord(letters: String, coord: GridCoord, direction: Direction): (Boolean, GridCoord) = {
    val wordLen = letters.length
    val endGridCoord = calcLastCellForWord(wordLen, coord, direction)
    if (isCellOutOfBounds(endGridCoord)) {
      return (false, GridCoord(-1, -1))
    }
    val canPlace = isDirFree(letters, coord, direction)

    (canPlace, endGridCoord)
  }

  def placeWordGivenDetails(letters: String, gridCoord: GridCoord, direction: Direction): (Boolean, GridCoord) = {

    val (isFreeSpot, endCoord) = canPlaceWordAtCoord(letters, gridCoord, direction)

    (isFreeSpot, endCoord)
  }

  def placeWordGivenDirection(word: String, direction: Direction): WordLocation = {

    val r = scala.util.Random
    var randomRow = r.nextInt(numberGameRows)
    var randomColumn = r.nextInt(numberGameColumns)

    for (_ <- 0 until numberGameRows) {
      for (_ <- 0 until numberGameColumns) {
        val testLocation = GridCoord(randomRow, randomColumn)
        val (isFreeSpot, endCoord) = canPlaceWordAtCoord(word, testLocation, direction)
        if (isFreeSpot) {
          println(s"Placing $word: $direction - ${testLocation.row}, ${testLocation.col}")

          val wordLocation = WordLocation(word, testLocation, endCoord, direction)
          placeWord(wordLocation)
          dumpGrid()
          return wordLocation

        }
        randomColumn = (randomColumn + 1) % numberGameColumns
      }
      randomRow = (randomRow + 1) % numberGameRows

    }
    val noCoord = GridCoord(1, -1)
    val noSolution = WordLocation("", noCoord, noCoord, North)

    noSolution
  }

  def generateGame(wordList: List[String], directions: List[Direction]): Unit = {
    var placedWords = ListBuffer[WordLocation]()

    for (word <- wordList) {
      var isWordPlaced = false
      val randomDirections = scala.util.Random.shuffle(directions)

      println(s"=== Placing $word  ===")
      breakable {
        for (direction <- randomDirections) {
          val wordLocation = placeWordGivenDirection(word, direction)
          if (wordLocation.startCoord.row >= 0) {
            isWordPlaced = true
            placedWords += wordLocation
            break
          }
        }
      }
    }
  }

}


object WordSearchGenerator {
  println("===== Grid =====")

  val grid = new GridObj(10, 10)
  val letter = grid.randomLowercaseLetter()
  println(s"randomLetter=$letter")

  grid.blankGrid()

  //  grid.dumpGrid()
  grid.generateGame(List("apple", "orange", "arc", "bird", "animal"), List(East, South))
  grid.dumpGrid()
  grid.fillEmptyCellsWithRandomLetters()
  grid.dumpGrid()

  println(s"matrix=$grid.matrix")
}

WordSearchGenerator
