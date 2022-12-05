import java.io.File

fun main() {
    day03()
}

/* map char to priority */
fun getPriority(c: Char): Int {
    return if (c.isLowerCase()) {
        c.code - 96
    } else {
        c.code - 38
    }
}

/* double items in the rucksack */
fun day03() {
    val prio = mutableListOf<Int>()

    File("input/input03.txt").forEachLine {
        val half = it.length / 2 // guaranteed by the input to be a integer
        val firstCompartment = it.subSequence(0, half)
        val secondCompartment = it.subSequence(half, it.length)
        // first part of the pair contains everything fulfilling the predicate, second the rest
        val duplicates = firstCompartment.partition { c -> secondCompartment.contains(c, false) }.first
        val dupChar = duplicates.toSet().first() // we only care for the item, not how often
        prio.add(getPriority(dupChar))
    }

    printDay(3, prio.sum(), -1)
}

const val WIN = 6
const val DRAW = 3
const val LOSE = 0

enum class Shape { A, B, C, X, Y, Z }
data class Round(val opponent: Shape, val player: Shape)

fun shapeScore(s: Shape): Int {
    return when (s) {
        Shape.A, Shape.X -> 1
        Shape.B, Shape.Y -> 2
        Shape.C, Shape.Z -> 3
    }
}

fun roundOutcome(r: Round): Int {
    // i lose
    if (Round(Shape.A, Shape.Z) == r)
        return LOSE
    if (Round(Shape.B, Shape.X) == r)
        return LOSE
    if (Round(Shape.C, Shape.Y) == r)
        return LOSE

    // i win
    if (Round(Shape.A, Shape.Y) == r)
        return WIN
    if (Round(Shape.B, Shape.Z) == r)
        return WIN
    if (Round(Shape.C, Shape.X) == r)
        return WIN

    return DRAW
}

fun ultraTopSecretRoundScore(opponentMove: String, playerMove: String, secret: Boolean): Int {
    val opponentShape = Shape.valueOf(opponentMove)
    var playerShape = Shape.valueOf(playerMove)

    if (secret) {
        playerShape = getUltraTopSecretPlayerShape(opponentShape, playerShape)
    }

    return shapeScore(playerShape) + roundOutcome(Round(opponentShape, playerShape))
}

fun getUltraTopSecretPlayerShape(opponent: Shape, player: Shape): Shape {
    // i loose
    if (Shape.X == player)
        return when (opponent) {
            Shape.A -> Shape.Z
            Shape.B -> Shape.X
            Shape.C -> Shape.Y
            else -> throw Exception("Whoops")
        }
    // draw
    if (Shape.Y == player)
        return opponent
    // i win
    return when (opponent) {
        Shape.A -> Shape.Y
        Shape.B -> Shape.Z
        Shape.C -> Shape.X
        else -> throw Exception("Whoops")
    }
}

/* rock paper scissors */
fun day02() {
    var totalScore = 0
    var ultraTopSecretScore = 0

    File("input/input02.txt").forEachLine {
        val moves = it.split(" ")
        totalScore += ultraTopSecretRoundScore(moves[0], moves[1], false)
        ultraTopSecretScore += ultraTopSecretRoundScore(moves[0], moves[1], true)
    }

    printDay(2, totalScore, ultraTopSecretScore)
}

data class Elf(val id: Int, val calories: Int)

/* count the calories each elf carries,
 * return the max value (solution 01),
 * return the 3 max values (solution 02)
*/
fun day01() {
    val elfs = mutableListOf<Elf>()
    var elfId = 1
    var elfCalories = 0

    File("input/input01.txt").forEachLine {
        if (it.isNotEmpty()) {
            elfCalories += it.toInt()
        } else {
            elfs.add(Elf(elfId, elfCalories))
            elfId += 1
            elfCalories = 0
        }
    }

    printDay(1,
        elfs.maxBy { it.calories }.calories,
        elfs.sortedBy { it.calories }.takeLast(3).sumOf { it.calories })
}

// quick helper function, just print the damn solutions
fun printDay(day: Int, sol1: Int, sol2: Int) {
    println("Day $day\n------")
    println("Solution 1: $sol1")
    println("Solution 2: $sol2")
}