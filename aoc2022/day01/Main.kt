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

/* find the common item (= badge) in the rucksack of three elfs */
tailrec fun findCommonBadges(badges: MutableList<Char>, s: List<String>): MutableList<Char> {
    return if (s.isEmpty()) {
        badges
    } else {
        val (elf1, elf2, elf3) = s.take(3)
        // common chars in elf1 and elf2
        val oneTwo = elf1.partition { c -> elf2.contains(c, false) }.first
        // common chars in elf2 and elf3
        val twoThree = elf2.partition { c -> elf3.contains(c, false) }.first
        // the char in all elfs
        val badge = (oneTwo.toList() intersect twoThree.toList().toSet()).first()
        badges.add(badge)

        findCommonBadges(badges, s.drop(3))
    }
}

/* find the common item in each rucksack (= line) */
fun findCommonItems(s: List<String>): List<Char> {
    return s.fold((mutableListOf())) { items, line ->
        val half = line.length / 2 // guaranteed by the input to be an integer
        val firstCompartment = line.subSequence(0, half)
        val secondCompartment = line.subSequence(half, line.length)
        // first part of the pair contains everything fulfilling the predicate, second the rest
        val duplicates = firstCompartment.partition { c -> secondCompartment.contains(c, false) }.first
        val dupItem = duplicates.toSet().first() // we only care for the item, not how often
        items.add(dupItem)

        items
    }
}

/* double items in the rucksack */
fun day03() {
    val input = File("input/input03.txt").readLines()

    printDay(
        3,
        findCommonItems(input).sumOf(::getPriority),
        findCommonBadges(mutableListOf(), input).sumOf(::getPriority)
    )
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