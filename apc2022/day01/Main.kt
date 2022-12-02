import java.io.File

data class Elf(val id: Int, val calories: Int)

fun main() {
    day01()
}

fun day01() {
    val elfs = mutableListOf<Elf>()
    var elfId = 1
    var elfCalories = 0

    File("input/input.txt").forEachLine {
        if (it.isNotEmpty()) {
            elfCalories += it.toInt()
        } else {
            elfs.add(Elf(elfId, elfCalories))
            elfId += 1
            elfCalories = 0
        }
    }

    val solution1: Int = elfs.maxBy { it.calories }.calories
    val solution2: Int = -1

    printDay(1, solution1, solution2)
}

fun printDay(day: Int, sol1: Int, sol2: Int) {
    println("Day $day\n------")
    println("Solution 1: $sol1")
    println("Solution 2: $sol2")
}