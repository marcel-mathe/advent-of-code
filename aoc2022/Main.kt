import com.scalified.tree.TraversalAction
import com.scalified.tree.TreeNode
import com.scalified.tree.multinode.LinkedMultiTreeNode
import java.io.File

fun main() {
    day08()
}

/* treetop tree house */

// transpose a matrix
// thx to: https://stackoverflow.com/questions/70230712/generic-transpose-or-anything-else-really-in-kotlin
inline fun <reified T> transpose(xs: List<List<T>>): List<List<T>> {
    val cols = xs[0].size
    val rows = xs.size
    return List(cols) { j ->
        List(rows) { i ->
            xs[i][j]
        }
    }
}

fun day08() {
    val grid = File("input/input08.txt").readLines().map { line -> line.toList().map { it.digitToInt() } }

    // all neighbours for tree (row, col) in a given grid
    fun neighbours(row: Int, col: Int, grid: List<List<Int>>): List<List<Int>> {
        // no neighbours on first and last column
        if (col == 0) return emptyList()
        if (col == (grid.size - 1)) return emptyList()

        // no neighbours on first and last row
        if (row == 0) return emptyList()
        if (row == (grid.size - 1)) return emptyList()

        // we actually have something to do
        val gridT = transpose(grid)

        // we need to have the list as seen from the tree,
        // but we build ot from the edge
        val north = gridT[col].subList(0, row).reversed()
        val east = grid[row].subList(col + 1, grid.size)
        val south = gridT[col].subList(row + 1, grid.size)
        // same as above
        val west = grid[row].subList(0, col).reversed()

        return listOf(north, east, south, west)
    }

    // is a tree with a height of myValue visible with those neighbours
    fun isVisible(neighbours: List<List<Int>>, myValue: Int): Boolean {
        // we stand on one the sides, always visible
        if (neighbours.isEmpty()) return true

        return neighbours.any { it.all { v -> (v < myValue) } }
    }

    // compute the viewing distance in one direction from a tree
    fun viewingDistance(trees: List<Int>, tree: Int): Int {
         tailrec fun views(t: List<Int>, count: Int = 0): Int {
            return if (t.isEmpty()) {
                count
            } else if (t.first() >= tree) {
                count + 1
            } else {
                views(t.drop(1), count + 1)
            }
        }
        return views(trees)
    }

    // multiply integer, woah
    fun multiply(a: Int, b: Int): Int {
        return a * b
    }

    // compute the scenic score of a tree
    fun scenicScore(neighbours: List<List<Int>>, myValue: Int): Int {
        if (neighbours.isEmpty()) return 0

        return neighbours.map { viewingDistance(it, myValue) }.reduce(::multiply)
    }

    // sum of all visible Trees
    val visibleTrees =
        grid.flatMapIndexed { rowIndex, row ->
            row.mapIndexed { colIndex, tree ->
                isVisible(neighbours(rowIndex, colIndex, grid), tree)
            }
        }.count { it }

    val bestTreeValue =
        grid.flatMapIndexed { rowIndex, row ->
            row.mapIndexed { colIndex, tree ->
                scenicScore(neighbours(rowIndex, colIndex, grid), tree)
            }
        }.max()

    printDay(8, visibleTrees, bestTreeValue)
}

/* no space left on device */
enum class Filetype { DIRECTORY, FILE }
data class DirEntry(val name: String, val size: Int, val type: Filetype)

fun day07() {
    tailrec fun modifyTree(fs: TreeNode<DirEntry>, curDir: String, cmdHistory: List<String>): TreeNode<DirEntry> {
        return if (cmdHistory.isEmpty()) {
            fs
        } else {
            val line = cmdHistory.first()

            // change directory
            if (line.startsWith("$ cd")) {
                val (_, _, dir) = line.split(" ")

                when (dir) {
                    // go to root
                    "/" -> {
                        return modifyTree(fs, "/", cmdHistory.drop(1))
                    }

                    // go to parent
                    ".." -> {
                        val parent = fs.find { it.data().name == curDir }?.parent()?.data()?.name

                        return if (parent.isNullOrBlank()) {
                            // we can't .. over /, so we just stay in it
                            if (curDir == "/") {
                                modifyTree(fs, curDir, cmdHistory.drop(1))
                            } else {
                                throw Exception("Parent not found")
                            }
                        } else {
                            modifyTree(fs, parent, cmdHistory.drop(1))
                        }
                    }

                    // go to target
                    else -> {
                        val target = fs.find { it.data().name == dir }?.data()?.name

                        return if (target.isNullOrBlank()) {
                            throw Exception("target not found")
                        } else {
                            modifyTree(fs, target, cmdHistory.drop(1))
                        }
                    }
                }
            }

            // dir output
            if (line.startsWith("dir")) {
                val (_, name) = line.split(" ")
                val node = LinkedMultiTreeNode(DirEntry(name, -1, Filetype.DIRECTORY))
                val newFs = fs.clone()
                val whereToAdd = newFs.find { it.data().name == curDir }

                whereToAdd?.add(node)

                return modifyTree(newFs, curDir, cmdHistory.drop(1))
            }

            // file output
            if (line.matches(Regex("^\\d+\\s.+"))) {
                val (size, name) = line.split(" ")
                val node = LinkedMultiTreeNode(DirEntry(name, size.toInt(), Filetype.FILE))
                val newFs = fs.clone()
                val whereToAdd = newFs.find { it.data().name == curDir }

                whereToAdd?.add(node)

                return modifyTree(newFs, curDir, cmdHistory.drop(1))
            }

            // ignore everything else
            modifyTree(fs, curDir, cmdHistory.drop(1))
        }
    }

    /* total size of a dir entry is its own size and (if any) of its descendants */
    fun totalSize(t: TreeNode<DirEntry>): Int {
        return when (t.data().type) {
            Filetype.FILE -> t.data().size
            Filetype.DIRECTORY -> t.subtrees().sumOf { totalSize(it) }
        }
    }

    val listOfDirectorySizes = mutableMapOf<String, Int>()

    /* what to do when we traverse the TreeNode */
    val action: TraversalAction<TreeNode<DirEntry>> = object : TraversalAction<TreeNode<DirEntry>> {
        override fun perform(node: TreeNode<DirEntry>) {
            if (node.data().type == Filetype.DIRECTORY)
            // we have to return Unit, so I think this is the way(?)
                listOfDirectorySizes[node.data().name] = totalSize(node)
        }

        override fun isCompleted(): Boolean {
            return false // return true in order to stop traversing
        }
    }

    var root: TreeNode<DirEntry> = LinkedMultiTreeNode(DirEntry("/", -1, Filetype.DIRECTORY))
    val input = File("input/input07.txt").readLines()

    root = modifyTree(root, "/", input)
    root.subtrees().map { it.traversePostOrder(action) }

    TODO("die answer is to low")
    printDay(7,
        listOfDirectorySizes.filterValues { it <= 100_000 }.entries.sumOf { it.value },
        -1
    )
}

/* tuning trouble */
fun day06() {
    val sopWindowSize = 4
    val somWindowSize = 14

    File("input/input06.txt").forEachLine { line ->
        val startOfPacket =
            line.windowed(sopWindowSize, 1).indexOfFirst { it.toSet().size == sopWindowSize } + sopWindowSize
        val startOfMessage =
            line.windowed(somWindowSize, 1).indexOfFirst { it.toSet().size == somWindowSize } + somWindowSize

        printDay(6, startOfPacket, startOfMessage)
    }
}

/* supply stacks */
fun day05() {
    /* example stack and starting state */
    val exampleStack = mapOf(
        1 to ArrayDeque<Char>(),
        2 to ArrayDeque(),
        3 to ArrayDeque()
    )

    exampleStack[1]?.addAll(listOf('N', 'Z'))
    exampleStack[2]?.addAll(listOf('D', 'C', 'M'))
    exampleStack[3]?.add('P')

    /* real input stack and starting state, Cheater McCheater */
    val realStack = mapOf(
        1 to ArrayDeque<Char>(),
        2 to ArrayDeque(),
        3 to ArrayDeque(),
        4 to ArrayDeque(),
        5 to ArrayDeque(),
        6 to ArrayDeque(),
        7 to ArrayDeque(),
        8 to ArrayDeque(),
        9 to ArrayDeque()
    )

    realStack[1]?.addAll(listOf('T', 'F', 'V', 'Z', 'C', 'W', 'S', 'Q'))
    realStack[2]?.addAll(listOf('B', 'R', 'Q'))
    realStack[3]?.addAll(listOf('S', 'M', 'P', 'Q', 'T', 'Z', 'B'))
    realStack[4]?.addAll(listOf('H', 'Q', 'R', 'F', 'V', 'D'))
    realStack[5]?.addAll(listOf('P', 'T', 'S', 'B', 'D', 'L', 'G', 'J'))
    realStack[6]?.addAll(listOf('Z', 'T', 'R', 'W'))
    realStack[7]?.addAll(listOf('J', 'R', 'F', 'S', 'N', 'M', 'Q', 'H'))
    realStack[8]?.addAll(listOf('W', 'H', 'F', 'N', 'R'))
    realStack[9]?.addAll(listOf('B', 'R', 'P', 'Q', 'T', 'Z', 'J'))

    tailrec fun rearrange9000(
        stack: Map<Int, ArrayDeque<Char>>,
        howOften: Int,
        fromWhere: Int,
        toWhere: Int
    ): Map<Int, ArrayDeque<Char>> {
        return if (howOften <= 0) {
            stack
        } else {
            val tmp = stack[fromWhere]?.removeFirst() as Char
            stack[toWhere]?.addFirst(tmp)

            rearrange9000(stack, howOften - 1, fromWhere, toWhere)
        }
    }

    tailrec fun rearrange9001(
        stack: Map<Int, ArrayDeque<Char>>,
        howOften: Int,
        fromWhere: Int,
        toWhere: Int
    ): Map<Int, ArrayDeque<Char>> {
        return if (howOften <= 0) {
            stack
        } else {
            val tmpStack = ArrayDeque<Char>()

            // remove from stack and save in temporary stack
            for (i in 1..howOften) {
                val tmp = stack[fromWhere]?.removeFirst() as Char
                tmpStack.addFirst(tmp)
            }

            // add items in correct order back onto stack
            for (item in tmpStack) {
                stack[toWhere]?.addFirst(item)
            }

            // loop de loop
            rearrange9001(stack, 0, fromWhere, toWhere)
        }
    }

    val sol = File("input/input05.txt").readLines().drop(10).fold(realStack) { stack, line ->
        val splitLine = line.split(Regex("\\s"), 6)
        val howOften = splitLine[1].toInt()
        val fromWhere = splitLine[3].toInt()
        val toWhere = splitLine[5].toInt()

        rearrange9001(stack, howOften, fromWhere, toWhere)
    }
        .values
        .fold("") { acc, stack -> acc.plus(stack.first()) }

    val ex = File("input/ex05.txt").readLines().drop(5).fold(exampleStack) { stack, line ->
        val splitLine = line.split(Regex("\\s"), 6)
        val howOften = splitLine[1].toInt()
        val fromWhere = splitLine[3].toInt()
        val toWhere = splitLine[5].toInt()

        rearrange9000(stack, howOften, fromWhere, toWhere)
    }
        .values
        .fold("") { acc, stack -> acc.plus(stack.first()) }

    println("Day 05:")
    println("-------")
    println("example:  $ex")
    println("Solution: $sol")
}

/* camp cleanup */
fun day04() {
    /* convert line into a pair of ranges */
    fun convertToRange(input: String): Pair<IntRange, IntRange> {
        val (f, s) = input.split(",")
        val (f1, f2) = f.split("-")
        val (s1, s2) = s.split("-")
        val first = (f1.toInt()).rangeTo(f2.toInt())
        val second = (s1.toInt()).rangeTo(s2.toInt())
        return Pair(first, second)
    }

    /* 1 if full contained, 0 otherwise */
    fun fullyContains(ranges: Pair<IntRange, IntRange>): Int {
        val b1 = ranges.first.all { i -> ranges.second.contains(i) }
        val b2 = ranges.second.all { i -> ranges.first.contains(i) }

        return if (b1 || b2) 1 else 0
    }

    /* 1 if any overlap, 0 otherwise */
    fun overlaps(ranges: Pair<IntRange, IntRange>): Int {
        val b1 = ranges.first.any { i -> ranges.second.contains(i) }
        val b2 = ranges.second.any { i -> ranges.first.contains(i) }

        return if (b1 || b2) 1 else 0
    }

    // count the pairs
    File("input/input04.txt").readLines().let {
        printDay(
            4,
            it.sumOf { i -> fullyContains(convertToRange(i)) },
            it.sumOf { i -> overlaps(convertToRange(i)) })
    }
}

/* double items in the rucksack */
fun day03() {
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

    File("input/input03.txt").readLines().let {
        printDay(
            3,
            findCommonItems(it).sumOf(::getPriority),
            findCommonBadges(mutableListOf(), it).sumOf(::getPriority)
        )
    }
}

/* rock paper scissors */
const val WIN = 6
const val DRAW = 3
const val LOSE = 0

enum class Shape { A, B, C, X, Y, Z }

fun day02() {
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

    fun ultraTopSecretRoundScore(opponentMove: String, playerMove: String, secret: Boolean): Int {
        val opponentShape = Shape.valueOf(opponentMove)
        var playerShape = Shape.valueOf(playerMove)

        if (secret) {
            playerShape = getUltraTopSecretPlayerShape(opponentShape, playerShape)
        }

        return shapeScore(playerShape) + roundOutcome(Round(opponentShape, playerShape))
    }

    var totalScore = 0
    var ultraTopSecretScore = 0

    File("input/input02.txt").forEachLine {
        val moves = it.split(" ")
        totalScore += ultraTopSecretRoundScore(moves[0], moves[1], false)
        ultraTopSecretScore += ultraTopSecretRoundScore(moves[0], moves[1], true)
    }

    printDay(2, totalScore, ultraTopSecretScore)
}

/* count the calories each elf carries,
 * return the max value (solution 01),
 * return the 3 max values (solution 02)
*/
fun day01() {
    data class Elf(val id: Int, val calories: Int)

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

/* quick helper function, just print the damn solutions */
fun printDay(day: Int, sol1: Int, sol2: Int) {
    println("Day $day\n------")
    println("Solution 1: $sol1")
    println("Solution 2: $sol2")
}