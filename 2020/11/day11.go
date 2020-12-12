// This was sort of a pain.  Golang compile-time semantics are surprisingly
// lax, it may as well be C in this regard.  It pushes a great deal of memory
// checking semantics to runtime, leading to panics and trivial errors only
// caught by running the program.
package main

import (
	"os"
	"fmt"
	"bufio"
	"log"
)

func max(a, b int) int {
	if a < b {
		return b
	} else {
		return a
	}
}

func min(a, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}

func dump(grid[][]rune)  {
	for _, r := range grid {
		for _, c := range r {
			fmt.Printf("%c", c)
		}
		fmt.Println()
	}
}

func neighbors(grid[][]rune, y, x int) []rune {
	ret := []rune{}
	for i := max(0, y-1); i < min(len(grid), y+2); i++ {
		for j := max(0, x-1); j < min(len(grid[0]), x+2); j++ {
			if i == y && j == x {
				continue
			}
			// log.Printf("%d,%d %d,%d", y, x, i, j)
			ret = append(ret, grid[i][j])
		}
	}
	return ret
}

func visible(grid[][]rune, y, x int) []rune {
	// log.Print("Visible")
	ret := []rune{}
	directions := [][]int {
		{-1, 0},  {1, 0},
		{0, -1},  {0, 1},
		{-1, -1}, {1, 1},
		{-1, 1},  {1, -1},
	}

next_direction:
	for _, dir := range directions {
		// log.Printf("dir = {%d, %d}", dir[0], dir[1])
		i, j := y+dir[0], x+dir[1]
		for i >= 0 && j >= 0 && i < len(grid) && j < len(grid[0]) {
			// log.Printf("grid[%d][%d] = '%c'", i, j, grid[i][j])
			if grid[i][j] != '.' {
				ret = append(ret, grid[i][j])
				continue next_direction
			}
			i, j = i+dir[0], j+dir[1]
		}
	}
	return ret
}

func equal2D(a [][]rune, b [][]rune) bool {
	for i, x := range a {
		for j, y := range x {
			if y != b[i][j] {
				return false
			}
		}
	}
	return true
}

func copy2D(a [][]rune) [][]rune {
	ret := make([][]rune, len(a))
	for i, r := range a {
		ret[i] = make([]rune, len(r))
		copy(ret[i], r)
	}
	return ret
}

func simulate(seed [][]rune, f func([][]rune, int, int) []rune, tolerance int) int {
	next := copy2D(seed)
	var grid [][]rune
	var step int
	// dump(grid)
	// dump(next)
	for step = 0; (grid == nil || !equal2D(grid, next)); step++ {
		// log.Print(step)
		// dump(grid)
		// fmt.Println("-----")
		grid = copy2D(next)

		// dump(next)
		for i, row := range grid {
			// log.Printf("i %d", i)
			
		next_cell:
			for j, c := range row {
				// log.Printf("j %d", j)
				switch (c) {
				case 'L':
					for _, n := range f(grid, i, j) {
						// log.Printf("%d,%d %d %c", i, j, len(ne),  n)
						if (n == '#') {
							continue next_cell
						}
					}
					next[i][j] = '#'
					break
				case '#':
					count := 0
					for _, n := range f(grid, i, j) {
						if (n == '#') {
							count++
						}
					}
					if count >= tolerance {
						next[i][j] = 'L'
					}
					break
				}
			}
		}
	}
	occupied := 0
	for _, r := range next {
		for _, c := range r {
			if c == '#' {
				occupied++
			}
		}
	}
	return occupied
}

func main() {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Println(err)
	}

	height := len(lines)
	width := len(lines[0])

	seed := make([][]rune, height)
	for i := range seed {
		seed[i] = make([]rune, width)
	}

	for i, line := range lines {
		for j, c := range line {
			seed[i][j] = c
		}
	}

	fmt.Println(simulate(seed, neighbors, 4))
	fmt.Println(simulate(seed, visible, 5))
}

// Local Variables:
// compile-command: "go run day11.go < input.txt"
// End:
