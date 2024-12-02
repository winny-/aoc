package main

import (
	"fmt"
	"io"
	"slices"
	"cmp"
)

func main() {
	var as, bs []uint
	for {
		var a, b uint;
		_, err := fmt.Scanln(&a, &b)
		as = append(as, a)
		bs = append(bs, b)
		if err != nil {
			if err == io.EOF {
				break;
			}
			panic(err)
		}
	}
	
	part1, part2 := uint(0), uint(0)
	slices.SortFunc(as, cmp.Compare)
	slices.SortFunc(bs, cmp.Compare)
	for i := range as {
		val := (int)(as[i]) - (int)(bs[i])
		if val < 0 {
			val = -val
		}
		part1 += uint(val)
	}
	for _, a := range as {
		for _, b := range bs {
			if a == b {
				part2 += a
			}
		}
	}
	
	fmt.Printf("%d\n%d\n", part1, part2)
}
