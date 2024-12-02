#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>


#define BEGIN_SIZE sizeof(unsigned int)*2*32

static int cmpuintp(const void *a, const void *b) {
	return *((unsigned int *)a) - *((unsigned int *)b);
}

int main(int argc, char** argv) {
	size_t max_size = BEGIN_SIZE;
	unsigned int *a1 = malloc(max_size);
	unsigned int *a2 = malloc(max_size);
	size_t lines = 0;
	unsigned int a, b;
	while (scanf("%u %u\n", &a, &b) == 2) {
		if (sizeof(unsigned int)*lines >= max_size) {
			max_size *= 2;
			a1 = realloc(a1, max_size);
			a2 = realloc(a2, max_size);
		}
		a1[lines] = a;
		a2[lines] = b;
		lines++;
	}
	qsort(a1, lines, sizeof(unsigned int), cmpuintp);
	qsort(a2, lines, sizeof(unsigned int), cmpuintp);
	unsigned int part1 = 0, part2 = 0;
	for (size_t i = 0; i < lines; i++) {
		unsigned int left = a1[i];
		unsigned int right = a2[i];
		part1 += abs((int)left - (int)right);
		for (size_t j = 0; j < lines; j++) {
			if (left == a2[j]) {
				part2 += left;
			}
		}
	}
	printf("%u\n%u\n", part1, part2);
	free(a1);
	free(a2);
	return 0;
}
