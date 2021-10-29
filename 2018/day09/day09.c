#include <stdio.h>
#include <stdlib.h>

struct game_spec {
	unsigned int players, last_marble;
};

struct state {
	struct game_spec *spec;
	unsigned int next_turn, next_marble;
	unsigned int *scores;
	struct marble *ring;
};

struct marble {
	struct marble *clockwise, *counterclockwise;
	unsigned int value;
};

struct marble *remove_marble(struct marble *marble) {
	struct marble *result = NULL;
	if (marble->clockwise != marble && marble->counterclockwise != marble) {
		marble->counterclockwise->clockwise = result = marble->clockwise;
		marble->clockwise->counterclockwise = marble->counterclockwise;
	}
	free(marble);
	return result;
}

void print_simple_state(struct state *state) {
	if (state->next_turn == 1) {
		printf("[-] ");
	} else {
		printf("[%u] ", state->next_turn);
	}
	struct marble *ptr = state->ring;
	do {
		printf("%u ", ptr->value);
		ptr = ptr->clockwise;
	} while (ptr != state->ring);
	puts("");
}

int scan_game_spec(FILE *file, struct game_spec **spec) {
	unsigned int players;
	unsigned int points;
	if (fscanf(file, "%u players; last marble is worth %u points\n", &players, &points) != 2) {
		return 0;
	}
	*spec = (struct game_spec *)malloc(sizeof(struct game_spec));
	(*spec)->players = players;
	(*spec)->last_marble = points;
	return 1;
}

int play_turn(struct state *state) {
	unsigned int current_value = state->next_marble;
	unsigned int current_turn = state->next_turn;
	/* printf("current turn: %u\n", current_turn); */
	if (current_value > 0 && current_value % 23 == 0) {
		state->scores[current_turn] += current_value;
		struct marble *ptr = state->ring;
		for (size_t i = 0; i < 7; i++) {
			ptr = ptr->counterclockwise;
		}
		state->scores[current_turn] += ptr->value;
		state->ring = remove_marble(ptr);
	} else {
		struct marble *counterclockwise = state->ring->clockwise;
		struct marble *new = malloc(sizeof(struct marble));
		new->counterclockwise = counterclockwise;
		new->clockwise = counterclockwise->clockwise;
		new->value = current_value;
		counterclockwise->clockwise->counterclockwise = new;
		counterclockwise->clockwise = new;
		state->ring = new;
	}
	state->next_marble++;
	state->next_turn = (state->next_turn < state->spec->players - 1) ? state->next_turn + 1 : 0;
	if (state->spec->last_marble < current_value) {
		return 0;
	}
	return 1;
}

int main(int argc, char **argv) {
	struct game_spec *spec;
	if (!scan_game_spec(stdin, &spec)) {
        	printf("No spec.\n");
		return 1;
	}
	/* printf("Read in: %u players; last marble is worth %u points.\n", spec->players, spec->last_marble); */

	struct state *state = malloc(sizeof(struct state));
	state->scores = calloc(spec->players, sizeof(unsigned int));
	state->spec = spec;
	state->next_turn = 0;
	state->next_marble = 1;
	struct marble *start = state->ring = malloc(sizeof(struct marble));
	start->clockwise = start->counterclockwise = start;
	start->value = 0;

	while (play_turn(state)) { /*print_simple_state(state);*/ }

	size_t winning_elf = 0;
	unsigned int winning_score = 0;
	for (size_t idx = 0; idx < state->spec->players; idx++) {
		if (state->scores[idx] > winning_score) {
			winning_elf = idx;
			winning_score = state->scores[idx];
		}
	}

	printf("%u\n", winning_score);

	struct marble *marble = state->ring;
	while (marble) {
		marble = remove_marble(marble);
	}
	free(state->scores);
	free(state->spec);
	free(state);

	return 0;
}

/* Local Variables: */
/* compile-command: "cc -std=c11 -Wall -Wextra -o day09 day09.c && valgrind ./day09 < input.txt" */
/* End: */
