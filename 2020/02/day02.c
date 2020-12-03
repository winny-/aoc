/*
 * stderr should resemble stdin except with the validity tuple appended afterwards like "= y,n"
 *
 * I haven't written C in a long time... I'm not missing out ;)... But it was fun.
 */

#include <stdio.h>
#include <string.h>

/* Via https://stackoverflow.com/questions/5459868/concatenate-int-to-string-using-c-preprocessor */
#define STR_HELPER(x) #x
#define MY_STR(x) STR_HELPER(x)

int main() {
	unsigned int count1 = 0;
	unsigned int count2 = 0;
	for (;;) {
		char unique = 0;
		char least, most, c;
#define PASSWORD_MAX_LENGTH 100
		char password[PASSWORD_MAX_LENGTH];
		*password = '\0';

		if (scanf("%hhd-%hhd %c: %"MY_STR(PASSWORD_MAX_LENGTH)"s", &least, &most, &c, password) != 4) {
			break;
		}
		fprintf(stderr, "%d-%d %c: %s = ", least, most, c, password);

		/* a XOR b, in 0/1 binary means only true when the sum is 1. */
		unique = ((password[least-1] == c) + (password[most-1] == c));

		char frequency = 0;
		for (size_t idx = 0; idx < strlen(password); idx++) {
			if (password[idx] == c) {
				frequency++;
			}
			if (frequency > most) {
				break;
			}
		}
		if (frequency >= least && frequency <= most) {
			fprintf(stderr, "y,");
			count1++;
		} else {
			fprintf(stderr, "n,");
		}
		if (unique == 1) {
			fprintf(stderr, "y\n");
			count2++;
		} else {
			fprintf(stderr, "n\n");
		}
	}
	printf("%u\n%u\n", count1, count2);
	return 0;
}

/* Local Variables: */
/* compile-command: "cc -g -o day02 day02.c && ./day02 < input.txt" */
/* End: */
