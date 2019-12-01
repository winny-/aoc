#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <openssl/md5.h>

#define ENOUGH ((CHAR_BIT * sizeof(int) - 1) / 3 + 2)
#define PREFIX "00000"
#define PASSWORD_LENGTH 8

char *
str2md5(const char *str, int length) {
    int n;
    MD5_CTX c;
    unsigned char digest[16];
    char *out = (char*)malloc(33);

    MD5_Init(&c);

    while (length > 0) {
        if (length > 512) {
            MD5_Update(&c, str, 512);
        } else {
            MD5_Update(&c, str, length);
        }
        length -= 512;
        str += 512;
    }

    MD5_Final(digest, &c);

    for (n = 0; n < 16; ++n) {
        snprintf(&(out[n*2]), 16*2, "%02x", (unsigned int)digest[n]);
    }

    return out;
}

int
interesting(char *s)
{
	return strncmp(s, PREFIX, strlen(PREFIX)) == 0 ? 1 : 0;
}

char *
hash(char *doorid, int n)
{
	char ns[ENOUGH];
	sprintf(ns, "%d", n);
#if DEBUG
	if (n % 1000000 == 0) {
		fprintf(stderr, "\r%d million", n / 1000000);
		fflush(stderr);
	}
#endif
	char *s = malloc(strlen(doorid) + strlen(ns) + 1);
	strncpy(s, doorid, strlen(doorid));
	strncat(s, ns, strlen(ns));
	char* ret = str2md5(s, strlen(s));
	free(s);
	return ret;
}

char *
part1(char *doorid)
{
	unsigned int n = 0;
	char *password = malloc(PASSWORD_LENGTH + 1);
	for (unsigned int i = 0; i < PASSWORD_LENGTH; i++) {
		for (;;) {
			char *s = hash(doorid, n++);
			if (interesting(s)) {
#if DEBUG
				fprintf(stderr, "\rPart 1: Found i:%d n:%d s:%s\n", i, n, s);
#endif
				password[i] = s[5];
				free(s);
				break;
			}
			free(s);
		}
	}
	password[PASSWORD_LENGTH] = '\0';
	return password;
}

char *
part2(char *doorid)
{
	unsigned int n = 0;
	char *password = calloc(PASSWORD_LENGTH + 1, sizeof(char));
	while (strlen(password) < PASSWORD_LENGTH) {
		char *s = hash(doorid, n++);
		if (interesting(s)) {
			char pos = s[5];
			if (pos < '0' || pos > '7') {
				free(s);
				continue;
			}
			int idx = (int)(pos - '0');
			if (password[idx] != '\0') {
				free(s);
				continue;
			}
#if DEBUG
			fprintf(stderr, "\rPart 2: Found n:%d s:%s\n", n, s);
#endif
			password[idx] = s[6];
			free(s);
		}
	}
	return password;
}

int
main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s door-id\n", argv[0]);
		return 1;
	}
	char* p1password = part1(argv[1]);
	puts(p1password);
	free(p1password);
	char* p2password = part2(argv[1]);
	puts(p2password);
	free(p2password);
}
