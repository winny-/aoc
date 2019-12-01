/*
 * By winny. Implements http://adventofcode.com/day/11 .
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void
increment(char *s, size_t len)
{
  if (len < 1) {
    return;
  }
  for (size_t idx = len - 1; ; idx--) {
    if (s[idx] == 'z') {
      s[idx] = 'a';
    } else {
      s[idx] += 1;
      break;
    }
  }
}

short
pairs(char *s, size_t len)
{
  if (len < 2) {
    return 0;
  }
  short n = 0;
  for (size_t idx = 1; idx < len; idx++) {
    if (s[idx-1] == s[idx]) {
      n++;
      idx++;
    }
  }
  return n;
}

// Returns 0 if no straights, 1 if there are straights.
short
straight(char *s, size_t len)
{
  if (len < 3) {
    return 0;
  }
  for (size_t idx = 2; idx < len; idx++) {
    char c = s[idx-2];
    if (c == s[idx-1]-1 && c == s[idx]-2) {
      return 1;
    }
  }
  return 0;
}

void
next(char *s, size_t len)
{
  do {
    char *firstp = NULL;
    char *ptrs[] = {memchr(s, 's', len),
                    memchr(s, 'l', len),
                    memchr(s, 'o', len)};
    for (short i = 0; i < 3; i++) {
      if (ptrs[i] != NULL &&
          (firstp == NULL || ptrs[i] < firstp)) {
        firstp = ptrs[i];
      }
    }

    if (firstp != NULL) {
      size_t bad_len = (size_t)(firstp-s)+1;
      increment(s, bad_len);
      for (size_t idx = bad_len; idx < len; idx++) {
        s[idx] = 'a';
      }
    } else {
      increment(s, len);
    }
  } while (pairs(s, len) < 2 || !straight(s, len));
}

int
main(void)
{
  for (;;) {
    printf("Input old password: ");

    char *old = NULL;
    size_t unused;
    ssize_t size;
    size = getline(&old, &unused, stdin);

    if (size == -1) {
      printf("EOF. Exiting.\n");
      free(old);
      return 0;
    } else {
      if (old[size-1] == '\n') {
        old[size-1] = '\0';
        size--;
      }

      char* new = malloc(sizeof(char)*size+1);
      strncpy(new, old, size);

      next(new, size);

      printf("Old password \"%s\" becomes \"%s\".\n", old, new);
      free(new);
    }

    free(old);
  }
}
