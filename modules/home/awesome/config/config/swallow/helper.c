#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

pid_t getparentpid(int child) {
	unsigned int v = 0;
	FILE *f;
	char buf[256];
	snprintf(buf, sizeof(buf) - 1, "/proc/%u/stat", (unsigned)child);

	if (!(f = fopen(buf, "r")))
		return 0;

	fscanf(f, "%*u %*s %*c %u", &v);
	fclose(f);

	return (pid_t)v;
}

int isdescprocess(pid_t p, pid_t c) {
	while (p != c && c != 0) {
		c = getparentpid(c);
	}

	return (int)c;
}

int main(int argc, char* argv[]) {

	printf("%d\n", isdescprocess((unsigned int)atoi(argv[1]), (unsigned int)atoi(argv[2])));

}
