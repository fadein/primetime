#include <stdio.h>
#include "stdint.h"

void factor_time(uintmax_t t);

int
main(void) {

	factor_time( (unsigned int)1412200537 );
	printf("  Should output 11 128381867\n");
	return 0;
}
