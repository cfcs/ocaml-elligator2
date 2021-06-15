#include <sys/types.h>
#include <sys/stat.h>
#include <sys/random.h>
#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <monocypher.h>


/*
 * Outputs a file with 97-byte records in this format:
 */

typedef struct {
	uint8_t in[32]; /* random input */
	uint8_t out[32]; /* hidden_to_curve(in) */
	uint8_t recovered[32]; /* curve_to_hidden(out, _ , tweak) */
	uint8_t tweak; /* random input to curve_to_hidden */
} __attribute__((packed)) testcase_t;

int
main(int argc, char **argv)
{
	testcase_t o = { 0 };
	FILE *fh = NULL;

	assert(argc == 3);

	if(0 == strcmp("gen", argv[1])) {
		size_t i = 0;
		fh = fopen(argv[2], "w");
		assert(NULL != fh);

		do {
			assert(0 == getentropy(o.in, sizeof(o.in)));
			assert(0 == getentropy(&o.tweak, sizeof(o.tweak)));
			crypto_hidden_to_curve(o.out, o.in);
			assert(0 == crypto_curve_to_hidden(o.recovered, o.out, o.tweak));
			fwrite(&o, sizeof(o), 1, fh);
		} while(++i < 100);
		fflush(fh);
	} else if (0 == strcmp("check", argv[1])) {
		typeof(o.out) out_cmp = { 0 };
		typeof(o.recovered) recovered_cmp = { 0 };
		size_t file_recs = 0 ;
		struct stat statbuf;

		fh = fopen(argv[2], "r");
		assert (NULL != fh);
		fstat(fileno(fh), &statbuf);
		file_recs = statbuf.st_size / sizeof(o);
		while(1 == fread(&o, sizeof(o), 1, fh)) {
			crypto_hidden_to_curve(out_cmp, o.in);
			assert(0 == memcmp(out_cmp, o.out, sizeof(out_cmp)));
			assert(0 == crypto_curve_to_hidden(recovered_cmp, o.out, o.tweak));
			assert(0 == memcmp(recovered_cmp, o.recovered, sizeof(recovered_cmp)));
		}
		if (ferror(fh) || !feof(fh)) return 1;
		if (ftell(fh) != file_recs * sizeof(o)) return 2;
	}
	fclose(fh);
	return 0;
}
