#include <stdio.h>

int main()
{
	// The program computes the sum of all divisors
	// result 1: sum of all divisors of 914 = 1374
	// result 2: sum of all divisors of 10551314 = 15826974
	

	int A = 0;
	int F = 836;
	int flags = 78;
	F += flags; 

	if (A == 1)
	{
		flags = 10550400;
		F += flags;
		A = 0;
	}

	int C = 0;
	int E = 0;

	C = 1;


	while (C <= F)
	{
		E = 1;
		while (E <= F)
		{
			if (C * E == F)
				A += C;
			E++;
		}

		C++;
	}

	printf("A = %d\n", A);
	return 0;
}
