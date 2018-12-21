#include <stdio.h>
#include <stdlib.h>

int contains(int *ptr, int len, int n)
{
	for(int *i = ptr; i < ptr + len; i++)
		if (*i == n) return 1;
	return 0;	
}

int main() 
{
	int allo = 100000;
	int used = 0;
	int *unique = malloc(sizeof(int)*allo);

	int A = 0, B = 123, C, D, F;
	A = 2525738; // result 1
	A = 0;
	do
		B &= 456;
	while (B != 72);

	B = 0;
	do
	{
		D = B | 65536;
		B = 6780005;
		
g8:
		C = D & 255;
		B += C;
		//B &= 16777215; // not necessary
		B *= 65899;
		B &= 16777215;

		if (D < 256)
		{
			//	part 1: - the first that gets printed
		//	printf("could halt\n");
		//	printf("%d\n",B);

			// part 2 - the valu that will be printed as the last one (11698 th new value) - 11316540
			if (!contains(unique, used, B))
			{
				printf("new value (%d th): %d\n", used + 1,B);
				unique[used++] = B;
			}
		}

		if (D < 256 && A == B)
			break;

		if (D > 255)
		{
			C = 0;
			do 
			{
				F = C + 1;
				F *= 256;
				if (F <= D)
					C++;
			}
			while(F <= D);
			D = C;
			goto g8;
		}
	}
	while(A != B);

	printf("Halt.\n");
}
