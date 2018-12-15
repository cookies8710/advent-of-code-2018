#include <stdio.h>
#include <stdlib.h>

/*void realloc(char** a, int *allo)
{
	
}
*/

//char ending[] = {9,8,5,1,5};// inverted target
//char ending[] = {4,1,4,9,5};// inverted target, 2018
char ending[] = {1,0,2,7,7,0};// inverted target, result 2

void next(char **rec, int *allo, int *used, char **f, char **s)
{
	if (*used + 2 > *allo)
	{
		int fo = *f - *rec;
		int so = *s - *rec;
		*allo *= 2;
		*rec = realloc(*rec, sizeof(char)* (*allo));
		*f = *rec + fo;
		*s = *rec + so;
	}


	int nr = **f + **s;
	if (nr > 9) 
		(*rec)[(*used)++] = nr / 10;

	(*rec)[(*used)++] = nr % 10;

	*f += **f + 1;
	while (*f - *rec >= *used) *f -= *used;
	*s += **s + 1;
	while (*s - *rec >= *used) *s -= *used;
}

void render(char *rec, int used, char *f, char *s)
{
	printf("size: %d\n", used);
	for(char *i = rec; i - rec < used; i++)
	{
		if (i == f) printf("(%d)", *i);
		else if (i == s) printf("[%d]", *i);
		else printf(" %d ", *i);
	}
}

int main(void)
{
	char *rec = (char*)malloc(sizeof(char)*2);
	int allo = 2;
	rec[0] = 3;
	rec[1] = 7;
	int used = 2;
	char *f = rec;
	char *s = rec + 1;
	int found = 0;
	int i = 0;

	while (!found)
	{
		if (used % 1000000 < 2)
			printf("in progress, %d\n", used);
		next(&rec, &allo, &used, &f, &s);
		int f1 =1; int j = 0; 
		char *e = rec + used - 1;
		for (; j < 5; e--,j++)
			if (ending[j] != *e)
				f1 = 0;
		
		int f2 = 1;
		j = 0; 
		e = rec + used - 2;
		for (; j < 5; e--,j++)
			if (ending[j] != *e)
				f2 = 0;

		found = f1 || f2;
		i++;
	}

	for (int k = 0; k <= sizeof(ending); k++)
	{
		int idx = used - sizeof(ending) + k - 1;
		printf("%d: %d\n", idx, rec[idx]);
	}


	free(rec);

	return 0;
}
