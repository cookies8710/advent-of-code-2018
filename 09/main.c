#include <stdio.h>
#include <stdlib.h>

#define PLAYERS 458
#define LAST 7130700
//#define PLAYERS 9
//#define LAST 25

typedef struct Node
{
    struct Node *prev;
    struct Node *next;
    int value;
} Node;


unsigned int scores[PLAYERS];


Node *create(int value)
{
    Node *new = malloc(sizeof(Node));
    new->value = value;
    return new;
}

Node *jump(Node* current, int delta)
{
    if (delta == 0) 
        return current;

    if (delta > 0)
        return jump(current->next, delta - 1);
    else
        return jump(current->prev, delta + 1);
}

// Will insert a new node accordingly
// Returns new current node
Node *insert(Node *current, int player, int value)
{
    Node* new = create(value);
    if (!current)
    {
        new->next = new->prev = new;
        return new;
    }
    
    Node *a, *b;
    if (value % 23)
    {
        a = jump(current, 1);
        b = jump(a, 1);

        new->prev = a;
        new->next = b;
        b->prev = new;
        a->next = new;
        
        return new;
    }
    else
    {
        scores[player] += value;
        Node* rm = jump(current, -7);
        scores[player] += rm->value;

        a = jump(rm, -1);
        b = jump(rm, 1);
        a->next = b;
        b->prev = a;

        free(rm);

        return b;
    }
}

void printChain(Node* current)
{
    Node* node = current;
    do
    {
        printf("%d-", node->value);
        node = node->next;
    }
    while(node != current);
    
    printf("\n");
}

void cleanup(Node** current)
{
    (*current)->prev->next = NULL;
    Node *rm = *current;
    Node *ptr = *current;
    
    while (ptr)
    {
	rm = ptr;
	ptr = ptr->next;
	free(rm);
    }

    *current = NULL;
}

int main(void) 
{
    printf("Starting\n");
    
    for(int i = 0; i < PLAYERS; i++)
        scores[i] = 0;

    Node *current = NULL;
    for(int i = 0; i <= LAST; i++)
        current = insert(current, i % PLAYERS, i);

    cleanup(&current);

    long max = 0;
    for(int i = 0; i < PLAYERS; i++)
        if (scores[i] > max)
            max = scores[i];

    printf("Done. Max score: %ld\n", max);
    return 0;
}
