
extern int printf(const char *, ...);

enum DEFINE {
 TRUE = 1,
 FALSE = 0,

/* This is the number of "disks" on tower A initially. Taken to be 64 in the
 * legend. The number of moves required, in general, is 2^N - 1. For N = 64,
 * this is 18,446,744,073,709,551,615 */
  N = 4
};

/* These are the three towers. For example if the state of A is 0,1,3,4, that
 * means that there are three discs on A of sizes 1, 3, and 4. (Think of right
 * as being the "down" direction.) */
int A[N], B[N], C[N];

void Hanoi(int,int*,int*,int*);

/* Print the current configuration of A, B, and C to the screen */
void PrintAll()
{
   int i;

   printf("A: ");
   for(i=0;i<N;i++)printf(" %d ",A[i]);
   printf("\n");

   printf("B: ");
   for(i=0;i<N;i++)printf(" %d ",B[i]);
   printf("\n");

   printf("C: ");
   for(i=0;i<N;i++)printf(" %d ",C[i]);
   printf("\n");
   printf("------------------------------------------\n");
   return;
}

/* Move the leftmost nonzero element of source to dest, leave behind 0. */
/* Returns the value moved (not used.) */
int Move(int *source, int *dest)
{
   int i = 0, j = 0;

   while (i<N && (source[i])==0) i++;
   while (j<N && (dest[j])==0) j++;

   dest[j-1] = source[i];
   source[i] = 0;
   PrintAll();       /* Print configuration after each move. */
   return dest[j-1];
}


/* Moves first n nonzero numbers from source to dest using the rules of Hanoi.
   Calls itself recursively.
   */
void Hanoi(int n,int *source, int *dest, int *spare)
{
   int i;
   if(n==1){
      Move(source,dest);
      return;
   }

   Hanoi(n-1,source,spare,dest);
   Move(source,dest);
   Hanoi(n-1,spare,dest,source);
   return;
}

int main()
{
   int i;

   /* initialize the towers */
   for(i=0;i<N;i++)A[i]=i+1;
   for(i=0;i<N;i++)B[i]=0;
   for(i=0;i<N;i++)C[i]=0;

   printf("Solution of Tower of Hanoi Problem with %d Disks\n\n",N);

   /* Print the starting state */
   printf("Starting state:\n");
   PrintAll();
   printf("\n\nSubsequent states:\n\n");

   /* Do it! Use A = Source, B = Destination, C = Spare */
   Hanoi(N,A,B,C);

   return 0;
}
