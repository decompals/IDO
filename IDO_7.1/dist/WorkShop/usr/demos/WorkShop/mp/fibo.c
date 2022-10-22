#include <stdio.h>
#include <sys/types.h>
#include <sys/prctl.h>

int 	NumberToCompute = 100;
int 	fibonacci();
void	run(), run1();

int fibonacci(int n)
{
int f, f_minus_1, f_plus_1;
int i;

    f = 1;
    f_minus_1 = 0;

    i = 0;

    for (; ;) {
        if (i++ == n)  return f;

        f_plus_1 = f + f_minus_1;
        f_minus_1 = f;
        f = f_plus_1;
    }
}

void run()
{
int fibon;
    for (; ;) {
	NumberToCompute = (NumberToCompute + 1) % 10;
        fibon = fibonacci(NumberToCompute);
    	printf("%d'th fibonacci number is %d\n", 
               NumberToCompute, fibon);
    }
    
}


void run1()
{
int grandChild;

    grandChild = sproc( (void (*)(void *))run, PR_SADDR);
    if (grandChild == -1) {
	perror("SPROC GRANDCHILD");
    }
    else
	printf("grandchild is %d\n", grandChild);

    run();
}



void main ()
{
int second;

    second = sproc( (void (*)(void *))run1, PR_SADDR);
    if (second == -1)
	perror("SPROC CHILD");
    else
	printf("child is %d\n", second);

    run();
    exit(0);
}
