
#define N 1000


double a[N];
double b[N];

void daxpy()
{
  int i, j;
  double s=2.71828;

  for ( j=0; j<N; j++)
	a[j] = a[j] + b[j]*s;
}


void daxpy1(double a[], double b[])
{
  int i,j;
  double s=2.71828;

  for (j=0; j < N; j++) {
        a[j] = a[j] + b[j]*s;
  }

}

