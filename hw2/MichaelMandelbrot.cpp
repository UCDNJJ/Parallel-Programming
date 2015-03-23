// hw2.cpp
#include <windows.h>
#include <stdio.h>

#include <ctime>
#include <iostream>

#include <complex>
#include <cmath>
#include <omp.h>



void findmyrange(int n, int nth, int me, int *myrange)
{
	int chunksize = n / nth;
	myrange[0] = me * chunksize;
	if (me < nth - 1)
		myrange[1] = (me + 1) * chunksize - 1;
	else
		myrange[1] = n - 1;
}

int inset(std::complex<double> c, int maxiters)
{
	int iters;
	float rl, im;
	std::complex<double> z = c;
	for (iters = 0; iters < maxiters; iters++)
	{
		z = z*z + c;
		rl = std::real(z);
		im = std::imag(z);
		if (rl*rl + im*im > 4)
			return 0;
	}
	return 1;
}

void rmandel(int nth, double xl, double xr, double yb, double yt, double inc, int maxiters, int sched, int chunksize)
{
	int nptssideX = ((xr - xl) / inc);
	float side2x = nptssideX / 2.0;
	float side4x = nptssideX / 4.0;

	int nptssideY = ((yt - yb) / inc);
	float side2y = nptssideY / 2.0;
	float side4y = nptssideY / 4.0;

	std::complex<double> I = sqrt(-1);
#pragma omp parallel
	int x, y; double xv, yv;
	 std::complex<double> z;
	 int me = omp_get_thread_num();

#pragma omp for schedule(static, 2)
	 for (x = 0; x < nptssideX; x++)
	 {
#pragma omp for schedule(static, 2)
		 for (y = 0; y < nptssideY; y++)
		 {
			 xv = (x - side2x) / side4x;
			 yv = (y - side2y) / side4y;
			 z = xv + yv * I;
			 if (inset(z, maxiters))
			 {
				 std::cout << me << "| " << x << " " << y << " " << xv << " " << xv << "\n";
			 }
		 }
	 }
}


int main(int argc, char **argv)
{

	rmandel(4, 3, 4, 2, 2.4, 0.2, 100, 1, 25);
	int wait;

	std::cin >> wait;

	return 0;
}