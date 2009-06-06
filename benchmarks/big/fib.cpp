/*

fib.cpp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

*/

#include <iostream>
using namespace std;

const int N = 10;

void print_array(int array[N]);
unsigned int fib(unsigned int n);

int main() {
    int A[N], B[N];
    int i, j;
    int a, b, result;

    for (i = 0; i < N; i++) {
        a = 1;
        b = 0;

        for (j = 0; j < i; j++) {
            result = a + b;
            a = b;
            b = result;
        }

        A[i] = fib(i);
        B[i] = result;
    }

    print_array(A);
    cout << endl;
    print_array(B);
}

void print_array(int array[N]) {
    int i;

    for (i = 0; i < N; i++) {
        printf("%d ", array[i]);
    }
}


// The familiar recursive definition of the Fibonacci function provided as a proof
// that the non-recursive implementation works correctly.
unsigned int fib(unsigned int n) {
    if (n == 0 || n == 1)
        return n;
    
    return fib(n - 1) + fib(n - 2);
}
