/*

list-reverse.cpp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

*/

#include <iostream>
using namespace std;

const int N = 10;

void print_array(int array[N]);

int main() {
    int A[N], B[N];
    int i;
    int tmp;

    for (i = 0; i < N; i++) {
        A[i] = i;
    }

    for (i = 0; i < N; i++) {
        B[N - 1 - i] = A[i];
    }

    cout << "A:" << endl;
    print_array(A);
    cout << endl << endl;
    cout << "B:" << endl;
    print_array(B);
}

void print_array(int array[N]) {
    int i;

    for (i = 0; i < N; i++) {
        printf("%d ", array[i]);
    }
}
