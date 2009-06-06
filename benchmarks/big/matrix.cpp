#include <iostream>
using namespace std;

const int N = 5;

void print_array(int array[][N]);

int main() {
    int i, j;
    int A[N][N], B[N][N];
    int loop_bound = N - 1;

    for (i = 0; i <= loop_bound; i++) {
	    for (j = 0; j <= loop_bound; j++) {
	        A[i][j] = i;
	        B[i][j] = j;
	    }
    }

    cout << "A before:" << endl;
    print_array(A);

    cout << "B before:" << endl;
    print_array(B);
    
    for (i = 1; i < loop_bound; i++) {
	    for (j = 1; j < loop_bound; j++) {
	        B[i][j] = (A[i - 1][j] + A[i + 1][j] + A[i][j + 1] + A[i][j - 1]) / 4;
	    } 
    }
 
    cout << "A after:" << endl;
    print_array(A);

    cout << "B after:" << endl;
    print_array(B);

    return 0;
}

void print_array(int array[][N]) {
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            printf("%d ", array[i][j]);
        }
        cout << endl;
    }
}
