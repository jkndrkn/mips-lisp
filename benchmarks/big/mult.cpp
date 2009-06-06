#include <iostream>
using namespace std;

const int N = 4;

void print_array(int array[][N]);

int main() {
    int i, j, temp, blah = 0;
    int A[N][N];
    int B[N][N];
    int C[N][N];
   
    for (i = 0; i < N; i++) {
	    for (j = 0; j < N; j++) {
	        A[i][j] = i;
	        B[i][j] = j;
	    }
    }
    
    for (i = 0; i < N; i++) {
	    for (j = 0; j < N; j++) {
	        cout << endl << endl << "start comp" << endl;
	        for (temp = 0; temp < N; temp++) {
	    	    blah = blah + (A[i][temp]*B[temp][j]);
	    	    cout << "A[" << i << "," << temp << "] * B["
	    	         << temp << " , " << j << "]" << endl;
	        }

	        C[i][j] = blah;
	        cout << "ASSIGNED TO C[" << i << "," << j << "]" << endl;
	    }
    }

    cout << endl;
    print_array(A);

    cout << endl;
    print_array(B);

    cout << endl;
    print_array(C);

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
