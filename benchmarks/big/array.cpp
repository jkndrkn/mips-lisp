#include <iostream>
using namespace std;

const int N = 5;

void print_array(int array[N]);

int main() {
	int i;
	int A[N], B[N], C[N], D[N], E[N];

	for(i = 0; i < N; i++){
		A[i] = i;
		B[i] = i;
		C[i] = i;
		D[i] = i;
		E[i] = i;
	}

	for(i = 0; i < N; i++){
		C[i] = A[i] + B[i];
	
	}

	for(i = 0; i < N; i++){
		D[i] = C[i] + E[i];
	}

	for(i = 0; i < N; i++){
		A[i] = A[i] + B[i];
	}

    cout << "A:" << endl;
    print_array(A);
    cout << endl;

    cout << "B:" << endl;
    print_array(B);
    cout << endl;

    cout << "C:" << endl;
    print_array(C);
    cout << endl;

    cout << "D:" << endl;
    print_array(D);
    cout << endl;

    cout << "E:" << endl;
    print_array(E);
    cout << endl;

    return 0;
}
 
void print_array(int array[N]) {
    int i = 0;

    for (i; i < N; i++) {
        printf("%d: %d\n", i, array[i]);
    }
}
