#include <iostream>
using namespace std;

const int N = 10;

void print_array(int array[N]);

int main() {
    int N=10;
    int i;
    int A[N];
    
    for(i = 0; i < N; i++){
	    A[i] = i;
    }

    cout << "A before:" << endl;
    print_array(A);
    cout << endl;
    
    for(i = 0; i < N; i++){
    	A[i] = A[i] + 1;
    }

    cout << "A after:" << endl;
    print_array(A);
    cout << endl;
}
 
void print_array(int array[N]) {
    int i = 0;

    for (i; i < N; i++) {
        printf("%d: %d\n", i, array[i]);
    }
}
