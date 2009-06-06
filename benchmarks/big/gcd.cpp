/* gcd.c */

#include <iostream>
using namespace std;

int main() {
    
    int x = 8;
    int y = 6;
    
    while (x != y) {
        
        if (x > y) {
            x = x - y;
        }
        else {
            y = y - x;
        }
    }
    cout << "gcd is " << x << endl;

    return 0;
}
