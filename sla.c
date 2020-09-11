#include <stdio.h>

void foo(int *x) {
    *x = 666;
}

int main() {
    int *f ;
    *f = 7;
    foo(f);
    printf("%i\n", *f);
}
