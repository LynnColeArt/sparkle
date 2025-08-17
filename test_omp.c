#include <omp.h>
#include <stdio.h>
int main() {
    omp_set_num_threads(1);
    #pragma omp parallel
    {
        int tid = omp_get_thread_num();
        int nthreads = omp_get_num_threads();
        if (tid == 0) {
            printf("num_threads=%d\\n", nthreads);
        }
    }
    return 0;
}
