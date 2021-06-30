#define GC_DEBUG
#include <gc.h>

void *malloc(size_t size){
    return GC_MALLOC(size);
}
void free(void* ptr){
    GC_FREE(ptr);
}
