// Wrapper functions for Fortran compatibility
#include <GL/gl.h>
#include <string.h>

// Forward declarations from gpu_opengl_reference.c
extern int gpu_initialize_opengl();
extern void gpu_cleanup_opengl();

// Wrapper functions
int gpu_init() {
    return gpu_initialize_opengl();
}

void gpu_cleanup() {
    gpu_cleanup_opengl();
}

void gpu_get_info(char* vendor, char* renderer, char* version) {
    if (!gpu_init()) return;
    
    const GLubyte* v = glGetString(GL_VENDOR);
    const GLubyte* r = glGetString(GL_RENDERER);
    const GLubyte* ver = glGetString(GL_VERSION);
    
    if (v) strcpy(vendor, (const char*)v);
    if (r) strcpy(renderer, (const char*)r);
    if (ver) strcpy(version, (const char*)ver);
}

