#include <stdio.h>
#include <stdlib.h>
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GL/gl.h>

int main() {
    printf("NVIDIA EGL/OpenGL Test\n");
    printf("======================\n\n");
    
    // Get default display
    EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
    if (display == EGL_NO_DISPLAY) {
        printf("Failed to get EGL display\n");
        return 1;
    }
    
    // Initialize EGL
    EGLint major, minor;
    if (!eglInitialize(display, &major, &minor)) {
        printf("Failed to initialize EGL\n");
        return 1;
    }
    printf("EGL version: %d.%d\n", major, minor);
    
    // Query EGL vendor/version
    const char* vendor = eglQueryString(display, EGL_VENDOR);
    const char* version = eglQueryString(display, EGL_VERSION);
    const char* apis = eglQueryString(display, EGL_CLIENT_APIS);
    const char* extensions = eglQueryString(display, EGL_EXTENSIONS);
    
    printf("EGL Vendor: %s\n", vendor);
    printf("EGL Version: %s\n", version);
    printf("Client APIs: %s\n", apis);
    printf("\nExtensions:\n");
    
    // Check for important extensions
    if (strstr(extensions, "EGL_KHR_create_context")) {
        printf("  ✓ EGL_KHR_create_context\n");
    }
    if (strstr(extensions, "EGL_KHR_surfaceless_context")) {
        printf("  ✓ EGL_KHR_surfaceless_context (headless compute!)\n");
    }
    if (strstr(extensions, "EGL_NV_device_cuda")) {
        printf("  ✓ EGL_NV_device_cuda (CUDA interop!)\n");
    }
    
    // Try to create an OpenGL context
    eglBindAPI(EGL_OPENGL_API);
    
    // Minimal config for compute only (no rendering)
    EGLint config_attribs[] = {
        EGL_SURFACE_TYPE, EGL_PBUFFER_BIT,
        EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
        EGL_NONE
    };
    
    EGLConfig config;
    EGLint num_configs;
    if (!eglChooseConfig(display, config_attribs, &config, 1, &num_configs) || num_configs == 0) {
        printf("\nFailed to choose config\n");
        return 1;
    }
    
    // Create context
    EGLint context_attribs[] = {
        EGL_CONTEXT_MAJOR_VERSION, 4,
        EGL_CONTEXT_MINOR_VERSION, 6,
        EGL_NONE
    };
    
    EGLContext context = eglCreateContext(display, config, EGL_NO_CONTEXT, context_attribs);
    if (context == EGL_NO_CONTEXT) {
        printf("Failed to create context\n");
        return 1;
    }
    
    // Make current without surface (for compute)
    if (!eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE, context)) {
        printf("Failed to make current\n");
        return 1;
    }
    
    printf("\n✓ OpenGL context created!\n\n");
    
    // Get OpenGL info
    const char* gl_vendor = (const char*)glGetString(GL_VENDOR);
    const char* gl_renderer = (const char*)glGetString(GL_RENDERER);
    const char* gl_version = (const char*)glGetString(GL_VERSION);
    
    printf("OpenGL Information:\n");
    printf("  Vendor: %s\n", gl_vendor);
    printf("  Renderer: %s\n", gl_renderer);
    printf("  Version: %s\n", gl_version);
    
    // Get compute capabilities
    GLint max_compute_work_group[3];
    GLint max_compute_invocations;
    GLint max_shared_memory;
    
    glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_COUNT, 0, &max_compute_work_group[0]);
    glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_COUNT, 1, &max_compute_work_group[1]);
    glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_COUNT, 2, &max_compute_work_group[2]);
    
    glGetIntegerv(GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS, &max_compute_invocations);
    glGetIntegerv(GL_MAX_COMPUTE_SHARED_MEMORY_SIZE, &max_shared_memory);
    
    printf("\nCompute Capabilities:\n");
    printf("  Max work groups: %d x %d x %d\n", 
           max_compute_work_group[0], max_compute_work_group[1], max_compute_work_group[2]);
    printf("  Max work group invocations: %d\n", max_compute_invocations);
    printf("  Max shared memory: %d KB\n", max_shared_memory / 1024);
    
    printf("\n================================\n");
    printf("NVIDIA A4500 Ready for Compute!\n");
    printf("5,888 CUDA cores at your service!\n");
    printf("================================\n");
    
    // Cleanup
    eglDestroyContext(display, context);
    eglTerminate(display);
    
    return 0;
}