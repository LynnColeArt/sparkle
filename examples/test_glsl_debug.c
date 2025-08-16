// Debug GLSL shader compilation
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GL/gl.h>
#include <GL/glext.h>
#include <GL/glcorearb.h>

// Function to create EGL context
int create_egl_context() {
    EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
    if (display == EGL_NO_DISPLAY) {
        printf("Failed to get EGL display\n");
        return 0;
    }
    
    EGLint major, minor;
    if (!eglInitialize(display, &major, &minor)) {
        printf("Failed to initialize EGL\n");
        return 0;
    }
    
    printf("EGL version: %d.%d\n", major, minor);
    
    // Choose config
    EGLint config_attribs[] = {
        EGL_SURFACE_TYPE, EGL_PBUFFER_BIT,
        EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
        EGL_NONE
    };
    
    EGLConfig config;
    EGLint num_configs;
    if (!eglChooseConfig(display, config_attribs, &config, 1, &num_configs)) {
        printf("Failed to choose EGL config\n");
        return 0;
    }
    
    // Bind OpenGL API
    if (!eglBindAPI(EGL_OPENGL_API)) {
        printf("Failed to bind OpenGL API\n");
        return 0;
    }
    
    // Create context
    EGLint context_attribs[] = {
        EGL_CONTEXT_MAJOR_VERSION, 4,
        EGL_CONTEXT_MINOR_VERSION, 3,
        EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT,
        EGL_NONE
    };
    
    EGLContext context = eglCreateContext(display, config, EGL_NO_CONTEXT, context_attribs);
    if (context == EGL_NO_CONTEXT) {
        printf("Failed to create EGL context\n");
        return 0;
    }
    
    // Create a small pbuffer surface
    EGLint surface_attribs[] = {
        EGL_WIDTH, 1,
        EGL_HEIGHT, 1,
        EGL_NONE
    };
    
    EGLSurface surface = eglCreatePbufferSurface(display, config, surface_attribs);
    if (surface == EGL_NO_SURFACE) {
        printf("Failed to create pbuffer surface\n");
        return 0;
    }
    
    // Make current
    if (!eglMakeCurrent(display, surface, surface, context)) {
        printf("Failed to make context current\n");
        return 0;
    }
    
    // Print GL info
    printf("GL Vendor: %s\n", glGetString(GL_VENDOR));
    printf("GL Renderer: %s\n", glGetString(GL_RENDERER));
    printf("GL Version: %s\n", glGetString(GL_VERSION));
    
    return 1;
}

void test_shader_compilation() {
    // Test different shader sources
    const char* shader1 = "#version 430 core\nlayout(local_size_x = 64) in;\nvoid main() {}\n";
    const char* shader2 = "#version 430 core\n"
                          "layout(local_size_x = 64) in;\n"
                          "void main() {}";
    
    printf("\n=== Testing Shader 1 ===\n");
    printf("Shader source:\n---\n%s\n---\n", shader1);
    printf("Length: %lu\n", strlen(shader1));
    
    GLuint shader = glCreateShader(GL_COMPUTE_SHADER);
    printf("Shader ID: %u\n", shader);
    
    glShaderSource(shader, 1, &shader1, NULL);
    glCompileShader(shader);
    
    GLint status;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    printf("Compile status: %d\n", status);
    
    if (status != GL_TRUE) {
        char log[1024];
        glGetShaderInfoLog(shader, 1024, NULL, log);
        printf("Error: %s\n", log);
    } else {
        printf("Success!\n");
    }
    
    glDeleteShader(shader);
    
    // Test with explicit length
    printf("\n=== Testing Shader 2 (with length) ===\n");
    shader = glCreateShader(GL_COMPUTE_SHADER);
    GLint length = strlen(shader2);
    glShaderSource(shader, 1, &shader2, &length);
    glCompileShader(shader);
    
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    printf("Compile status: %d\n", status);
    
    if (status != GL_TRUE) {
        char log[1024];
        glGetShaderInfoLog(shader, 1024, NULL, log);
        printf("Error: %s\n", log);
    } else {
        printf("Success!\n");
    }
    
    glDeleteShader(shader);
}

int main() {
    printf("=== GLSL Debug Test ===\n");
    
    if (!create_egl_context()) {
        printf("Failed to create EGL context\n");
        return 1;
    }
    
    test_shader_compilation();
    
    return 0;
}