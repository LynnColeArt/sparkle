// Simple C wrapper to create EGL context and run GPU test
#include <stdio.h>
#include <stdlib.h>
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GL/gl.h>
#include <GL/glext.h>
#include <GL/glcorearb.h>
#include <sys/time.h>

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

// Simple test function
void test_simple_compute() {
    // Create a simple compute shader
    const char* compute_source = 
        "#version 430 core\n"
        "layout(local_size_x = 64) in;\n"
        "layout(std430, binding = 0) buffer OutputBuffer {\n"
        "    float data[];\n"
        "} output_buf;\n"
        "void main() {\n"
        "    uint idx = gl_GlobalInvocationID.x;\n"
        "    if (idx < 1024) {\n"
        "        output_buf.data[idx] = float(idx) * 0.1;\n"
        "    }\n"
        "}\n";
    
    // Compile shader
    GLuint shader = glCreateShader(GL_COMPUTE_SHADER);
    glShaderSource(shader, 1, &compute_source, NULL);
    glCompileShader(shader);
    
    GLint status;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    if (status != GL_TRUE) {
        char log[1024];
        glGetShaderInfoLog(shader, 1024, NULL, log);
        printf("Shader compilation failed: %s\n", log);
        return;
    }
    
    // Create program
    GLuint program = glCreateProgram();
    glAttachShader(program, shader);
    glLinkProgram(program);
    
    glGetProgramiv(program, GL_LINK_STATUS, &status);
    if (status != GL_TRUE) {
        char log[1024];
        glGetProgramInfoLog(program, 1024, NULL, log);
        printf("Program linking failed: %s\n", log);
        return;
    }
    
    // Create buffer
    GLuint ssbo;
    glGenBuffers(1, &ssbo);
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, ssbo);
    glBufferData(GL_SHADER_STORAGE_BUFFER, 1024 * sizeof(float), NULL, GL_DYNAMIC_DRAW);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, ssbo);
    
    // Execute
    glUseProgram(program);
    
    struct timeval start, end;
    gettimeofday(&start, NULL);
    
    glDispatchCompute(16, 1, 1);  // 16 * 64 = 1024 threads
    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);
    glFinish();
    
    gettimeofday(&end, NULL);
    
    double gpu_time_ms = (end.tv_sec - start.tv_sec) * 1000.0 + 
                        (end.tv_usec - start.tv_usec) / 1000.0;
    
    // Read back
    float* data = (float*)glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0, 
                                           1024 * sizeof(float), GL_MAP_READ_BIT);
    if (data) {
        printf("First 10 values: ");
        for (int i = 0; i < 10; i++) {
            printf("%.1f ", data[i]);
        }
        printf("\n");
        glUnmapBuffer(GL_SHADER_STORAGE_BUFFER);
    }
    
    printf("GPU execution time: %.3f ms\n", gpu_time_ms);
    
    // Cleanup
    glDeleteBuffers(1, &ssbo);
    glDeleteProgram(program);
    glDeleteShader(shader);
}

int main() {
    printf("=== EGL GPU Test ===\n");
    
    if (!create_egl_context()) {
        printf("Failed to create EGL context\n");
        return 1;
    }
    
    test_simple_compute();
    
    return 0;
}