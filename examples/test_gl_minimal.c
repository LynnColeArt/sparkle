#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <EGL/egl.h>
#include <GL/gl.h>
#include <GL/glext.h>

// Define missing constants/functions
#ifndef GL_COMPUTE_SHADER
#define GL_COMPUTE_SHADER 0x91B9
#endif
#ifndef GL_SHADER_STORAGE_BUFFER
#define GL_SHADER_STORAGE_BUFFER 0x90D2
#endif
#ifndef GL_SHADER_STORAGE_BARRIER_BIT
#define GL_SHADER_STORAGE_BARRIER_BIT 0x00002000
#endif

// Function pointers
PFNGLCREATESHADERPROC glCreateShader;
PFNGLSHADERSOURCEPROC glShaderSource;
PFNGLCOMPILESHADERPROC glCompileShader;
PFNGLGETSHADERIVPROC glGetShaderiv;
PFNGLGETSHADERINFOLOGPROC glGetShaderInfoLog;
PFNGLCREATEPROGRAMPROC glCreateProgram;
PFNGLATTACHSHADERPROC glAttachShader;
PFNGLLINKPROGRAMPROC glLinkProgram;
PFNGLGETPROGRAMIVPROC glGetProgramiv;
PFNGLGETPROGRAMINFOLOGPROC glGetProgramInfoLog;
PFNGLUSEPROGRAMPROC glUseProgram;
PFNGLGENBUFFERSPROC glGenBuffers;
PFNGLBINDBUFFERPROC glBindBuffer;
PFNGLBUFFERDATAPROC glBufferData;
PFNGLBINDBUFFERBASEPROC glBindBufferBase;
PFNGLDISPATCHCOMPUTEPROC glDispatchCompute;
PFNGLMEMORYBARRIERPROC glMemoryBarrier;
PFNGLGETBUFFERSUBDATAPROC glGetBufferSubData;
PFNGLDELETEBUFFERSPROC glDeleteBuffers;
PFNGLDELETEPROGRAMPROC glDeleteProgram;
PFNGLDELETESHADERPROC glDeleteShader;

const char* compute_shader_src = 
    "#version 310 es\n"
    "layout(local_size_x = 1) in;\n"
    "layout(std430, binding = 0) buffer Out {\n"
    "    uint data[];\n"
    "};\n"
    "void main() {\n"
    "    data[gl_GlobalInvocationID.x] = 0xDEADBEEFu;\n"
    "}\n";

int main() {
    printf("=== Minimal GL Compute Test (C) ===\n");
    
    // Initialize EGL
    EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
    if (!display) {
        printf("Failed to get EGL display\n");
        return 1;
    }
    
    if (!eglInitialize(display, NULL, NULL)) {
        printf("Failed to initialize EGL\n");
        return 1;
    }
    
    // Bind OpenGL ES API
    eglBindAPI(EGL_OPENGL_ES_API);
    
    // Choose config
    EGLint config_attribs[] = {
        EGL_SURFACE_TYPE, EGL_PBUFFER_BIT,
        EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT,
        EGL_NONE
    };
    
    EGLConfig config;
    EGLint num_configs;
    if (!eglChooseConfig(display, config_attribs, &config, 1, &num_configs) || num_configs == 0) {
        printf("Failed to choose config\n");
        return 1;
    }
    
    // Create context
    EGLint context_attribs[] = {
        EGL_CONTEXT_CLIENT_VERSION, 3,
        EGL_NONE
    };
    
    EGLContext context = eglCreateContext(display, config, EGL_NO_CONTEXT, context_attribs);
    if (!context) {
        printf("Failed to create context\n");
        return 1;
    }
    
    // Make current
    if (!eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE, context)) {
        printf("Failed to make current\n");
        return 1;
    }
    
    printf("OpenGL ES version: %s\n", glGetString(GL_VERSION));
    printf("GLSL version: %s\n", glGetString(GL_SHADING_LANGUAGE_VERSION));
    
    // Create and compile shader
    GLuint shader = glCreateShader(GL_COMPUTE_SHADER);
    glShaderSource(shader, 1, &compute_shader_src, NULL);
    glCompileShader(shader);
    
    GLint status;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    if (status != GL_TRUE) {
        char log[1024];
        glGetShaderInfoLog(shader, sizeof(log), NULL, log);
        printf("Shader compilation failed: %s\n", log);
        return 1;
    }
    
    // Create and link program
    GLuint program = glCreateProgram();
    glAttachShader(program, shader);
    glLinkProgram(program);
    
    glGetProgramiv(program, GL_LINK_STATUS, &status);
    if (status != GL_TRUE) {
        char log[1024];
        glGetProgramInfoLog(program, sizeof(log), NULL, log);
        printf("Program linking failed: %s\n", log);
        return 1;
    }
    
    printf("✓ Shader compiled and linked successfully\n");
    
    // Create buffer
    uint32_t data[4] = {0xBAD0BAD0, 0xBAD0BAD0, 0xBAD0BAD0, 0xBAD0BAD0};
    GLuint buffer;
    glGenBuffers(1, &buffer);
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffer);
    glBufferData(GL_SHADER_STORAGE_BUFFER, sizeof(data), data, GL_DYNAMIC_COPY);
    
    // Dispatch
    glUseProgram(program);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffer);
    glDispatchCompute(4, 1, 1);
    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);
    glFinish();
    
    // Check GL error
    GLenum error = glGetError();
    if (error != GL_NO_ERROR) {
        printf("GL Error after dispatch: 0x%x\n", error);
    } else {
        printf("✓ No GL errors\n");
    }
    
    // Read back
    glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0, sizeof(data), data);
    
    printf("\nResults:\n");
    for (int i = 0; i < 4; i++) {
        printf("  data[%d] = 0x%08X %s\n", i, data[i], 
               data[i] == 0xDEADBEEF ? "✓" : "✗");
    }
    
    // Cleanup
    glDeleteBuffers(1, &buffer);
    glDeleteProgram(program);
    glDeleteShader(shader);
    
    eglDestroyContext(display, context);
    eglTerminate(display);
    
    return 0;
}