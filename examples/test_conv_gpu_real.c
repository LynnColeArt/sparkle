#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GL/gl.h>
#include <GL/glext.h>

// GL function declarations already provided by GL_GLEXT_PROTOTYPES

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

// Simple conv2d GLSL shader
const char* conv2d_shader_source = R"(
#version 430 core
layout(local_size_x = 64) in;

layout(std430, binding = 0) readonly buffer InputBuffer {
  float data[];
} input_buf;

layout(std430, binding = 1) readonly buffer WeightBuffer {
  float data[];
} weight_buf;

layout(std430, binding = 2) writeonly buffer OutputBuffer {
  float data[];
} output_buf;

layout(std430, binding = 3) readonly buffer ParamBuffer {
  int N, H, W, C, K;
  int kernel_size, stride, pad;
  int H_out, W_out;
} params;

void main() {
  uint idx = gl_GlobalInvocationID.x;
  if (idx >= uint(params.N * params.K * params.H_out * params.W_out)) return;
  
  // Decode output position
  int n = int(idx) / (params.K * params.H_out * params.W_out);
  int k = (int(idx) / (params.H_out * params.W_out)) % params.K;
  int h_out = (int(idx) / params.W_out) % params.H_out;
  int w_out = int(idx) % params.W_out;
  
  float sum = 0.0;
  
  // Convolution
  for (int c = 0; c < params.C; c++) {
    for (int kh = 0; kh < params.kernel_size; kh++) {
      for (int kw = 0; kw < params.kernel_size; kw++) {
        int h_in = h_out * params.stride + kh;
        int w_in = w_out * params.stride + kw;
        
        if (h_in < params.H && w_in < params.W) {
          int in_idx = ((n * params.C + c) * params.H + h_in) * params.W + w_in;
          int weight_idx = ((k * params.C + c) * params.kernel_size + kh) * params.kernel_size + kw;
          sum += input_buf.data[in_idx] * weight_buf.data[weight_idx];
        }
      }
    }
  }
  
  output_buf.data[idx] = sum;
}
)";

// Set shader source for Fortran
void set_conv2d_shader_source(GLuint shader) {
    printf("Setting conv2d shader source...\n");
    glShaderSource(shader, 1, &conv2d_shader_source, NULL);
}

// Forward declaration
void test_conv_gpu_real();

int main() {
    if (!create_egl_context()) {
        printf("Failed to create EGL context\n");
        return 1;
    }
    
    // Call Fortran function
    test_conv_gpu_real();
    
    return 0;
}