#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>
#include <GL/glext.h>

// External functions from gpu_opengl_reference.c
extern int gpu_is_initialized();
extern float gpu_execute_conv2d_fortran(const float* input, const float* weights, float* output,
                                        int N, int C, int H, int W, int K, int kernel_size, 
                                        int stride, int pad, int H_out, int W_out);

// Conv2D parameters structure
typedef struct {
    int N, H, W, C, K;
    int kernel_size, stride, pad;
    int H_out, W_out;
} conv2d_params_t;

// Execute conv2d with custom shader source
float gpu_execute_conv2d_with_shader(const float* input, const float* weights, float* output,
                                     int N, int C, int H, int W, int K, int kernel_size, 
                                     int stride, int pad, int H_out, int W_out,
                                     const char* shader_source) {
    
    if (!gpu_is_initialized()) {
        printf("ERROR: GPU not initialized!\n");
        return -1.0f;
    }
    
    // Create and compile the shader
    GLuint compute_shader = glCreateShader(GL_COMPUTE_SHADER);
    if (compute_shader == 0) {
        printf("Failed to create compute shader\n");
        return -1.0f;
    }
    
    // Set the custom shader source
    glShaderSource(compute_shader, 1, &shader_source, NULL);
    
    // Compile shader
    glCompileShader(compute_shader);
    
    // Check compilation status
    GLint compile_status;
    glGetShaderiv(compute_shader, GL_COMPILE_STATUS, &compile_status);
    if (compile_status != GL_TRUE) {
        GLint log_length;
        glGetShaderiv(compute_shader, GL_INFO_LOG_LENGTH, &log_length);
        if (log_length > 0) {
            char* log = malloc(log_length);
            glGetShaderInfoLog(compute_shader, log_length, NULL, log);
            printf("Shader compilation failed:\n%s\n", log);
            free(log);
        }
        glDeleteShader(compute_shader);
        return -1.0f;
    }
    
    // Create program
    GLuint program = glCreateProgram();
    if (program == 0) {
        printf("Failed to create compute program\n");
        glDeleteShader(compute_shader);
        return -1.0f;
    }
    
    // Attach and link
    glAttachShader(program, compute_shader);
    glLinkProgram(program);
    
    // Check link status
    GLint link_status;
    glGetProgramiv(program, GL_LINK_STATUS, &link_status);
    if (link_status != GL_TRUE) {
        GLint log_length;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &log_length);
        if (log_length > 0) {
            char* log = malloc(log_length);
            glGetProgramInfoLog(program, log_length, NULL, log);
            printf("Program linking failed:\n%s\n", log);
            free(log);
        }
        glDeleteProgram(program);
        glDeleteShader(compute_shader);
        return -1.0f;
    }
    
    // Clean up shader object
    glDeleteShader(compute_shader);
    
    // Calculate sizes
    size_t input_size = N * C * H * W * sizeof(float);
    size_t weight_size = K * C * kernel_size * kernel_size * sizeof(float);
    size_t output_size = N * K * H_out * W_out * sizeof(float);
    size_t param_size = sizeof(conv2d_params_t);
    
    // Create buffers
    GLuint buffers[4];
    glGenBuffers(4, buffers);
    
    // Upload input data
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers[0]);
    glBufferData(GL_SHADER_STORAGE_BUFFER, input_size, input, GL_STATIC_DRAW);
    
    // Upload weights
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers[1]);
    glBufferData(GL_SHADER_STORAGE_BUFFER, weight_size, weights, GL_STATIC_DRAW);
    
    // Create output buffer
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers[2]);
    glBufferData(GL_SHADER_STORAGE_BUFFER, output_size, NULL, GL_STATIC_DRAW);
    
    // Upload parameters
    conv2d_params_t params = {
        .N = N, .C = C, .H = H, .W = W, .K = K,
        .kernel_size = kernel_size, .stride = stride, .pad = pad,
        .H_out = H_out, .W_out = W_out
    };
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers[3]);
    glBufferData(GL_SHADER_STORAGE_BUFFER, param_size, &params, GL_STATIC_DRAW);
    
    // Bind buffers to shader
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffers[0]);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, buffers[1]);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, buffers[2]);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 3, buffers[3]);
    
    // Use program
    glUseProgram(program);
    
    // Calculate work groups
    int total_elements = N * K * H_out * W_out;
    
    // Determine local_size_x from shader source
    // Look for "local_size_x = " in the shader
    int local_size_x = 64;  // default
    const char* local_size_str = strstr(shader_source, "local_size_x = ");
    if (local_size_str) {
        local_size_str += strlen("local_size_x = ");
        local_size_x = atoi(local_size_str);
    }
    
    int num_groups = (total_elements + local_size_x - 1) / local_size_x;
    
    // GPU timing with queries
    GLuint query_ids[2];
    glGenQueries(2, query_ids);
    
    // Benchmark iterations
    int bench_iters = 20;
    
    glQueryCounter(query_ids[0], GL_TIMESTAMP);
    
    // Execute multiple times
    for (int i = 0; i < bench_iters; i++) {
        glDispatchCompute(num_groups, 1, 1);
        glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);
    }
    glFinish();
    
    glQueryCounter(query_ids[1], GL_TIMESTAMP);
    
    // Get timestamps
    GLuint64 time_start, time_end;
    glGetQueryObjectui64v(query_ids[0], GL_QUERY_RESULT, &time_start);
    glGetQueryObjectui64v(query_ids[1], GL_QUERY_RESULT, &time_end);
    
    // Calculate average time per iteration in milliseconds
    double time_ms = (double)(time_end - time_start) / 1.0e6 / bench_iters;
    
    // Download results
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffers[2]);
    void* mapped = glMapBuffer(GL_SHADER_STORAGE_BUFFER, GL_READ_ONLY);
    if (mapped) {
        memcpy(output, mapped, output_size);
        glUnmapBuffer(GL_SHADER_STORAGE_BUFFER);
    }
    
    // Clean up
    glDeleteBuffers(4, buffers);
    glDeleteQueries(2, query_ids);
    glDeleteProgram(program);
    
    return (float)time_ms;
}