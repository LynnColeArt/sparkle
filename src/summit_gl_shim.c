// Mini's surgical C shim for Summit performance
// Exposes exactly the 6-8 GL calls needed for 17-19 TFLOPS
#include <GL/gl.h>

// GPU Timer Queries
void summit_gen_queries(int count, unsigned int* queries) {
    glGenQueries(count, queries);
}

void summit_begin_query(unsigned int query) {
    glBeginQuery(GL_TIME_ELAPSED, query);
}

void summit_end_query(void) {
    glEndQuery(GL_TIME_ELAPSED);
}

int summit_query_available(unsigned int query) {
    GLint available = 0;
    glGetQueryObjectiv(query, GL_QUERY_RESULT_AVAILABLE, &available);
    return available;
}

unsigned long long summit_query_result(unsigned int query) {
    GLuint64 nanoseconds = 0;
    glGetQueryObjectui64v(query, GL_QUERY_RESULT, &nanoseconds);
    return nanoseconds;
}

// Persistent Ring Buffers
void summit_gen_buffers(int count, unsigned int* buffers) {
    glGenBuffers(count, buffers);
}

void summit_bind_buffer_base(unsigned int buffer, int binding_point) {
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, binding_point, buffer);
}

void summit_buffer_storage(unsigned int buffer, size_t size, int flags) {
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffer);
    glBufferStorage(GL_SHADER_STORAGE_BUFFER, size, NULL, flags);
}

void* summit_map_buffer_range(unsigned int buffer, size_t size, int flags) {
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffer);
    return glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0, size, flags);
}

// Non-blocking Fences
void* summit_fence_sync(void) {
    return glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
}

int summit_client_wait_sync(void* sync) {
    return glClientWaitSync((GLsync)sync, 0, 0);
}

void summit_delete_sync(void* sync) {
    glDeleteSync((GLsync)sync);
}

// Heavy Dispatches
void summit_dispatch_compute(int groups_x, int groups_y, int groups_z) {
    glDispatchCompute(groups_x, groups_y, groups_z);
}

void summit_memory_barrier(void) {
    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);
}

void summit_use_program(unsigned int program) {
    glUseProgram(program);
}

// VSync Control
void summit_flush(void) {
    glFlush();
}

// Constants for Fortran
int summit_gl_already_signaled(void) { return GL_ALREADY_SIGNALED; }
int summit_gl_map_write_bit(void) { return GL_MAP_WRITE_BIT; }
int summit_gl_map_persistent_bit(void) { return GL_MAP_PERSISTENT_BIT; }
int summit_gl_map_coherent_bit(void) { return GL_MAP_COHERENT_BIT; }
int summit_gl_dynamic_storage_bit(void) { return GL_DYNAMIC_STORAGE_BIT; }