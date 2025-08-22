# Sporkle PM4 Core Makefile

CC = gcc
CFLAGS = -O2 -Wall -g
LDFLAGS = -ldrm

# Source files
CORE_SRCS = compute/submit.c pm4/preamble.c compute/selftest.c
CLI_SRCS = cli/sporkle.c

# Object files
CORE_OBJS = $(CORE_SRCS:.c=.o)
CLI_OBJS = $(CLI_SRCS:.c=.o)

# Targets
all: sporkle

sporkle: $(CORE_OBJS) $(CLI_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(CORE_OBJS) $(CLI_OBJS) sporkle

# Quick test target
test: sporkle
	./sporkle pm4 --info
	./sporkle pm4 --selftest

.PHONY: all clean test