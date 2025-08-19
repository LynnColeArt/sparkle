#!/usr/bin/env python3
"""
Find circular dependencies in Fortran modules
"""

import os
import re
from collections import defaultdict

def find_module_name(filepath):
    """Extract module name from Fortran file"""
    with open(filepath, 'r') as f:
        content = f.read()
        match = re.search(r'^\s*module\s+(\w+)', content, re.MULTILINE)
        if match:
            return match.group(1)
    return None

def find_dependencies(filepath):
    """Extract dependencies (use statements) from Fortran file"""
    deps = []
    with open(filepath, 'r') as f:
        for line in f:
            match = re.match(r'^\s*use\s+(\w+)', line)
            if match:
                dep = match.group(1)
                # Skip intrinsic modules
                if not dep.startswith('iso_'):
                    deps.append(dep)
    return deps

def build_dependency_graph():
    """Build complete dependency graph"""
    graph = defaultdict(set)
    module_to_file = {}
    
    # First pass: find all modules and their files
    for root, dirs, files in os.walk('src'):
        for file in files:
            if file.endswith('.f90'):
                filepath = os.path.join(root, file)
                module_name = find_module_name(filepath)
                if module_name:
                    module_to_file[module_name] = filepath
    
    # Second pass: build dependency graph
    for module, filepath in module_to_file.items():
        deps = find_dependencies(filepath)
        for dep in deps:
            if dep in module_to_file:  # Only include our modules
                graph[module].add(dep)
    
    return graph, module_to_file

def find_cycles_dfs(graph, start, visited, path, cycles):
    """Find all cycles using DFS"""
    visited.add(start)
    path.append(start)
    
    for neighbor in graph.get(start, []):
        if neighbor in path:
            # Found a cycle
            cycle_start = path.index(neighbor)
            cycle = path[cycle_start:] + [neighbor]
            cycles.append(cycle)
        elif neighbor not in visited:
            find_cycles_dfs(graph, neighbor, visited, path, cycles)
    
    path.pop()

def find_all_cycles(graph):
    """Find all circular dependencies"""
    cycles = []
    visited = set()
    
    for node in graph:
        if node not in visited:
            find_cycles_dfs(graph, node, visited, [], cycles)
    
    # Remove duplicate cycles
    unique_cycles = []
    for cycle in cycles:
        # Normalize cycle (smallest element first)
        min_idx = cycle.index(min(cycle))
        normalized = cycle[min_idx:] + cycle[:min_idx]
        if normalized not in unique_cycles:
            unique_cycles.append(normalized)
    
    return unique_cycles

def main():
    print("🔍 Analyzing Fortran module dependencies...")
    print("=" * 60)
    
    graph, module_to_file = build_dependency_graph()
    
    print(f"\nFound {len(module_to_file)} modules")
    print(f"Total dependencies: {sum(len(deps) for deps in graph.values())}")
    
    # Find circular dependencies
    cycles = find_all_cycles(graph)
    
    if cycles:
        print(f"\n🔴 Found {len(cycles)} circular dependencies:\n")
        for i, cycle in enumerate(cycles, 1):
            print(f"{i}. Circular dependency chain:")
            for j in range(len(cycle) - 1):
                print(f"   {cycle[j]} → {cycle[j+1]}")
            print()
    else:
        print("\n✅ No circular dependencies found!")
    
    # Find modules with most dependencies
    print("\n📊 Modules with most dependencies:")
    sorted_deps = sorted(graph.items(), key=lambda x: len(x[1]), reverse=True)[:10]
    for module, deps in sorted_deps:
        print(f"  {module}: {len(deps)} dependencies")
    
    # Find most depended-upon modules
    reverse_graph = defaultdict(set)
    for module, deps in graph.items():
        for dep in deps:
            reverse_graph[dep].add(module)
    
    print("\n📊 Most depended-upon modules:")
    sorted_rev = sorted(reverse_graph.items(), key=lambda x: len(x[1]), reverse=True)[:10]
    for module, dependents in sorted_rev:
        print(f"  {module}: used by {len(dependents)} modules")

if __name__ == "__main__":
    main()