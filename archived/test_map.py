import sys, time
import numpy as np
from parallel_primitives import parallel_map, sequential_map
from typing import Callable, TypeVar, List, Any

# Helper functions
def square(i):
    return i * i

def cube(i):
    return i * i * i

def complex_operation(i):
    return np.sqrt(i * i + i) * np.sin(i) * np.cos(i)

class ArrayAccessor:
    def __init__(self, arr):
        self.arr = arr
    
    def __call__(self, i):
        return self.arr[i]

def test_map():
    print("Testing parallel map with different examples:")
    
    # Test 1: Computing squares with larger range
    n = 9999999  # Large size
    print("\n1. Computing squares from 0 to", n-1)
    
    start_time = time.time()
    parallel_result = parallel_map(square, 0, n, num_workers=4)
    parallel_time = time.time() - start_time
    
    start_time = time.time()
    sequential_result = sequential_map(square, 0, n)
    sequential_time = time.time() - start_time
    
    print(f"Parallel result length: {len(parallel_result)}")
    print(f"Sequential result length: {len(sequential_result)}")
    print(f"Results match: {parallel_result == sequential_result}")
    print(f"Parallel time: {parallel_time:.4f} seconds")
    print(f"Sequential time: {sequential_time:.4f} seconds")
    print(f"Speedup: {sequential_time/parallel_time:.2f}x")
    
    # Test 2: Complex mathematical operations
    print("\n2. Complex mathematical operations")
    n = 999999  # Slightly smaller size due to complexity
    
    start_time = time.time()
    parallel_result = parallel_map(complex_operation, 0, n, num_workers=4)
    parallel_time = time.time() - start_time
    
    start_time = time.time()
    sequential_result = sequential_map(complex_operation, 0, n)
    sequential_time = time.time() - start_time
    
    print(f"Parallel result length: {len(parallel_result)}")
    print(f"Sequential result length: {len(sequential_result)}")
    print(f"Results match: {parallel_result == sequential_result}")
    print(f"Parallel time: {parallel_time:.4f} seconds")
    print(f"Sequential time: {sequential_time:.4f} seconds")
    print(f"Speedup: {sequential_time/parallel_time:.2f}x")
    
    # Test 3: Array element access
    print("\n3. Array element access")
    arr = np.random.randint(0, 1000000, size=1000000)
    array_accessor = ArrayAccessor(arr)
    
    start_time = time.time()
    parallel_result = parallel_map(array_accessor, 0, len(arr), num_workers=4)
    parallel_time = time.time() - start_time
    
    start_time = time.time()
    sequential_result = sequential_map(array_accessor, 0, len(arr))
    sequential_time = time.time() - start_time
    
    print(f"Parallel result length: {len(parallel_result)}")
    print(f"Sequential result length: {len(sequential_result)}")
    print(f"Results match: {parallel_result == sequential_result}")
    print(f"Parallel time: {parallel_time:.4f} seconds")
    print(f"Sequential time: {sequential_time:.4f} seconds")
    print(f"Speedup: {sequential_time/parallel_time:.2f}x")

if __name__ == "__main__":
    test_map() 