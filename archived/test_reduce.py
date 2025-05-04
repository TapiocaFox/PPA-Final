import sys, time
import numpy as np
from parallel_primitives import parallel_reduce, sequential_reduce
from typing import Callable, TypeVar, List, Any

# Helper functions
def add(a, b):
    return a + b

def cube(i):
    return i * i * i

def get_array_element(arr, i):
    return arr[i]

class ArrayAccessor:
    def __init__(self, arr):
        self.arr = arr
    
    def __call__(self, i):
        return self.arr[i]

class ComplexArrayAccessor:
    def __init__(self, arr):
        self.arr = arr
    
    def __call__(self, i):
        # More complex operation: multiple operations
        val = self.arr[i]
        return np.sqrt(val * val + i * i) * np.sin(val) * np.cos(i)

def test_reduce():
    print("Testing parallel reduce with different examples:")
    
    # Test 1: Computing sum of cubes with larger range
    n = 10000000  # Increased size
    print("\n1. Computing sum of cubes from 0 to", n-1)
    
    start_time = time.time()
    parallel_result = parallel_reduce(add, 0, 0, n, cube, num_workers=4)
    parallel_time = time.time() - start_time
    
    start_time = time.time()
    sequential_result = sequential_reduce(add, 0, 0, n, cube)
    sequential_time = time.time() - start_time
    
    print(f"Parallel result: {parallel_result}")
    print(f"Sequential result: {sequential_result}")
    print(f"Results match: {parallel_result == sequential_result}")
    print(f"Parallel time: {parallel_time:.4f} seconds")
    print(f"Sequential time: {sequential_time:.4f} seconds")
    print(f"Speedup: {sequential_time/parallel_time:.2f}x")
    
    # Test 2: Finding maximum value with larger array and more complex operation
    print("\n2. Finding maximum value in array with complex operation")
    arr = np.random.randint(0, 1000000, size=1000000)  # Increased size
    
    # Create a picklable array accessor with complex operation
    array_accessor = ComplexArrayAccessor(arr)
    
    start_time = time.time()
    parallel_max = parallel_reduce(max, float('-inf'), 0, len(arr), array_accessor, num_workers=4)
    parallel_time = time.time() - start_time
    
    start_time = time.time()
    sequential_max = sequential_reduce(max, float('-inf'), 0, len(arr), array_accessor)
    sequential_time = time.time() - start_time
    
    print(f"Parallel max: {parallel_max}")
    print(f"Sequential max: {sequential_max}")
    print(f"Results match: {parallel_max == sequential_max}")
    print(f"Parallel time: {parallel_time:.4f} seconds")
    print(f"Sequential time: {sequential_time:.4f} seconds")
    print(f"Speedup: {sequential_time/parallel_time:.2f}x")
    
    # Test 3: String concatenation with larger size
    print("\n3. String concatenation with larger size")
    n = 199999  # Increased size
    
    start_time = time.time()
    parallel_str = parallel_reduce(add, "", 0, n, str, num_workers=4)
    parallel_time = time.time() - start_time
    
    start_time = time.time()
    sequential_str = sequential_reduce(add, "", 0, n, str)
    sequential_time = time.time() - start_time
    
    print(f"Parallel string length: {len(parallel_str)}")
    print(f"Sequential string length: {len(sequential_str)}")
    print(f"Results match: {parallel_str == sequential_str}")
    print(f"Parallel time: {parallel_time:.4f} seconds")
    print(f"Sequential time: {sequential_time:.4f} seconds")
    print(f"Speedup: {sequential_time/parallel_time:.2f}x")

if __name__ == "__main__":
    test_reduce() 