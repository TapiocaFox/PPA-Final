import sys, time
import numpy as np
from parallel_primitives import parallel_scan, sequential_scan
from typing import Callable, TypeVar, List, Any, Tuple

# Helper functions
def add(a, b):
    return a + b

def multiply(a, b):
    return a * b

def square(i):
    return i * i

def complex_operation(i):
    return np.sqrt(i * i + i) * np.sin(i) * np.cos(i)

# Linear recurrence operation: g((a1,b1), (a2,b2)) = (a1·a2, b1·a2 + b2)
def linear_recurrence_op(pair1: Tuple[float, float], pair2: Tuple[float, float]) -> Tuple[float, float]:
    a1, b1 = pair1
    a2, b2 = pair2
    # Use logarithms to handle large numbers
    log_a1 = np.log(np.abs(a1)) if a1 != 0 else float('-inf')
    log_a2 = np.log(np.abs(a2)) if a2 != 0 else float('-inf')
    log_a = log_a1 + log_a2
    
    # If the result would overflow, return a reasonable value
    if log_a > 300:  # exp(300) is approximately 1e130
        return (float('inf'), float('inf'))
    
    a = a1 * a2
    b = b1 * a2 + b2
    
    # Handle potential overflow in b
    if np.isinf(a) or np.isinf(b):
        return (float('inf'), float('inf'))
    
    return (a, b)

def copy_scan_op(a, b):
    return b if b != 0 else a

# Helper class for linear recurrence
class LinearRecurrenceHelper:
    def __init__(self, pairs):
        self.pairs = pairs
    
    def __call__(self, i):
        return self.pairs[i]

# Helper class for copy scan
class CopyScanHelper:
    def __init__(self, arr):
        self.arr = arr
    
    def __call__(self, i):
        return self.arr[i]

# Helper class for prefix sums
class PrefixSumHelper:
    def __init__(self):
        pass
    
    def __call__(self, i):
        return i

def test_scan():
    print("Testing parallel scan with different examples:")
    
    # Test 1: Linear Recurrence
    print("\n1. Linear Recurrence: r(i) = ai·r(i-1) + bi")
    n = 100000
    # Generate random coefficients for the recurrence
    a_coeffs = np.random.uniform(0.5, 1.5, n)  # Reduced range to avoid overflow
    b_coeffs = np.random.uniform(-0.5, 0.5, n)  # Reduced range to avoid overflow
    pairs = list(zip(a_coeffs, b_coeffs))
    pairs_helper = LinearRecurrenceHelper(pairs)
    
    start_time = time.time()
    parallel_result = parallel_scan(linear_recurrence_op, (1.0, 0.0), 0, n, pairs_helper, num_workers=4)
    parallel_time = time.time() - start_time
    
    start_time = time.time()
    sequential_result = sequential_scan(linear_recurrence_op, (1.0, 0.0), 0, n, pairs_helper)
    sequential_time = time.time() - start_time
    
    print(f"Parallel result length: {len(parallel_result)}")
    print(f"Sequential result length: {len(sequential_result)}")
    print(f"Results match: {parallel_result == sequential_result}")
    print(f"Parallel time: {parallel_time:.4f} seconds")
    print(f"Sequential time: {sequential_time:.4f} seconds")
    print(f"Speedup: {sequential_time/parallel_time:.2f}x")
    
    # Test 2: Copy Scan
    print("\n2. Copy Scan: Finding nearest non-zero to the left")
    # Create a sequence with sparse non-zero values
    n = 1000000
    arr = np.zeros(n, dtype=int)
    # Place non-zero values at random positions
    non_zero_indices = np.random.choice(n, size=n//100, replace=False)
    arr[non_zero_indices] = np.random.randint(1, 100, size=len(non_zero_indices))
    arr_helper = CopyScanHelper(arr)
    
    start_time = time.time()
    parallel_result = parallel_scan(copy_scan_op, 0, 0, n, arr_helper, num_workers=4)
    parallel_time = time.time() - start_time
    
    start_time = time.time()
    sequential_result = sequential_scan(copy_scan_op, 0, 0, n, arr_helper)
    sequential_time = time.time() - start_time
    
    print(f"Parallel result length: {len(parallel_result)}")
    print(f"Sequential result length: {len(sequential_result)}")
    print(f"Results match: {parallel_result == sequential_result}")
    print(f"Parallel time: {parallel_time:.4f} seconds")
    print(f"Sequential time: {sequential_time:.4f} seconds")
    print(f"Speedup: {sequential_time/parallel_time:.2f}x")
    
    # Test 3: Prefix Sums with Different Block Sizes
    print("\n3. Prefix Sums with Different Block Sizes")
    n = 1000  # Reduced size for debugging
    # Test with different block sizes to demonstrate work efficiency
    block_sizes = [100]  # Test with just one block size first
    prefix_helper = PrefixSumHelper()
    
    for block_size in block_sizes:
        print(f"\nBlock size: {block_size}")
        # Modify the block size in parallel_primitives
        import parallel_primitives
        parallel_primitives.BLOCK_SIZE = block_size
        
        start_time = time.time()
        parallel_result = parallel_scan(add, 0, 0, n, prefix_helper, num_workers=4)
        parallel_time = time.time() - start_time
        
        start_time = time.time()
        sequential_result = sequential_scan(add, 0, 0, n, prefix_helper)
        sequential_time = time.time() - start_time
        
        print(f"Parallel result length: {len(parallel_result)}")
        print(f"Sequential result length: {len(sequential_result)}")
        print(f"Results match: {parallel_result == sequential_result}")
        print(f"Parallel time: {parallel_time:.4f} seconds")
        print(f"Sequential time: {sequential_time:.4f} seconds")
        print(f"Speedup: {sequential_time/parallel_time:.2f}x")

if __name__ == "__main__":
    test_scan() 