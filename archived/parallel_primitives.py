import numpy as np
import multiprocessing as mp
from typing import Callable, TypeVar, List, Any, Tuple
import time

threads_num = 4
BLOCK_SIZE = 10000  # Increased block size to reduce overhead

T = TypeVar('T')

# Helper class for block sums
class BlockSumHelper:
    def __init__(self, block_sums):
        self.block_sums = block_sums
    
    def __call__(self, i):
        return self.block_sums[i]

# Map
def process_map_chunk(args):
    f, start, end = args
    return [f(i) for i in range(start, end)]

def parallel_map(f: Callable[[int], T], lo: int, hi: int, num_workers: int = None) -> List[T]:
    """Parallel map implementation."""
    if lo >= hi:
        return []
    
    # Split range into chunks
    if num_workers is None:
        num_workers = 4  # Default to 4 workers
    print(f"Using {num_workers} workers")
    
    # Calculate chunk size and handle remainder
    total_size = hi - lo
    chunk_size = total_size // num_workers
    remainder = total_size % num_workers
    
    # Create chunks with proper boundaries
    chunks = []
    current = lo
    for i in range(num_workers):
        # Add 1 to chunk size for the first 'remainder' chunks
        this_chunk_size = chunk_size + (1 if i < remainder else 0)
        chunks.append((f, current, current + this_chunk_size))
        current += this_chunk_size
    
    # Use multiprocessing Pool for parallel execution
    with mp.Pool(processes=num_workers) as pool:
        results = pool.map(process_map_chunk, chunks)
    
    # Flatten results
    return [item for sublist in results for item in sublist]

def sequential_map(f: Callable[[int], T], lo: int, hi: int) -> List[T]:
    """Sequential map implementation for comparison."""
    return [f(i) for i in range(lo, hi)]

# Reduce
def process_chunk(args):
    g, z, f, start, end = args
    result = z
    for i in range(start, end):
        result = g(result, f(i))
    return result

def parallel_reduce(g: Callable[[T, T], T], z: T, lo: int, hi: int, f: Callable[[int], T], num_workers: int = None) -> T:
    """Parallel reduction implementation."""
    if lo >= hi:
        return z
    elif lo + 1 == hi:
        return f(lo)
    
    # Split range into chunks
    if num_workers is None:
        num_workers = 4  # Default to 4 workers
    print(f"Using {num_workers} workers")
    
    # Calculate chunk size and handle remainder
    total_size = hi - lo
    chunk_size = total_size // num_workers
    remainder = total_size % num_workers
    
    # Create chunks with proper boundaries
    chunks = []
    current = lo
    for i in range(num_workers):
        # Add 1 to chunk size for the first 'remainder' chunks
        this_chunk_size = chunk_size + (1 if i < remainder else 0)
        chunks.append((g, z, f, current, current + this_chunk_size))
        current += this_chunk_size
    
    # Use multiprocessing Pool for parallel execution
    with mp.Pool(processes=num_workers) as pool:
        results = pool.map(process_chunk, chunks)
    
    # Combine results sequentially
    result = z
    for r in results:
        result = g(result, r)
    return result

def sequential_reduce(g: Callable[[T, T], T], z: T, lo: int, hi: int, f: Callable[[int], T]) -> T:
    """Sequential reduction implementation for comparison."""
    result = z
    for i in range(lo, hi):
        result = g(result, f(i))
    return result

# Scan
def process_block(args):
    g, z, f, start, end = args
    # Compute block results
    block_results = []
    current = z
    for i in range(start, end):
        current = g(current, f(i))
        block_results.append(current)
    
    return block_results

def parallel_scan(g: Callable[[T, T], T], z: T, lo: int, hi: int, f: Callable[[int], T], num_workers: int = None) -> List[T]:
    """Two-phase parallel scan implementation."""
    if lo >= hi:
        return []
    elif lo + 1 == hi:
        return [z, g(z, f(lo))]
    
    n = hi - lo
    # Allocate output array
    output = [None] * (n + 1)
    output[0] = z
    
    # Phase 1: Process blocks in parallel
    num_blocks = (n + BLOCK_SIZE - 1) // BLOCK_SIZE
    blocks = []
    current = lo
    for i in range(num_blocks):
        block_end = min(current + BLOCK_SIZE, hi)
        blocks.append((g, z, f, current, block_end))
        current = block_end
    
    # Process blocks in parallel
    with mp.Pool(processes=num_workers) as pool:
        block_results = pool.map(process_block, blocks)
    
    # Debug: Print block results
    print("\nDebug - Block Results:")
    for i, block in enumerate(block_results):
        print(f"Block {i}: {block[:5]}...{block[-5:] if len(block) > 10 else ''}")
    
    # Phase 2: Combine results sequentially
    current_prefix = z
    current_block = 0
    current_offset = 0
    
    for i in range(1, n + 1):
        if i % BLOCK_SIZE == 0:
            if current_block > 0:
                current_prefix = block_results[current_block - 1][-1]
                print(f"\nDebug - Block {current_block} prefix: {current_prefix}")
            current_block += 1
            current_offset = 0
        
        if current_block < len(block_results):
            if current_offset == 0:
                # For the first element in each block, combine prefix with first element
                first_element = f(lo + i - 1)  # Get the actual first element
                output[i] = g(current_prefix, first_element)
            else:
                # For other elements, use the block results directly
                output[i] = block_results[current_block][current_offset]
            
            # Debug: Print first few elements of each block
            if i < 10 or (i % BLOCK_SIZE < 5 and i > BLOCK_SIZE):
                print(f"Debug - Output[{i}]: {output[i]}")
            
            current_offset += 1
    
    return output

def sequential_scan(g: Callable[[T, T], T], z: T, lo: int, hi: int, f: Callable[[int], T]) -> List[T]:
    """Sequential scan implementation for comparison."""
    if lo >= hi:
        return []
    
    result = z
    scan_results = [z]  # Start with the identity element
    
    # Debug: Print first few elements
    print("\nDebug - Sequential Results:")
    for i in range(lo, hi):
        result = g(result, f(i))
        scan_results.append(result)
        if i < 10 or (i % BLOCK_SIZE < 5 and i > BLOCK_SIZE):
            print(f"Debug - Sequential[{i+1}]: {result}")
    
    return scan_results