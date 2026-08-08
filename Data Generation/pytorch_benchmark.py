import torch
import timeit

def batched_dot_mul_sum(a, b):
    '''Computes batched dot by multiplying and summing'''
    return a.mul(b).sum(-1)


def batched_dot_bmm(a, b):
    '''Computes batched dot by reducing to ``bmm``'''
    a = a.reshape(-1, 1, a.shape[-1])
    b = b.reshape(-1, b.shape[-1], 1)
    return torch.bmm(a, b).flatten(-3)

# Input for benchmarking
x = torch.randn(10000, 64)

# Ensure that both functions compute the same output
assert batched_dot_mul_sum(x, x).allclose(batched_dot_bmm(x, x))


t0 = timeit.Timer(
    stmt='batched_dot_mul_sum(x, x)',
    setup='from __main__ import batched_dot_mul_sum',
    globals={'x': x})

t1 = timeit.Timer(
    stmt='batched_dot_bmm(x, x)',
    setup='from __main__ import batched_dot_bmm',
    globals={'x': x})

mul_sum_out = f'mul_sum(x, x):  {t0.timeit(100) / 100 * 1e6:>5.1f} us'
bmm_out = f'bmm(x, x):      {t1.timeit(100) / 100 * 1e6:>5.1f} us'

print(mul_sum_out)
print(bmm_out)

## write the output to a file
with open('pytorch_benchmark.txt', 'w') as f:
    f.write(mul_sum_out + '\n')
    f.write(bmm_out + '\n')
# Save the output to a file
print('Benchmark results saved to pytorch_benchmark.txt')

# move this file to /dummy_directory/
import shutil
shutil.move('pytorch_benchmark.txt', '/dummy_directory/pytorch_benchmark.txt')
