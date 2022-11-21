a = 1
b = 2

a + b

# Test with numpy- just make sure it works with poetry following simple https://numpy.org/
# The standard way to import NumPy:
import numpy as np

# Create a 2-D array, set every second element in
# some rows and find max per row:

x = np.arange(15, dtype=np.int64).reshape(3, 5)
x[1:, ::2] = -99
x

x.max(axis=1)

# Try pandas after moving poetry's venv location
import pandas as pd

pd.DataFrame(x)

# Works.