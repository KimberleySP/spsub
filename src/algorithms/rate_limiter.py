from typing import Optional
import math
import time

class TokenBucket:
    """
    Token Bucket Algorithm implementation.
    
    Theoretical Model:
    Let r be the token rate and b the bucket size.
    At any time t, the number of available tokens is:
    
    N(t) = min(b, N₀ + r(t - t₀))
    
    where N₀ is the initial token count and t₀ is the last update time.
    
    The algorithm satisfies the following properties:
    1. ∀t. 0 ≤ N(t) ≤ b
    2. lim(t→∞) N(t)/t = r
    """
    
    def __init__(self, rate: float, capacity: float):
        """
        Initialize with rate r and capacity b where:
        r ∈ ℝ⁺ (positive reals)
        b ∈ ℝ⁺ and b ≥ r
        """
        assert rate > 0 and capacity >= rate
        self.rate = rate
        self.capacity = capacity 