package metrics.aggregator

/**
 * Implements exponential time-decay windowing with the following properties:
 * 
 * Let λ be the decay factor, then for any metric m at time t:
 * m(t) = m₀e^(-λt) where m₀ is the initial value
 *
 * The windowed aggregation satisfies:
 * ∫[t-w,t] m(τ)dτ = m₀(1-e^(-λw))/λ
 */
class WindowedStats(windowSize: Duration, λ: Double) {
  // Exponentially weighted statistics using Kahan summation
  private case class ExponentialSum(
    μ: Double,    // Mean
    σ²: Double,   // Variance
    ε: Double     // Compensation term
  )
  
  private val windows = new ConcurrentSkipListMap[Long, ExponentialSum]
  
  def add(value: Double, timestamp: Long): Unit = synchronized {
    // Apply time-decay transformation: v(t) = v₀e^(-λt)
    val decayedValue = value * math.exp(-λ * timestamp)
    updateWindows(decayedValue, timestamp)
  }
} 