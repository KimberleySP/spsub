package metrics.aggregator

case class StatsWindow(
  startTime: Long,
  endTime: Long,
  count: Long,
  sum: Double,
  min: Double,
  max: Double
)

class WindowedStats(windowSize: Duration) {
  private var windows = Vector.empty[StatsWindow]
  
  def add(value: Double, timestamp: Long): Unit = {
    synchronized {
      // Implementation
    }
  }
} 