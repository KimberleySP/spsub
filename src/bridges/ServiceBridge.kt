package bridges

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import java.nio.ByteBuffer
import com.sun.jna.Library
import com.sun.jna.Native

interface RustCore : Library {
    fun handle_connection(data: ByteBuffer, length: Int): Int
    fun get_metrics(): String
}

interface ScalaStats : Library {
    fun record_stats(value: Double, timestamp: Long)
    fun get_summary(): String
}

class ServiceBridge {
    private val rustCore: RustCore
    private val scalaStats: ScalaStats
    private val metricsChannel = Channel<MetricsData>(Channel.BUFFERED)
    
    init {
        rustCore = Native.load("proxy_core", RustCore::class.java)
        scalaStats = Native.load("stats_scala", ScalaStats::class.java)
        
        // 启动metrics收集协程
        GlobalScope.launch {
            collectMetrics()
        }
    }
    
    private suspend fun collectMetrics() = coroutineScope {
        while (true) {
            val rustMetrics = rustCore.get_metrics()
            val scalaMetrics = scalaStats.get_summary()
            metricsChannel.send(
                MetricsData(rustMetrics, scalaMetrics)
            )
            delay(1000)
        }
    }
} 