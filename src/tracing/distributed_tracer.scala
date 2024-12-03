import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import io.opentracing.{Scope, Span, SpanContext, Tracer}
import io.opentracing.propagation.{Format, TextMapAdapter}
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}

// Advanced distributed tracing system implementation
object DistributedTracer {
  sealed trait Command
  case class StartTrace(operationName: String) extends Command
  case class ContinueTrace(context: SpanContext) extends Command
  case class EndTrace(span: Span) extends Command
  case class AddTag(key: String, value: String) extends Command
  case class SetBaggageItem(key: String, value: String) extends Command
  
  sealed trait Event
  case class TraceStarted(span: Span) extends Event
  case class TraceContinued(span: Span) extends Event
  case class TraceEnded(duration: Duration) extends Event
  case class SpanModified(span: Span) extends Event
  
  // Advanced tracing context with rich metadata
  case class TraceContext(
    span: Span,
    baggage: Map[String, String],
    samplingPriority: Double,
    references: Seq[Reference],
    tags: Map[String, AnyRef],
    startTime: Long,
    parentId: Option[String],
    traceFlags: Int
  )
  
  // Trace reference types for building trace topology
  sealed trait Reference
  case class ChildOf(context: SpanContext) extends Reference
  case class FollowsFrom(context: SpanContext) extends Reference
  case class LinkedTo(context: SpanContext, kind: String) extends Reference
}

class DistributedTracer extends Actor with ActorLogging {
  import DistributedTracer._
  import context.dispatcher
  
  private val tracer: Tracer = initializeTracer()
  private var activeTraces = Map.empty[String, TraceContext]
  private val metricsCollector = new MetricsCollector()
  private val samplingManager = new AdaptiveSamplingManager()
  
  def receive: Receive = {
    case StartTrace(operationName) =>
      val span = tracer.buildSpan(operationName)
        .withTag("component", "proxy")
        .withTag("span.kind", "server")
        .withStartTimestamp(System.currentTimeMillis())
        .start()
        
      val context = TraceContext(
        span = span,
        baggage = extractBaggage(span),
        samplingPriority = samplingManager.calculatePriority(),
        references = Nil,
        tags = Map(
          "service.name" -> "proxy",
          "service.version" -> BuildInfo.version,
          "environment" -> sys.env.getOrElse("ENV", "production")
        ),
        startTime = System.currentTimeMillis(),
        parentId = Option(span.context().toSpanId),
        traceFlags = calculateTraceFlags()
      )
      
      activeTraces += (span.context().toTraceId -> context)
      metricsCollector.incrementActiveTraces()
      sender() ! TraceStarted(span)

    case ContinueTrace(parentContext) =>
      val childSpan = tracer.buildSpan("proxy-operation")
        .asChildOf(parentContext)
        .withTag("span.kind", "internal")
        .start()
        
      val context = buildChildContext(childSpan, parentContext)
      activeTraces += (childSpan.context().toTraceId -> context)
      sender() ! TraceContinued(childSpan)

    case EndTrace(span) =>
      activeTraces.get(span.context().toTraceId).foreach { context =>
        finishTrace(context)
        metricsCollector.decrementActiveTraces()
        activeTraces -= span.context().toTraceId
      }
      sender() ! TraceEnded(calculateDuration(context.startTime))

    case AddTag(key, value) =>
      val spanId = sender().path.name
      activeTraces.get(spanId).foreach { context =>
        context.span.setTag(key, value)
        sender() ! SpanModified(context.span)
      }

    case SetBaggageItem(key, value) =>
      val spanId = sender().path.name
      activeTraces.get(spanId).foreach { context =>
        context.span.setBaggageItem(key, value)
        propagateContext(context, createCarrier())
      }
  }

  private def buildChildContext(span: Span, parent: SpanContext): TraceContext = {
    TraceContext(
      span = span,
      baggage = extractBaggage(span),
      samplingPriority = inheritSamplingPriority(parent),
      references = Seq(ChildOf(parent)),
      tags = inheritTags(parent),
      startTime = System.currentTimeMillis(),
      parentId = Some(parent.toSpanId),
      traceFlags = inheritTraceFlags(parent)
    )
  }

  private def finishTrace(context: TraceContext): Unit = {
    context.span.finish()
    samplingManager.recordTraceDuration(
      System.currentTimeMillis() - context.startTime
    )
    metricsCollector.recordTrace(context)
  }

  private def propagateContext(
    context: TraceContext,
    carrier: TextMapAdapter
  ): Unit = {
    tracer.inject(
      context.span.context(),
      Format.Builtin.TEXT_MAP,
      carrier
    )
    
    context.baggage.foreach { case (key, value) =>
      carrier.put(s"baggage-$key", value)
    }
    
    carrier.put("sampling-priority", 
      context.samplingPriority.toString)
    carrier.put("trace-flags",
      context.traceFlags.toString)
  }

  private def extractBaggage(span: Span): Map[String, String] = {
    val context = span.context()
    val keys = context.baggageItems().iterator()
    keys.map(k => k -> context.getBaggageItem(k)).toMap
  }

  private def inheritTags(parent: SpanContext): Map[String, AnyRef] = {
    activeTraces.get(parent.toTraceId)
      .map(_.tags)
      .getOrElse(Map.empty)
  }

  private def calculateTraceFlags(): Int = {
    var flags = 0
    if (sys.env.get("SAMPLED").contains("1")) flags |= 1
    if (sys.env.get("DEBUG").contains("1")) flags |= 2
    flags
  }

  private def inheritTraceFlags(parent: SpanContext): Int = {
    activeTraces.get(parent.toTraceId)
      .map(_.traceFlags)
      .getOrElse(0)
  }

  private def inheritSamplingPriority(parent: SpanContext): Double = {
    activeTraces.get(parent.toTraceId)
      .map(_.samplingPriority)
      .getOrElse(samplingManager.calculatePriority())
  }

  private def calculateDuration(startTime: Long): Duration = {
    Duration(System.currentTimeMillis() - startTime, MILLISECONDS)
  }
} 