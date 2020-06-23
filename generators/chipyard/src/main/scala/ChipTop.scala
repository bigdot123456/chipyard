package chipyard

import chisel3._

import scala.collection.mutable.{ArrayBuffer}

import freechips.rocketchip.prci._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, LazyRawModuleImp, LazyModuleImpLike}
import freechips.rocketchip.util.{ResetCatchAndSync}
import chipyard.config.ConfigValName._
import chipyard.iobinders.{IOBinders, TestHarnessFunction, IOBinderTuple}

import barstools.iocell.chisel._

case object BuildSystem extends Field[Parameters => LazyModule]((p: Parameters) => LazyModule(new DigitalTop()(p)))

/**
  * Chipyard provides three baseline, top-level reset schemes, set using the
  * [[GlobalResetSchemeKey]] in a Parameters instance. These are:
  *
  * 1) Synchronous: The input coming to the chip is synchronous to the provided
  *    clocks and will be used without modification as a synchronous reset.
  *    This is safe only for use in FireSim and SW simulation.
  *
  * 2) Asynchronous: The input reset is asynchronous to the input clock, but it
  *    is caught and synchronized to that clock before it is dissemenated.
  *    Thus, downsteam modules will be emitted with synchronously reset state
  *    elements.
  *
  * 3) Asynchronous Full: The input reset is asynchronous to the input clock,
  *    and is used globally as an async reset. Downstream modules will be emitted
  *    with asynchronously reset state elements.
  *
  */
sealed trait GlobalResetScheme {
  def pinIsAsync: Boolean
}
sealed trait HasAsyncInput { self: GlobalResetScheme =>
  def pinIsAsync = true
}

sealed trait HasSyncInput { self: GlobalResetScheme =>
  def pinIsAsync = false
}

case object GlobalResetSynchronous extends GlobalResetScheme with HasSyncInput
case object GlobalResetAsynchronous extends GlobalResetScheme with HasAsyncInput
case object GlobalResetAsynchronousFull extends GlobalResetScheme with HasAsyncInput
case object GlobalResetSchemeKey extends Field[GlobalResetScheme](GlobalResetSynchronous)



/**
 * The base class used for building chips. This constructor instantiates a module specified by the BuildSystem parameter,
 * named "system", which is an instance of DigitalTop by default. The default clock and reset for "system" are set by two
 * wires, "systemClock" and "systemReset", which are intended to be driven by traits mixed-in with this base class.
 */
abstract class BaseChipTop(implicit p: Parameters) extends LazyModule with HasTestHarnessFunctions {

  // A publicly accessible list of IO cells (useful for a floorplanning tool, for example)
  val iocells = ArrayBuffer.empty[IOCell]
  // A list of functions to call in the test harness
  val harnessFunctions = ArrayBuffer.empty[TestHarnessFunction]

  // The system module specified by BuildSystem
  val lSystem = p(BuildSystem)(p).suggestName("system")

  // The provider of all clock groups. Subclasses define this
  def provideDiplomaticClocks(): Unit

  // This function generates RTL that may be necessary to drive the diplomatic clocks
  // Subclasses define this
  def driveClockGroups(systemClock: Clock, systemReset: Reset): (Seq[IOCell], Seq[TestHarnessFunction])


  // Provides the implicit clock for the lSystem
  val systemClockSinkNode = ClockSinkNode(List(ClockSinkParameters()))
  val clockGroupNode = ClockGroupIdentityNode()

  // Connect the lazy system's diplomatic clocks to ours
  lSystem match { case l: BaseSubsystem => l.asyncClockGroupsNode :*= clockGroupNode }
  systemClockSinkNode := ClockGroup()(p, ValName("chipyard_system")) := clockGroupNode

  // Now that all the clock sinks are connected to the clockGroupNode, provide the
  // clock source with this configurably-defined function
  provideDiplomaticClocks()

  lazy val module: LazyModuleImpLike = new LazyRawModuleImp(this) {
    // The system clock
    // These are given so that IOCell can use DataMirror and generate ports with
    // The right flow (Input/Output). These do not generate IOs
    val systemClock = Wire(Input(Clock()))
    val systemReset = Wire(Input(Reset()))

    // Call all of the IOBinders and provide them with a default clock and reset
    withClockAndReset(systemClock, systemReset) {
      // Call each IOBinder on both the lazyModule instance and the module
      // instance. Generally, an IOBinder PF should only be defined on one, so
      // this should not lead to two invocations.
      val (_ports, _iocells, _harnessFunctions) = p(IOBinders).values.flatMap(f => f(lSystem) ++ f(lSystem.module)).unzip3
      // We ignore _ports for now...
      iocells ++= _iocells.flatten
      harnessFunctions ++= _harnessFunctions.flatten
    }

    // Set the System clock/reset to the diplomatic clock bundle
    systemClock := systemClockSinkNode.in.head._1.clock
    systemReset := systemClockSinkNode.in.head._1.reset

    // The ModuleImp might be a LazyRawModule, in which case there is no clock
    lSystem.module match { case l: LazyModuleImp => {
      l.clock := systemClockSinkNode.in.head._1.clock
      l.reset := systemClockSinkNode.in.head._1.reset
    }}

    val (clockIOCells, clockHarnessFns) = driveClockGroups(systemClock, systemReset)
    iocells ++= clockIOCells
    harnessFunctions ++= clockHarnessFns
  }
}

/**
 * A simple clock and reset implementation that punches out clock and reset ports with the same
 * names as the implicit clock and reset for standard Module classes. Three basic reset schemes
 * are provided. See [[GlobalResetScheme]].
 */
trait HasChipTopSimpleClockAndReset { this: BaseChipTop =>
  lazy val simpleClockGroupSourceNode = ClockGroupSourceNode(Seq(ClockGroupSourceParameters()))
  lazy val simpleClockAggregator = ClockGroupAggregator()(p, ValName("chipyard_clocks"))
  def provideDiplomaticClocks(): Unit = {
    // Aggregate all the clock groups in clockGroupNode into 1, then drive it with a TestHarness-derived clock
    clockGroupNode :*=* simpleClockAggregator := simpleClockGroupSourceNode
  }

  def driveClockGroups(systemClock: Clock, systemReset: Reset): (Seq[IOCell], Seq[TestHarnessFunction]) = {
    // TODO: These should be renamed to be less confusing
    val (clock, systemClockIO) = IOCell.generateIOFromSignal(systemClock, Some("iocell_clock"))
    val (reset, systemResetIO) = p(GlobalResetSchemeKey) match {
      case GlobalResetSynchronous  =>
        IOCell.generateIOFromSignal(systemReset, Some("iocell_reset"))
      case GlobalResetAsynchronousFull =>
        IOCell.generateIOFromSignal(systemReset, Some("iocell_reset"), abstractResetAsAsync = true)
      case GlobalResetAsynchronous =>
        val asyncResetCore = Wire(Input(AsyncReset()))
        systemReset := ResetCatchAndSync(systemClock, asyncResetCore.asBool)
        IOCell.generateIOFromSignal(asyncResetCore, Some("iocell_reset"), abstractResetAsAsync = true)
    }

    clock.suggestName("clock")
    reset.suggestName("reset")

    simpleClockGroupSourceNode.out.unzip._1.flatMap(_.member).map { o =>
      o.clock := clock
      o.reset := reset
    }

    val connectClockInTestHarness = ((th: TestHarness) => {
      // Connect clock; it's not done implicitly with RawModule
      clock := th.clock
      // Connect reset; it's not done implicitly with RawModule
      // Note that we need to use dutReset, not harnessReset
      reset := th.dutReset
      Nil
    })
    (systemClockIO ++ systemResetIO, Seq(connectClockInTestHarness))
  }

}

class ChipTop()(implicit p: Parameters) extends BaseChipTop()(p)
  with HasChipTopSimpleClockAndReset
