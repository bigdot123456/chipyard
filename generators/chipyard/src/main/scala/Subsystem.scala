//******************************************************************************
// Copyright (c) 2019 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package chipyard

import chisel3._
import chisel3.internal.sourceinfo.{SourceInfo}

import freechips.rocketchip.prci._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.model.{OMInterrupt}
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{RocketTileLogicalTreeNode, LogicalModuleTree}
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.axi4._

import boom.common.{BoomTile}


import testchipip.{DromajoHelper}

class ChipyardSubsystem(implicit p: Parameters) extends BaseSubsystem
  with HasTiles
{
  def coreMonitorBundles = tiles.map {
    case r: RocketTile => r.module.core.rocketImpl.coreMonitorBundle
    case b: BoomTile => b.module.core.coreMonitorBundle
  }.toList

  // TODO: In the future, RC tiles may extend ClockDomain. When that happens,
  // we won't need to manually create this clock node and connect it to the
  // tiles' implicit clocks
  val tilesClockSinkNode = ClockSinkNode(List(ClockSinkParameters()))
  tilesClockSinkNode := ClockGroup()(p, ValName("chipyard_tiles")) := asyncClockGroupsNode
  def tilesClockBundle = tilesClockSinkNode.in.head._1

  override lazy val module = new ChipyardSubsystemModuleImp(this)
}

class ChipyardSubsystemModuleImp[+L <: ChipyardSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
  with HasResetVectorWire
  with HasTilesModuleImp
{
  for (i <- 0 until outer.tiles.size) {
    val wire = tile_inputs(i)
    wire.hartid := outer.hartIdList(i).U
    wire.reset_vector := global_reset_vector

    outer.tiles(i).module.clock := outer.tilesClockBundle.clock
    outer.tiles(i).module.reset := outer.tilesClockBundle.reset
  }

  // create file with core params
  ElaborationArtefacts.add("""core.config""", outer.tiles.map(x => x.module.toString).mkString("\n"))
  // Generate C header with relevant information for Dromajo
  // This is included in the `dromajo_params.h` header file
  DromajoHelper.addArtefacts()
}

